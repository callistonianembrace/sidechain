type blake2b = bytes

let assert_msg ((message, condition): (string * bool)) =
    if not condition then
      failwith message

(* store hash *)
module Root_hash = struct
  type validator = key
  type validators = validator list

  type storage = {
    (* TODO: is having current_block_hash even useful? *)
    (* consensus proof *)
    current_block_hash: blake2b;
    current_block_height: int;
    current_state_hash: blake2b;
    current_handles_hash: blake2b;
    current_validators: validators;
  }

  type signatures = signature option list

  type action = {
    block_hash: blake2b;
    block_height: int;
    block_payload_hash: blake2b;

    state_hash: blake2b;
    handles_hash: blake2b;
    (* TODO: performance, can this blown up? *)
    validators: validators;

    signatures: signatures;
  }

    (* (pair (pair int bytes) (pair bytes validators)) *)
    (* TODO: performance, put this structures in an optimized way *)
  type block_hash_structure = {
    block_height: int;
    block_payload_hash: blake2b;
    state_hash: blake2b;
    handles_hash: blake2b;
    validators_hash: blake2b;
  }
end

let root_hash_update_root_hash
  (root_hash_update : Root_hash.action)
  (storage : Root_hash.storage) =
  let check_block_height (storage: Root_hash.storage) (block_height: int) =
    assert_msg (
      "old block height",
      block_height > storage.current_block_height
    ) in

  let check_hash (root_hash_update: Root_hash.action) =
    let block_hash_structure = {
      block_height = root_hash_update.block_height;
      block_payload_hash = root_hash_update.block_payload_hash;
      state_hash = root_hash_update.state_hash;
      handles_hash = root_hash_update.handles_hash;
      (* TODO: should we do pack of list? *)
      validators_hash = Crypto.blake2b (Bytes.pack root_hash_update.validators)
    } in
    let calculated_hash = Crypto.blake2b (Bytes.pack block_hash_structure) in
    assert_msg (
      "invalid block hash",
      root_hash_update.block_hash = calculated_hash
    ) in

  let rec check_signatures
    (validators, signatures, block_hash, remaining:
     Root_hash.validators * Root_hash.signatures * blake2b * int) : unit =
      match (validators, signatures) with
      (* already signed *)
      | ([], []) ->
        (* TODO: this can be short circuited *)
        if remaining > 0 then
          failwith "not enough signatures"
      | ((_ :: v_tl), (None :: sig_tl)) ->
        check_signatures (v_tl, sig_tl, block_hash, remaining)
      | ((validator :: v_tl), (Some signature :: sig_tl)) ->
        if Crypto.check validator signature block_hash
        then check_signatures (v_tl, sig_tl, block_hash, (remaining - 1))
        else failwith "bad signature"
      | (_, _) ->
        failwith "validators and signatures have different size" in

  let check_signatures
    (storage: Root_hash.storage)
    (signatures: Root_hash.signatures)
    (block_hash: blake2b) =
      let validators_length = (int (List.length storage.current_validators)) in
      let required_validators = (validators_length * 2) / 3 in
      check_signatures (
        storage.current_validators,
        signatures,
        block_hash,
        required_validators
      ) in
  let block_hash = root_hash_update.block_hash in
  let block_height = root_hash_update.block_height in
  let state_hash = root_hash_update.state_hash in
  let handles_hash = root_hash_update.handles_hash in
  let validators = root_hash_update.validators in
  let signatures = root_hash_update.signatures in

  let () = check_block_height storage block_height in
  let () = check_hash root_hash_update in
  let () = check_signatures storage signatures block_hash in

  {
    current_block_hash = block_hash;
    current_block_height = block_height;
    current_state_hash = state_hash;
    current_handles_hash = handles_hash;
    current_validators = validators;
  }

module Vault = struct
  type deposit = {
    ticket: bytes ticket;
    (* TODO: deposit address should be to a valid sidechain address *)
    (* TODO: this cannot be a KT1 if I understand correctly *)
    address: address
  }

  type handle = nat
  (* TODO: this could be variable in size *)
  (* TODO: name for the hashes needed *)
  type proof = {
    left: blake2b;
    right: blake2b;
  }
  type handle = nat
  type handle_hash_structure = {
    (* having the id is really important to change the hash,
      otherwise people would be able to craft handles using old proofs *)
    id: handle;
    owner: address;
    amount: nat;
    ticketer: address;
    (* TODO: probably data_hash *)
    data: bytes;
  }
  type withdraw = {
    handles_hash: blake2b;
    handle: handle_hash_structure;
    proofs: proof list;
    callback: bytes ticket contract;
  }

  type used_handle_set = (handle, unit) big_map
  (* TODO: better way than this, probably rotate it *)
  type known_handles_hash_set = (blake2b, unit) big_map
  type vault = (address * bytes, bytes ticket) big_map
  type storage = {
    known_handles_hash: known_handles_hash_set;
    used_handles: used_handle_set;
    vault: vault;
  }
end

type storage = {
  root_hash: Root_hash.storage;
  vault: Vault.storage;
}

type action =
  | Update_root_hash of Root_hash.action
  | Deposit of Vault.deposit
  | Withdraw of Vault.withdraw

(* deposit *)
(* TLDR: join tickets in vault*)
let vault_deposit (deposit: Vault.deposit) (storage: Vault.storage) =
  let { known_handles_hash; used_handles; vault } = storage in
  let (content, ticket) =  Tezos.read_ticket deposit.ticket in
  let (ticketer, (data, _)) = content in
  let (ticket, vault) =
    (* TODO: can we ignore the return of this? Like, is big_map immutable? *)
    match
      Big_map.get_and_update
        (ticketer, data)
        (None: bytes ticket option)
        vault
    with
    | (None, vault) -> (ticket, vault)
    | (Some old_ticket, vault) ->
      (match Tezos.join_tickets (old_ticket, ticket) with
      | Some ticket -> (ticket, vault)
      | None -> ((failwith "unreachable"): (bytes ticket * Vault.vault))) in
  let vault = Big_map.add (ticketer, data) ticket vault in

  {
    known_handles_hash = known_handles_hash;
    used_handles = used_handles;
    vault = vault;
  }

(* withdraw *)
(* steps:
    validate handle hash
    validate if handle id is used
    validate if the proofs match the data provided
    validate if the caller is the owner

    mark handle id as used
    split ticket in fragment and remaining
    store remaining
    send ticket fragment to callback

    TODO: ensure that sent ticket is the one with the right ticket
    *)

let vault_withdraw (withdraw: Vault.withdraw) (storage: Vault.storage) =
  let check_proof_hash (parent_hash: blake2b) (proof: Vault.proof) =
    let calculated_hash =
      Crypto.blake2b (Bytes.concat proof.left proof.right) in
    assert_msg ("invalid proof hash", parent_hash = calculated_hash) in

  let check_handle_hash
    (expected_hash: blake2b)
    (handle: Vault.handle_hash_structure) =
      let calculated_hash = Crypto.blake2b (Bytes.pack handle) in
      assert_msg ("invalid handle data", expected_hash = calculated_hash) in

  (* TODO: the signature of this function is bad *)
  let check_proof
    (proofs: Vault.proof list)
    (root: blake2b)
    (handle: Vault.handle_hash_structure) =
      let bit_is_set (bit: int) (handle: nat) =
        Bitwise.and (Bitwise.shift_left 1n (abs bit)) handle <> 0n in
      let rec verify (bit, proofs, root: int * Vault.proof list * blake2b): unit =
        match proofs with
        | [] -> check_handle_hash root handle
        | proof :: tl ->
          let () = check_proof_hash root proof in
          let root =
            if bit_is_set bit handle.id then
              proof.right
            else
              proof.left in
          verify (bit - 1, tl, root) in
      verify (int (List.length proofs) - 1, proofs, root) in
  let handles_hash = withdraw.handles_hash in
  let handle = withdraw.handle in
  let proofs = withdraw.proofs in

  let { known_handles_hash; used_handles; vault } = storage in

  let () = assert_msg (
    "unknown handles hash",
    Big_map.mem handles_hash known_handles_hash
  ) in
  let () = assert_msg (
    "already used handle",
    not Big_map.mem handle.id used_handles
  ) in
  let () = assert_msg (
    "only the owner can withdraw a handle used handle",
    handle.owner = Tezos.sender
  ) in
  let () = check_proof proofs handles_hash handle in

  (* start transfer *)
  let used_handles = Big_map.add handle.id () used_handles in

  let (fragment, vault) =
    let (old_ticket, vault) =
      match 
        Big_map.get_and_update
          (handle.ticketer, handle.data)
          (None: bytes ticket option)
          vault
      with
      | (Some old_ticket, vault) -> (old_ticket, vault)
      | (None, _) -> (failwith "unreachable" : bytes ticket * Vault.vault) in
    let ((_, (_, total)), old_ticket) = Tezos.read_ticket old_ticket in
    let (fragment, remaining) =
      match
        Tezos.split_ticket
          old_ticket
          (handle.amount, abs (total - handle.amount))
      with
      | Some (fragment, remaining) -> (fragment, remaining)
      | None -> (failwith "unreachable" : bytes ticket * bytes ticket) in
    (fragment, Big_map.add (handle.ticketer, handle.data) remaining vault) in
  let transaction = Tezos.transaction fragment 0tz withdraw.callback in
  ([transaction], {
    known_handles_hash = known_handles_hash;
    used_handles = used_handles;
    vault = vault;
  })

let vault_add_handles_hash (handles_hash: blake2b) (storage: Vault.storage) =
  let { known_handles_hash; used_handles; vault } = storage in
  let known_handles_hash = Big_map.add handles_hash () known_handles_hash in
  {
    known_handles_hash = known_handles_hash;
    used_handles = used_handles;
    vault = vault;
  }

let main (action, storage : action * storage) =
  let { root_hash; vault } = storage in
  match action with
  | Update_root_hash root_hash_update ->
    let root_hash =
      root_hash_update_root_hash
        root_hash_update
        root_hash in
    let vault =
      vault_add_handles_hash
        root_hash.current_handles_hash
        vault in
    (([] : operation list), { root_hash = root_hash; vault = vault; })
  | Deposit deposit ->
    let vault = vault_deposit deposit vault in
    (([] : operation list), { root_hash = root_hash; vault = vault; })
  | Withdraw withdraw ->
    let (operations, vault) = vault_withdraw withdraw vault in
    (operations, { root_hash = root_hash; vault = vault; })
