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

  let check_block_height (storage: storage) (block_height: int) =
    assert_msg (
      "old block height",
      block_height > storage.current_block_height
    )

  let check_hash (root_hash_update: action) =
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
    )

  let rec check_signatures
    (validators, signatures, block_hash, remaining:
     validators * signatures * blake2b * int) : unit =
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
        failwith "validators and signatures have different size"

  let check_signatures
    (storage: storage)
    (signatures: signatures)
    (block_hash: blake2b) =
      let validators_length = (int (List.length storage.current_validators)) in
      let required_validators = (validators_length * 2) / 3 in
      check_signatures (
        storage.current_validators,
        signatures,
        block_hash,
        required_validators
      )

  let update_root_hash (root_hash_update : action) (storage : storage) =
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
end

module Vault = struct
  type deposit = {
    ticket: bytes ticket;
    (* TODO: deposit address should be to a valid sidechain address *)
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
    proofs: proof list
  }

  type used_handle_set = (handle, unit) big_map
  (* TODO: better way than this *)
  type known_handles_hash_set = (blake2b, unit) big_map
  type vault = (address * bytes, bytes ticket) big_map
  type storage = {
    known_handles_hash: known_handles_hash_set;
    used_handles: used_handle_set;
    vault: vault;
  }

  (* deposit *)
  let deposit (deposit: deposit) (storage: storage) =
    let vault = storage.vault in
    let (content, ticket) =  Tezos.read_ticket deposit.ticket in
    let (ticketer, (data, _)) = content in
    let (vault, ticket) =
      (* TODO: can we ignore the return of this? Like, is big_map immutable? *)
      match
        Big_map.get_and_update
          (ticketer, data)
          (None: bytes ticket option)
          storage.vault
      with
      | (None, vault) -> (vault, ticket)
      | (Some old_ticket, vault) ->
        (match Tezos.join_tickets (old_ticket, ticket) with
        | Some ticket -> (vault, ticket)
        | None -> ((failwith "unreachable"): (vault * bytes ticket))) in
    let vault = Big_map.add (ticketer, data) ticket vault in

    let known_handles_hash = storage.known_handles_hash in
    let used_handles = storage.used_handles in
    {
      known_handles_hash = known_handles_hash;
      vault = vault;
      used_handles = used_handles;
    }

  (* withdraw *)
  (* steps:
      validate if handle hash
      validate if handle is taken
      validate if the proofs match the data provided

      addd handles as used
      transfer money(how)
      *)
  let check_handles_hash (storage: storage) (handles_hash: blake2b) =
    assert_msg (
      "unknown handles hash",
      Big_map.mem handles_hash storage.known_handles_hash
    )
  let check_handle_available (storage: storage) (handle_id: handle) =
    if Big_map.mem handle_id storage.used_handles then
      failwith "already used handle"

  let check_proof_hash (parent_hash: blake2b) (proof: proof) =
    let calculated_hash =
      Crypto.blake2b (Bytes.concat proof.left proof.right) in
    assert_msg ("invalid proof hash", parent_hash = calculated_hash)

  let check_data_hash (data_hash: blake2b) (data: handle_hash_structure) = 
    let calculated_hash = Crypto.blake2b (Bytes.pack data) in
    assert_msg ("invalid handle data", data_hash = calculated_hash)

  (* TODO: the signature of this function is bad *)
  let rec check_proof
    (proofs, root, handle, data:
     proof list * blake2b * handle * handle_hash_structure): unit =
      match proofs with
      | [] -> check_data_hash root data
      | proof :: tl ->
        let () = check_proof_hash root proof in
        let root =
          if (Bitwise.and handle 1n) = 0n then
            proof.left
          else
            proof.right in
        let handle = Bitwise.shift_right handle 1n in
        check_proof (tl, root, handle, data)

  let withdraw (withdraw: withdraw) (storage: storage) =
    let handles_hash = withdraw.handles_hash in
    let handle = withdraw.handle in
    let proofs = withdraw.proofs in

    let () = check_handles_hash storage handles_hash in
    let () = check_handle_available storage handle.id in
    let () = check_proof (proofs, handles_hash, handle.id, handle) in

    let known_handles_hash = storage.known_handles_hash in 
    let used_handles = Big_map.add handle.id () storage.used_handles in
    (* TODO: transfer money *)
    let vault = storage.vault in
    {
      known_handles_hash = known_handles_hash;
      vault = vault;
      used_handles = used_handles;
    }

  let add_handles_hash (handles_hash: blake2b) (storage: storage) =
    let known_handles_hash = storage.known_handles_hash in
    let vault = storage.vault in 
    let used_handles = storage.used_handles in 

    let known_handles_hash = Big_map.add handles_hash () known_handles_hash in
    {
      known_handles_hash = known_handles_hash;
      vault = vault;
      used_handles = used_handles;
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

let main (action, storage : action * storage) =
  let root_hash = storage.root_hash in
  let vault = storage.vault in
  match action with
  | Update_root_hash root_hash_update ->
    let root_hash =
      Root_hash.update_root_hash
        root_hash_update
        root_hash in
    let vault =
      Vault.add_handles_hash
        root_hash.current_handles_hash
        vault in
    (([] : operation list), { root_hash = root_hash; vault = vault; })
  | Deposit deposit ->
    let root_hash = storage.root_hash in
    let vault = Vault.deposit deposit storage.vault in
    (([] : operation list), { root_hash = root_hash; vault = vault; })
  | Withdraw withdraw ->
    let root_hash = storage.root_hash in
    let vault = Vault.withdraw withdraw storage.vault in
    (([] : operation list), { root_hash = root_hash; vault = vault; })
