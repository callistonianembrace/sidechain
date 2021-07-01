open Helpers;
include Exn_noop;

module Signed = Signed;
module Signature = Signature;
module Address = Address;
module Wallet = Wallet;
module Ledger = Ledger;
module Validators = Validators;
module Amount = Amount;
module Block = Block;
module Old_operation = Old_operation;

include State;

let apply_main_op = (state, main_op: Operation.main_chain_operation) =>
  // TODO: should check if tezos_hash actually exists?
  switch (main_op.kind) {
  | Deposit({destination, amount, ticket}) =>
    let ledger = Ledger.deposit(~destination, ticket, amount, state.ledger);
    {...state, ledger};
  };
let apply_side_op = (state, side_op) => {
  open Operation;
  if (side_op.max_block_height < state.block_height) {
    raise(Noop("really old operation"));
  };

  let {source, amount, ticket, _} = side_op;

  switch (side_op.kind) {
  | Transaction({destination}) =>
    let ledger =
      switch (
        Ledger.transfer(~source, ~destination, ticket, amount, state.ledger)
      ) {
      | Ok(ledger) => ledger
      | Error(`Not_enough_funds) => raise(Noop("not enough funds"))
      };

    {...state, ledger};
  | Burn({owner}) =>
    // TODO: do something with the handle
    let (ledger, _) =
      switch (Ledger.burn(~source, ~owner, ticket, amount, state.ledger)) {
      | Ok(ledger) => ledger
      | Error(`Not_enough_funds) => raise(Noop("not enough funds"))
      };
    {...state, ledger};
  };
};
let apply_op = (state, op: Operation.t) => {
  module Set = Operation_side_chain_set;
  if (Set.mem(op, state.included_operations)) {
    raise(Noop("duplicated operation"));
  };

  let state =
    switch (op) {
    | Main(main_op) => apply_main_op(state, main_op)
    | Side(side_op) => apply_side_op(state, side_op)
    };

  let state = {
    // TODO: if apply_main_op or apply_side_op raises, this never happens
    let included_operations = Set.add(op, state.included_operations);
    {...state, included_operations};
  };
  state;
};

let is_next = (state, block) =>
  Int64.add(state.block_height, 1L) == block.Block.block_height
  && state.last_block_hash == block.previous_hash;

let apply_block = (state, block) => {
  Printf.printf("%Ld\n%!", block.Block.block_height);
  let fold_left_noop_when_exception = (f, state, list) =>
    List.fold_left(
      (state, op) =>
        try(f(state, op)) {
        | Noop(_string) =>
          // TODO: print exception
          Printexc.print_backtrace(stdout);
          state;
        },
      state,
      list,
    );
  let state =
    fold_left_noop_when_exception(apply_op, state, block.Block.operations);

  // TODO: move to function trim state
  let state = {
    ...state,
    included_operations:
      state.included_operations
      |> Operation_side_chain_set.filter(
           fun
           | Main(_main) =>
             // TODO: should we ever filter main chain ops?
             true
           | Side(side) => side.max_block_height < state.block_height,
         ),
  };

  {
    ...state,
    block_height: block.block_height,
    validators: state.validators |> Validators.update_current(block.author),
    last_block_hash: block.hash,
    last_state_root_update:
      block.state_root_hash != state.state_root_hash
        ? Unix.time() : state.last_state_root_update,
    last_applied_block_timestamp: Unix.time(),
    state_root_hash: block.state_root_hash,
    validators_hash: block.validators_hash,
  };
};

let make = (~initial_block) => {
  let empty = {
    ledger: Ledger.empty,
    included_operations: Operation_side_chain_set.empty,
    validators: Validators.empty,
    validators_hash: Validators.hash(Validators.empty),
    block_height: Int64.sub(initial_block.Block.block_height, 1L),
    /* because this calls apply_block internally
       it requires some care to ensure all fields
       are in the right place, otherwise invariants
       can be broken */
    last_block_hash: initial_block.Block.previous_hash,
    state_root_hash: initial_block.Block.state_root_hash,
    last_state_root_update: 0.0,
    last_applied_block_timestamp: 0.0,
  };
  apply_block(empty, initial_block);
};
let apply_block = (state, block) => {
  let.assert () = (`Invalid_block_when_applying, is_next(state, block));
  let (valid_hash, hash) =
    if (block.state_root_hash == state.state_root_hash) {
      (true, None);
    } else {
      // TODO: pipeline this
      let (hash, data) = hash(state);
      (block.state_root_hash == hash, Some((hash, data)));
    };
  let.assert () = (`Invalid_state_root_hash, valid_hash);
  let state = apply_block(state, block);
  Ok((state, hash));
};

let get_current_block_producer = state =>
  if (state.last_applied_block_timestamp == 0.0) {
    None;
  } else {
    let diff = Unix.time() -. state.last_applied_block_timestamp;
    // TODO: I'm really into magic numbers
    let skips = Float.to_int(diff /. 10.0);
    Validators.after_current(skips, state.validators);
  };