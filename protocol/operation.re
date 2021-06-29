open Helpers;

module Ticket_id = {
  [@deriving yojson]
  type t = {
    // TODO: KT1
    ticketer: string,
    data_hash: BLAKE2B.t,
  };
};

[@deriving yojson]
type main_chain_operation_kind =
  | Deposit({
      destination: Wallet.t,
      amount: Amount.t,
      ticket: Ticket_id.t,
    });

[@deriving yojson]
type main_chain_operation = {
  hash: BLAKE2B.t,
  signature: Signature.t,
  tezos_hash: BLAKE2B.t,
  kind: main_chain_operation_kind,
};
let compare_main_chain_operation = (a, b) => BLAKE2B.compare(a.hash, b.hash);

[@deriving yojson]
type side_chain_operation_kind =
  | Transaction({destination: Wallet.t})
  | Burn;

[@deriving yojson]
type side_chain_operation = {
  // header
  hash: BLAKE2B.t,
  signature: Signature.t,
  // body
  max_block_height: int64,
  source: Wallet.t,
  amount: Amount.t,
  ticket: Ticket_id.t,
  kind: side_chain_operation_kind,
};
let compare_side_chain_operation = (a, b) => BLAKE2B.compare(a.hash, b.hash);

[@deriving (ord, yojson)]
type t =
  | Main(main_chain_operation)
  | Side(side_chain_operation);

// main
let (hash, verify) = {
  /* TODO: this is bad name, it exists like this to prevent
     duplicating all this name parameters */
  let apply = (f, ~tezos_hash, ~kind) => {
    let to_yojson = [%to_yojson: (BLAKE2B.t, main_chain_operation_kind)];
    let json = to_yojson((tezos_hash, kind));
    let payload = Yojson.Safe.to_string(json);
    f(payload);
  };
  let hash = apply(BLAKE2B.hash);
  let verify = (~hash) => apply(BLAKE2B.verify(~hash));
  (hash, verify);
};

let sign_main = (~secret, ~tezos_hash, ~kind) => {
  let hash = hash(~tezos_hash, ~kind);
  let signature = Signature.sign(~key=secret, hash);
  {hash, signature, tezos_hash, kind};
};

let verify_main = (~hash, ~signature, ~tezos_hash, ~kind) => {
  let.ok () =
    verify(~hash, ~tezos_hash, ~kind) ? Ok() : Error(`Invalid_hash);
  let.ok () =
    Signature.verify(~signature, hash) ? Ok() : Error(`Invalid_signature);
  Ok({hash, tezos_hash, kind, signature});
};

// side
let (hash, verify) = {
  /* TODO: this is bad name, it exists like this to prevent
     duplicating all this name parameters */
  let apply = (f, ~max_block_height, ~source, ~amount, ~ticket, ~kind) => {
    let to_yojson = [%to_yojson:
      (int64, Wallet.t, Amount.t, Ticket_id.t, side_chain_operation_kind)
    ];
    let json = to_yojson((max_block_height, source, amount, ticket, kind));
    let payload = Yojson.Safe.to_string(json);
    f(payload);
  };
  let hash = apply(BLAKE2B.hash);
  let verify = (~hash) => apply(BLAKE2B.verify(~hash));
  (hash, verify);
};

let sign_side = (~secret, ~max_block_height, ~source, ~amount, ~ticket, ~kind) => {
  let hash = hash(~max_block_height, ~source, ~amount, ~ticket, ~kind);
  let signature = Signature.sign(~key=secret, hash);
  {hash, signature, max_block_height, source, amount, ticket, kind};
};
let verify_side =
    (~hash, ~signature, ~max_block_height, ~source, ~amount, ~ticket, ~kind) => {
  let.ok () =
    verify(~hash, ~max_block_height, ~source, ~amount, ~ticket, ~kind)
      ? Ok() : Error(`Invalid_hash);
  let.ok () =
    Signature.verify(~signature, hash) ? Ok() : Error(`Invalid_signature);
  Ok({hash, signature, max_block_height, source, amount, ticket, kind});
};
