open Helpers;

type main_chain_operation_kind =
  pri
    | Deposit({
        destination: Wallet.t,
        amount: Amount.t,
        ticket: Ledger.Ticket.t,
      });

type main_chain_operation =
  pri {
    hash: BLAKE2B.t,
    signature: Signature.t,
    tezos_hash: BLAKE2B.t,
    kind: main_chain_operation_kind,
  };

type side_chain_operation_kind =
  pri
    | Transaction({destination: Wallet.t})
    | Burn({owner: Tezos_interop.Key_hash.t});

type side_chain_operation =
  pri {
    hash: BLAKE2B.t,
    signature: Signature.t,
    max_block_height: int64,
    source: Wallet.t,
    amount: Amount.t,
    ticket: Ledger.Ticket.t,
    kind: side_chain_operation_kind,
  };

[@deriving (ord, yojson)]
type t =
  | Main(main_chain_operation)
  | Side(side_chain_operation);

let sign_main:
  // TODO: must be signed by the node key
  //       so better API to this
  (
    ~secret: Address.key,
    ~tezos_hash: BLAKE2B.t,
    ~kind: main_chain_operation_kind
  ) =>
  main_chain_operation;
let verify_main:
  (
    ~hash: BLAKE2B.t,
    // TODO: must be signed by the node key
    //       so better API to this
    ~signature: Signature.t,
    ~tezos_hash: BLAKE2B.t,
    ~kind: main_chain_operation_kind
  ) =>
  result(main_chain_operation, [> | `Invalid_hash | `Invalid_signature]);
let sign_side:
  (
    ~secret: Address.key,
    ~max_block_height: int64,
    ~source: Wallet.t,
    ~amount: Amount.t,
    ~ticket: Ledger.Ticket.t,
    ~kind: side_chain_operation_kind
  ) =>
  side_chain_operation;
let verify_side:
  (
    ~hash: BLAKE2B.t,
    ~signature: Signature.t,
    ~max_block_height: int64,
    ~source: Wallet.t,
    ~amount: Amount.t,
    ~ticket: Ledger.Ticket.t,
    ~kind: side_chain_operation_kind
  ) =>
  result(side_chain_operation, [> | `Invalid_hash | `Invalid_signature]);
