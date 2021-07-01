module Ticket: {
  [@deriving yojson]
  type t;
};
module Handle: {
  [@deriving yojson]
  type t;

  let owner: t => Tezos_interop.Key_hash.t;
  let ticket: t => Ticket.t;
  let amount: t => Amount.t;
};

[@deriving yojson]
type t;
let empty: t;

let balance: (Wallet.t, Ticket.t, t) => Amount.t;
let transfer:
  (~source: Wallet.t, ~destination: Wallet.t, Ticket.t, Amount.t, t) =>
  result(t, [> | `Not_enough_funds]);

// tezos operations
let deposit: (~destination: Wallet.t, Ticket.t, Amount.t, t) => t;
let burn:
  (
    ~source: Wallet.t,
    ~owner: Tezos_interop.Key_hash.t,
    Ticket.t,
    Amount.t,
    t
  ) =>
  result((t, Handle.t), [> | `Not_enough_funds]);
