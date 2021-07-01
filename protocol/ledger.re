open Helpers;

module Ticket = {
  [@deriving (yojson, ord)]
  type t = {
    // TODO: KT1
    ticketer: string,
    data_hash: BLAKE2B.t,
  };
};

let assert_available = (~balance, ~amount: Amount.t) =>
  balance >= amount ? Ok() : Error(`Not_enough_funds);

module Ledger: {
  [@deriving yojson]
  type t;

  let empty: t;
  let balance: (Wallet.t, Ticket.t, t) => Amount.t;
  let transfer:
    (~source: Wallet.t, ~destination: Wallet.t, Ticket.t, Amount.t, t) =>
    result(t, [> | `Not_enough_funds]);
  let deposit: (~destination: Wallet.t, Ticket.t, Amount.t, t) => t;
  let burn:
    (Wallet.t, Ticket.t, Amount.t, t) => result(t, [> | `Not_enough_funds]);
} = {
  [@deriving (ord, yojson)]
  type key = {
    wallet: Wallet.t,
    ticket: Ticket.t,
  };
  // TODO: fusing this has advantages in performance, but we loose enumerability
  //       do we actually loose it? Can't we find just by having half the key?
  module Wallet_and_ticket_map =
    Map_with_yojson_make({
      [@deriving (ord, yojson)]
      type t = key;
    });

  [@deriving yojson]
  type t = Wallet_and_ticket_map.t(Amount.t);

  let empty = Wallet_and_ticket_map.empty;

  let balance = (key, t) => {
    let.default () = Amount.zero;
    Wallet_and_ticket_map.find_opt(key, t);
  };
  let transfer = (~source, ~destination, amount, t) => {
    open Amount;

    let source_balance = balance(source, t);
    let.ok () = assert_available(~balance=source_balance, ~amount);

    let destination_balance = balance(destination, t);

    let t =
      t
      |> Wallet_and_ticket_map.add(source, source_balance - amount)
      |> Wallet_and_ticket_map.add(destination, destination_balance + amount);
    Ok(t);
  };
  let deposit = (~destination, amount, t) => {
    open Amount;
    let balance = balance(destination, t);
    t |> Wallet_and_ticket_map.add(destination, balance + amount);
  };
  let burn = (key, amount, t) => {
    open Amount;
    let balance = balance(key, t);
    let.ok () = assert_available(~balance, ~amount);
    let t = t |> Wallet_and_ticket_map.add(key, balance - amount);
    Ok(t);
  };

  let balance = (wallet, ticket, t) => balance({wallet, ticket}, t);
  let transfer = (~source, ~destination, ticket, amount, t) =>
    transfer(
      ~source={wallet: source, ticket},
      ~destination={wallet: destination, ticket},
      amount,
      t,
    );
  let deposit = (~destination, ticket, amount, t) =>
    deposit(~destination={wallet: destination, ticket}, amount, t);
  let burn = (wallet, ticket, amount, t) =>
    burn({wallet, ticket}, amount, t);
};

module Handles: {
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
  let next:
    (~owner: Tezos_interop.Key_hash.t, Ticket.t, Amount.t, t) => (t, Handle.t);
} = {
  module Handle = {
    [@deriving yojson]
    type t = {
      id: int64,
      owner: Tezos_interop.Key_hash.t,
      ticket: Ticket.t,
      amount: Amount.t,
    };
    let owner = t => t.owner;
    let ticket = t => t.ticket;
    let amount = t => t.amount;
  };
  open Handle;

  module Int64_map =
    Map_with_yojson_make({
      [@deriving (ord, yojson)]
      type t = int64;
    });

  [@deriving yojson]
  type t = {
    last_id: int64,
    map: Int64_map.t(Handle.t),
  };

  let empty = {last_id: 0L, map: Int64_map.empty};

  let next = (~owner, ticket, amount, t) => {
    let id = Int64.add(t.last_id, 1L);
    let handle = {id, ticket, owner, amount};
    let t = {last_id: id, map: Int64_map.add(id, handle, t.map)};
    (t, handle);
  };
};

module Handle = Handles.Handle;

[@deriving yojson]
type t = {
  ledger: Ledger.t,
  handles: Handles.t,
};

let empty = {ledger: Ledger.empty, handles: Handles.empty};

let balance = (ticket, amount, t) =>
  Ledger.balance(ticket, amount, t.ledger);
let transfer = (~source, ~destination, ticket, amount, t) => {
  let.ok ledger =
    Ledger.transfer(~source, ~destination, ticket, amount, t.ledger);
  Ok({ledger, handles: t.handles});
};
let deposit = (~destination, ticket, amount, t) => {
  let ledger = Ledger.deposit(~destination, ticket, amount, t.ledger);
  {ledger, handles: t.handles};
};

let burn = (~source, ~owner, ticket, amount, t) => {
  let.ok ledger = Ledger.burn(source, ticket, amount, t.ledger);
  let (handles, handle) = Handles.next(~owner, ticket, amount, t.handles);
  Ok(({ledger, handles}, handle));
};
