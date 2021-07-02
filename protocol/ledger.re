open Helpers;

module Ticket = {
  [@deriving (yojson, ord)]
  type t = {
    // TODO: KT1
    ticketer: string,
    data: bytes,
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
  [@deriving yojson]
  type key = int;

  module Handle = {
    [@deriving yojson]
    type t = {
      id: int,
      hash: BLAKE2B.t,
      owner: Tezos_interop.Key_hash.t,
      ticket: Ticket.t,
      amount: Amount.t,
    };
    let owner = t => t.owner;
    let ticket = t => t.ticket;
    let amount = t => t.amount;
  };

  module Tree: {
    [@deriving yojson]
    type t;

    let empty: t;
    let add: (key => Handle.t, t) => (t, Handle.t);
    [@warning "-32"]
    let find: (key, t) => option((list((BLAKE2B.t, BLAKE2B.t)), Handle.t));
  } = {
    [@deriving yojson]
    type tree =
      | Empty
      | Leaf({
          value: Handle.t,
          hash: BLAKE2B.t,
        })
      | Node({
          left: tree,
          hash: BLAKE2B.t,
          right: tree,
        });
    [@deriving yojson]
    type t = {
      tree,
      top_bit: int,
      last_key: int,
    };

    let is_set = (bit, number) => 1 lsl bit land number != 0;

    let hash_of_t =
      fun
      | Empty => BLAKE2B.hash("")
      | Leaf({hash, _})
      | Node({hash, _}) => hash;
    let rec find = (bit, proofs, key, t) =>
      switch (t) {
      | Empty => None
      | Leaf({value, _}) when key == value.id =>
        Some((List.rev(proofs), value))
      | Leaf(_) => None
      | Node({left, right, _}) =>
        let t = is_set(bit, key) ? right : left;
        find(
          bit - 1,
          [(hash_of_t(left), hash_of_t(right)), ...proofs],
          key,
          t,
        );
      };

    let find = (key, t) => find(t.top_bit - 1, [], key, t.tree);

    let node = (left, right) => {
      let hash =
        BLAKE2B.hash(
          BLAKE2B.to_raw_string(hash_of_t(left))
          ++ BLAKE2B.to_raw_string(hash_of_t(right)),
        );
      Node({left, hash, right});
    };

    let rec empty = n =>
      if (n == 0) {
        Empty;
      } else {
        let tree = empty(n - 1);
        node(tree, tree);
      };

    let add = (f, t) => {
      let rec add = (bit, key, value, t) =>
        switch (bit, t) {
        | ((-1), Empty) => Leaf({value, hash: value.hash})
        | (_, Node({left, right, _})) =>
          is_set(bit, key)
            ? node(left, add(bit - 1, key, value, right))
            : node(add(bit - 1, key, value, left), right)
        | _ => assert(false)
        };

      let key = t.last_key + 1;
      let value = f(key);
      let increase_top_bit = key lsr t.top_bit;
      let top_bit = t.top_bit + increase_top_bit;
      let tree =
        if (increase_top_bit == 1) {
          let right = empty(top_bit - 1);
          node(t.tree, right);
        } else {
          t.tree;
        };

      let tree = add(top_bit - 1, key, value, tree);
      ({tree, top_bit, last_key: key}, value);
    };
    let empty = {top_bit: 0, tree: Empty, last_key: (-1)};
  };
  include Tree;

  open Handle;

  let next = (~owner, ticket, amount) =>
    add(id => {
      // TODO: proper hash
      let hash = BLAKE2B.hash(Printf.sprintf("%d", id));
      {id, hash, owner, ticket, amount};
    });
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
