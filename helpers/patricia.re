[@deriving yojson]
type key = int;

module Make =
       (Value: {
          type t;
          let key: t => key;
          let hash: t => BLAKE2B.t;
        })
       : {
         type t;

         let empty: t;
         let add: (key => Value.t, t) => t;
         let find:
           (key, t) => option((list((BLAKE2B.t, BLAKE2B.t)), Value.t));
       } => {
  type tree =
    | Empty
    | Leaf({
        value: Value.t,
        hash: BLAKE2B.t,
      })
    | Node({
        left: tree,
        hash: BLAKE2B.t,
        right: tree,
      });
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
    | Leaf({value, _}) when key == Value.key(value) =>
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
      | ((-1), Empty) => Leaf({value, hash: Value.hash(value)})
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
    {tree, top_bit, last_key: key};
  };
  let empty = {top_bit: 0, tree: Empty, last_key: (-1)};
};
