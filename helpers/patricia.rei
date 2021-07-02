type key = int;
module Make:
  (Value: {
     type t;
     let key: t => key;
     let hash: t => BLAKE2B.t;
   }) =>
   {
    type t;

    let empty: t;
    let add: (key => Value.t, t) => t;
    let find: (key, t) => option((list((BLAKE2B.t, BLAKE2B.t)), Value.t));
  };
