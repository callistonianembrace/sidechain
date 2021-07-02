open Setup;
open Helpers;

type data = {
  key: int,
  hash: BLAKE2B.t,
};
open Patricia.Make({
       type t = data;
       let key = t => t.key;
       let hash = t => t.hash;
     });

describe("patricia", ({test, _}) => {
  test("add and find", ({expect, _}) => {
    let expected =
      List.init(1234, n => (n, BLAKE2B.hash(Printf.sprintf("%d", n))));
    let tree =
      List.fold_left(
        (tree, (_, hash)) => add(key => {key, hash}, tree),
        empty,
        expected,
      );
    expected
    |> List.iter(((key, expected_hash)) => {
         // TODO: test the proofs
         let (_proofs, value) = find(key, tree) |> Option.get;
         expect.equal({key, hash: expected_hash}, value);
       });
  })
});
