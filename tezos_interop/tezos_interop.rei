open Helpers;

module Base58 = Base58;

module Key: {
  type t =
    | Ed25519(Mirage_crypto_ec.Ed25519.pub_);

  let encoding: Data_encoding.t(t);
  let to_string: t => string;
  let of_string: string => option(t);
};

module Key_hash: {
  type t =
    | Ed25519(Helpers.BLAKE2B_20.t);

  let of_key: Key.t => t;
  let to_string: t => string;
  let of_string: string => option(t);
  let to_yojson: t => Yojson.Safe.t;
  let of_yojson: Yojson.Safe.t => result(t, string);
};

module Secret: {
  type t =
    | Ed25519(Mirage_crypto_ec.Ed25519.priv);

  let to_string: t => string;
  let of_string: string => option(t);
};

module Signature: {
  type t = pri | Ed25519(string);

  let sign: (Secret.t, string) => t;
  let check: (Key.t, t, string) => bool;

  let to_string: t => string;
  let of_string: string => option(t);
};

module Pack: {
  type t;

  let int: Z.t => t;
  let bytes: bytes => t;
  let pair: (t, t) => t;
  let list: list(t) => t;
  let key: Key.t => t;

  let to_bytes: t => bytes;
};

module Consensus: {
  let hash_validators: list(Key.t) => BLAKE2B.t;
  let hash_block:
    (
      ~block_height: int64,
      ~block_payload_hash: BLAKE2B.t,
      ~state_root_hash: BLAKE2B.t,
      ~validators_hash: BLAKE2B.t
    ) =>
    BLAKE2B.t;
};

module Discovery: {let sign: (Secret.t, ~nonce: int64, Uri.t) => Signature.t;};
