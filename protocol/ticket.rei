// TODO: do it in a different way
[@deriving (ord, yojson)]
type t =
  Tezos_interop.Ticket.t = {
    ticketer: Tezos_interop.Address.t,
    data: bytes,
  };
