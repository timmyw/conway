
type t

val init_screen : string -> t

val shutdown_screen: unit -> unit

val print_board: t -> int -> Board.t -> unit
