   
(** A snapshot of a conway board *)
type t

type row
  
(** Get the array from a row *)
val to_line : row -> string

(** Get board dimensions *)
val get_dims : int * int

(** Create an empty board. *)
val create_board : t

(** Retrieve an element. *)
val get : t -> int -> int -> int

(** Convert cell value to char *)
val cell_to_char : int -> char

(** Dump the board out as a string *)
val to_string : t -> string

(** Run an iteration through the supplied board, returned a new board
    representing the new state *)
val iterate : t -> t

(** Debug functions
    These won't be exposed forever *)

val calc_neighbours : t -> int -> int -> int
