   
(** A snapshot of a conway board *)
type t

type row
  
(** Get the array from a row *)
val to_line : row -> string
    
(** Create an empty board. *)
val create_board : t

(** Retrieve an element. *)
val get : t -> int -> int -> int

(** Dump the board out as a string *)
val to_string : t -> string



(** Debug functions
    These won't be exposed forever *)

val calc_neighbours : t -> int -> int -> int
