   
(** A snapshot of a conway board *)
type t

type row
  
(** Get the array from a row *)
val to_line : row -> string
    
(** Create an empty board. *)
val empty : t

(** Retrieve an element. *)
val get : t -> int -> int -> int

(** Dump the board out as a string *)
val to_string : t -> string
