open Core
open Board

let print_row (row : Board.row) =
  print_string (to_line row)
    
let print_board (board : Board.t) =
  print_string (Board.to_string board);
    print_string "\n"
    
let run_step =
  let board = Board.empty in
  print_board board
  (* print_endline (sprintf "%d" (get board 0 0)) *)
