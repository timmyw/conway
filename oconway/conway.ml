open Core

(* open Board *)
open Ui

(* let print_row (row : Board.row) = *)
(*   print_string (to_line row) *)
    
  
(* let run_step win = *)
(*   let board = Board.create_board in *)
(*   print_board win board *)
  (* Out_channel.output_string stdout (sprintf "%d" (calc_neighbours board 19 19)) *)
  (* print_endline (sprintf "%d" (get board 0 0)) *)

let min x1 x2 = if x1 < x2 then x1 else x2

let () =
  let board = Board.create_board in
  let board_window = init_screen "Conway's Game of Life" in
  print_board board_window 1 board;
  Unix.sleep 2;
  shutdown_screen ()
