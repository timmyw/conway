open Core
open Board
open Curses
    
let print_row (row : Board.row) =
  print_string (to_line row)
    
let print_board win (board : Board.t) =
  let (b_x, b_y) = get_dims in
  let rows = Listutils.zero_range_as_list b_x in
  let cols = Listutils.zero_range_as_list b_y in
  List.iter rows ~f:(fun r_i ->
      List.iter cols ~f:(fun c_i ->
          let c = int_of_char (cell_to_char (get board r_i c_i)) in
          let _ =  mvwaddch win (r_i+1) (c_i+1) c in
          ()
        )
    )
  
let run_step win =
  let board = Board.create_board in
  print_board win board
  (* Out_channel.output_string stdout (sprintf "%d" (calc_neighbours board 19 19)) *)
  (* print_endline (sprintf "%d" (get board 0 0)) *)

let min x1 x2 = if x1 < x2 then x1 else x2

let () =
  let board = Board.create_board in
  let main_window = initscr ()  in
  assert (mvwaddstr main_window 0 0 "Conway's Game of Life");
  ignore(cbreak ());
  let (max_y, max_x) = getmaxyx main_window in
  let (b_y, b_x) = get_dims in
  let board_window = newwin (min (max_y - 5) (b_y+2)) (min (max_x - 5) b_x+2) 1 1 in
  (* let board_window = newwin 10 10 5 5 in *)
  (* assert (mvwaddstr board_window 2 2 (sprintf "%d x %d" b_x b_y)); *)
  box board_window 0 0;
  (* assert (refresh ()); *)
  print_board board_window board;
  assert (refresh ());
  assert(wrefresh board_window);
  Unix.sleep 2;
  endwin()
  
