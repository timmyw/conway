open Core

open Curses
open Board

type t = Curses.window

let init_screen title =
  let main_window = initscr ()  in
  assert (mvwaddstr main_window 0 0 title);
  ignore(cbreak ());
  let (max_y, max_x) = getmaxyx main_window in
  let (b_y, b_x) = get_dims in
  let board_window = newwin (min (max_y - 5) (b_y+2)) (min (max_x - 5) b_x+2) 1 1 in
  assert (curs_set 0);
  box board_window 0 0;
  board_window

let display_iteration win iteration =
  let its = sprintf "Iteration %d" iteration in
  let (_, mx) = getmaxyx win in
  let x = (mx - String.length its - 2) in
  assert(mvwaddstr win 0 x its)

let print_board win iteration (board : Board.t) =
  let (b_x, b_y) = get_dims in
  let rows = Listutils.zero_range_as_list b_x in
  let cols = Listutils.zero_range_as_list b_y in
  List.iter rows ~f:(fun r_i ->
      List.iter cols ~f:(fun c_i ->
          let c = int_of_char (cell_to_char (get board r_i c_i)) in
          let _ =  mvwaddch win (r_i+1) (c_i+1) c in
          ()
        )
    );
  display_iteration win iteration;
  assert (refresh ());
  assert(wrefresh win)


let shutdown_screen () =
  assert(curs_set 1);
  endwin()

  (* let board_window = newwin 10 10 5 5 in *)
  (* assert (mvwaddstr board_window 2 2 (sprintf "%d x %d" b_x b_y)); *)
  (* assert (refresh ()); *)
