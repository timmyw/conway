open Core
    
type cell = int

type row = cell array

let cell_to_char c = 
  match c with
    | 0 -> ' '
    | 1 -> '+'
    | _ -> ' '


let to_line r = Array.fold r ~init:"" ~f:(fun a c -> a ^ Char.escaped (cell_to_char c))
  
type t = row array

let board_rows =20
let board_cols = 60

let get_dims = board_rows, board_cols

let to_string board =
  let rows = Array.map board ~f:to_line in
  Array.fold ~f:(fun a r -> a ^ "\n" ^ r) ~init:"" rows

let cell_of_int x = x

(* let make_empty_row () = Array.init board_cols ~f:(fun _ -> 0) *)
    
let make_row trigger level = Array.init board_cols ~f:(fun _
                                                     -> let v = Random.int level in
                                                       cell_of_int (if v > trigger then 1 else 0))

let create_board =
  let _ = Random.self_init () in
  Array.init board_rows ~f:(fun _ -> make_row 7 10)

let empty =
  Array.init board_rows ~f:(fun _ -> Array.init board_cols ~f:(fun _ -> 0))
    
let get board row col =
  board.(row).(col)

let set board row col v =
  board.(row).(col) <- v
  
let calc_neighbours board row col =
  let counts = [ (-1, 0); (-1, -1); (0, -1); ( 1, -1);
                 ( 1, 0); ( 1, 1);  (0, 1);  (-1, 1)] in
  let count_list = List.map ~f:(fun (x,y) ->
      if
        row + x >= 0 && row + x < board_rows &&
        col + y >= 0 && col + y < board_cols
      then get board (row+x) (col+y) else 0
    ) counts in
  List.fold_left ~f:(fun a c -> a + c) ~init:0 count_list

let apply_rules board x y =
  let neighbour_count = calc_neighbours board x y in
  if neighbour_count < 2 then 0
  else if neighbour_count > 3 then 0
  else 1

let iterate board =
  let new_board = empty in
  let rows = Listutils.range_as_list board_rows in
  let cols = Listutils.range_as_list board_cols in
  List.iter rows ~f:(fun r_i ->
      List.iter cols ~f:(fun c_i ->
          set new_board r_i c_i (apply_rules board r_i c_i)
        )
    );
   new_board
