    
type cell = int

type row = cell array

let cell_to_char c = 
  match c with
    | 0 -> '.'
    | 1 -> '+'
    | _ -> ' '


let to_line r = Array.fold_left (fun a c -> a ^ Char.escaped (cell_to_char c)) "" r
  
type t = row array

let board_rows =20
let board_cols = 60

let to_string board =
  let rows = Array.map to_line board in
  Array.fold_left (fun a r -> a ^ "\n" ^ r) "" rows

let cell_of_int x = x
    
let make_row trigger level = Array.init board_cols (fun _
                                                     -> let v = Random.int level in
                                                       cell_of_int (if v > trigger then 1 else 0))

let create_board =
  let _ = Random.self_init () in
  Array.init board_rows (fun _ -> make_row 7 10)

let get board row col =
  board.(row).(col)

let calc_neighbours board row col =
  let counts = [ (-1, 0); (-1, -1); (0, -1); ( 1, -1);
                 ( 1, 0); ( 1, 1);  (0, 1);  (-1, 1)] in
  let count_list = List.map (fun (x,y) ->
      if
        row + x >= 0 && row + x < board_rows &&
        col + y >= 0 && col + y < board_cols
      then
        get board (row+x) (col+y)
else 0
    ) counts in
  List.fold_left (fun a c -> a + c) 0 count_list
