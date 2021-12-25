open Prelude

type state = Marked of int | Unmarked of int
let marked x = Marked x
let unmarked x = Unmarked x

let is_marked = function Marked _ -> true | Unmarked _ -> false 
let is_unmarked = Fun.negate is_marked
let scalar = function Marked x | Unmarked x -> x

let rec transpose = function
  | [] -> [] 
  | [xs] -> xs |> List.map singleton
  | xs :: xss -> transpose xss |> List.combine xs |> List.map (fun (x, xs) -> x :: xs)

let mark move = List.map (List.map (function Unmarked x when x = move -> Marked x | any -> any))

let has_won matrix = 
  matrix |> List.exists (List.for_all is_marked) || 
  matrix |> transpose |> List.exists (List.for_all is_marked)

let get_winner boards = boards |> List.find_opt has_won

let part1 moves boards = 
  let rec loop moves boards = 
    match moves with 
    | move :: moves ->
      let boards = boards |> List.map (mark move) in
      (match get_winner boards with  
      | Some winner -> move, winner 
      | None -> loop moves boards)
    | [] -> failwith "moves finished before a winner was found." 
  in
  let move, winner = loop moves boards in
  let filter_unmarked  = List.concat_map (List.filter is_unmarked) in
  let result = winner |> filter_unmarked |> List.map scalar |> sum in 
  result * move 

let part2 moves boards =
  let rec loop len moves boards = 
    match moves with 
    | move :: moves ->
      let boards = boards |> List.map (mark move) in
      (match get_winner boards, len with  
      | Some winner, 1 -> move, winner 
      | Some _winner, _ -> 
        let boards = boards |> List.filter (has_won >> not) in
        loop (List.length boards) moves boards
      | None, _ -> loop len moves boards)
    | [] -> failwith "moves finished before a winner was found." 
  in 
  let len = List.length boards in
  let move, winner = loop len moves boards in
  let filter_unmarked  = List.concat_map (List.filter is_unmarked) in
  let result = winner |> filter_unmarked |> List.map scalar |> sum in
  result * move

let () =  
  let file = "./input/day4.txt" in
  let split_blank_line = "[ \n]+\n" |> Str.regexp |> Str.split in
  let split_space =  "[ \t]+" |> Str.regexp |> Str.split in
  let lines = file |> input_string |> split_blank_line in 
  let parse_moves  = String.split_on_char ',' >> List.map int_of_string in
  let parse_boards =  
    List.map (
      String.split_on_char '\n' >> 
      List.map (split_space >> List.map (int_of_string >> unmarked)))
  in
  let moves, boards = 
    lines |> List.hd |> parse_moves, 
    lines |> List.tl |> parse_boards 
  in
  Printf.printf "part one: %d \n" (part1 moves boards);
  Printf.printf "part two: %d \n" (part2 moves boards)