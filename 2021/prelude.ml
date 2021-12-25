
let sum xs = List.fold_left (+) 0 xs

let singleton x = [x]

let (>>) f g a = a |> f |> g

let input_lines file = 
  let rec loop chan = 
    match input_line chan with 
    | line -> line :: loop chan 
    | exception End_of_file -> []
  in
  let chan = open_in file in 
  let result = loop chan in 
  close_in chan;
  result

let input_string = input_lines >> String.concat "\n"

