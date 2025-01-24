open Types

let rec collect_orders tree =
  match tree with
  | Empty -> []
  | Node {price; orders; left; right; _} ->
      let left_orders = collect_orders left in
      let right_orders = collect_orders right in
      left_orders @ [(price, List.hd orders)] @ right_orders

let print_orderbook (buy_tree, sell_tree) =
  let asks = List.rev (collect_orders sell_tree) in
  let bids = List.rev (collect_orders buy_tree) in

  print_endline "\nOrderbook:";
  print_endline "----------------------------------------";
  print_endline "Type    Price    Volume";
  print_endline "----------------------------------------";
  
  List.iter (fun (price, order) ->
    Printf.printf "ASK     %.2f    %d\n" price order.volume
  ) asks;
  
  print_endline "----------------------------------------";
  
  List.iter (fun (price, order) ->
    Printf.printf "BID     %.2f    %d\n" price order.volume
  ) bids;
  
  print_endline "----------------------------------------"

let read_float prompt =
  Printf.printf "%s" prompt;
  try float_of_string (read_line())
  with Failure _ -> 
    Printf.printf "Invalid input. Please enter a valid number.\n";
    nan

let read_int prompt =
  Printf.printf "%s" prompt;
  try int_of_string (read_line())
  with Failure _ -> 
    Printf.printf "Invalid input. Please enter a valid number.\n";
    -1
