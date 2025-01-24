open Ocaml_orderbook_lib (* OPEN FIRST *)
open Types
open Order_operations
open Execution
open Utils

let rec interact_with_orderbook market_state order_id =
  print_endline "\nWhat would you like to do?";
  print_endline "1. Place Buy order";
  print_endline "2. Place Sell order";
  print_endline "3. Remove price level";
  print_endline "4. View orderbook";
  print_endline "5. Exit";
  Printf.printf "> ";

  let choice = read_line() in
  match choice with
  | "1" | "2" -> 
      let price = read_float "Enter price: " in
      if Float.is_nan price then
        interact_with_orderbook market_state order_id
      else
        let volume = read_int "Enter volume: " in
        if volume <= 0 then
          interact_with_orderbook market_state order_id
        else begin
          let order = {
            orderId = order_id;
            timestamp = 0.0;
            side = if choice = "1" then Buy else Sell;  
            price = price;
            volume = volume
          } in
          let new_market_state = 
            if order.side = Buy then
              execute_buy market_state order
            else
              execute_sell market_state order
          in
          print_orderbook new_market_state;
          interact_with_orderbook new_market_state (order_id + 1)
        end

  | "3" ->
      let price = read_float "Enter price to remove: " in
      if Float.is_nan price then
        interact_with_orderbook market_state order_id
      else begin
        let (buy_tree, sell_tree) = market_state in
        match (search_price buy_tree price, search_price sell_tree price) with
        | (None, None) -> 
            Printf.printf "No orders exist at price %.2f\n" price;
            interact_with_orderbook market_state order_id
        | _ ->
            let new_buy_tree = remove_node buy_tree price in
            let new_sell_tree = remove_node sell_tree price in
            let new_market_state = (new_buy_tree, new_sell_tree) in
            Printf.printf "Removed all orders at price %.2f\n" price;
            print_orderbook new_market_state;
            interact_with_orderbook new_market_state order_id
      end

  | "4" ->
      print_orderbook market_state;
      interact_with_orderbook market_state order_id

  | "5" ->
      print_endline "Goodbye!"

  | _ ->
      print_endline "Invalid choice. Please try again.";
      interact_with_orderbook market_state order_id

let () =
  let initial_state = (Empty, Empty) in
  print_endline "\nWelcome to the Interactive Orderbook Engine!";
  interact_with_orderbook initial_state 1
