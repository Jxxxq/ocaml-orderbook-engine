[@@@warning "-26-27-32-37-69"]  (* Disables unused variable warnings *)

(* Types *)

type side = Buy | Sell

type order = {
  orderId: int;
  timestamp: float;
  side: side;
  price: float;
  volume: int;
}

type color = Red | Black

type order_tree =
  | Empty 
  | Node of {
      color: color;
      left: order_tree;
      orders: order list;
      price: float;
      right: order_tree
    }

(* Tree Operations *)

let new_node color price orders left right = Node{color;price;orders;left;right}

let combine_orders existing_orders new_order =
  let total_volume = List.fold_left (fun acc order -> acc + order.volume) 0 existing_orders in
  [{new_order with volume = total_volume + new_order.volume}]
  

(* Rotates tree left *)
let rotate_left = function
  | Node {color; left; orders; price; right = Node {color = Red; left = rleft; orders = rorders; price = rprice; right = rright}} ->
      new_node color rprice rorders 
        (new_node Red price orders left rleft) 
        rright
  | _ -> failwith "rotate_left"


(* Rotates tree right *)  
let rotate_right = function
  | Node {color; left = Node {color = Red; left = lleft; orders = lorders; price = lprice; right = lright}; orders; price; right} ->
      new_node color lprice lorders
        lleft
        (new_node Red price orders lright right)
  | _ -> failwith "rotate_right"

(* Flips the colors of a node and its children *)
let flip_colors = function
  | Node {color; left = Node {color = Red; _} as l; orders; price; right = Node {color = Red; _} as r} ->
      new_node Red price orders 
        (match l with Node n -> new_node Black n.price n.orders n.left n.right | _ -> Empty)
        (match r with Node n -> new_node Black n.price n.orders n.left n.right | _ -> Empty)
  | _ -> failwith "flip_colors"

(* Just checks to see if a node is red or not *)
let is_red = function
  | Node {color = Red; _} -> true
  | _ -> false


(* ----------- Red-Black tree properties: -----------
    1. Node Color: Each node is either red or black.
    2. Root Property: The root of the tree is always black.
    3. Red Property: Red nodes cannot have red children (no two consecutive red nodes on any path).
    4. Black Property: Every path from a node to its descendant null nodes (leaves) has the same number of black nodes.
    5. Leaf Property: All leaves (NIL nodes) are black. (No leaves in this case)
*)


(* Balances the tree after insertion to maintain the Red-Black properties above *)
let balance tree =
  match tree with
  | Node {color = Black; left; orders; price; right} as node ->
      if is_red left && is_red right then
        flip_colors node  
      else if is_red left && is_red (match left with 
          | Node {left = l; _} -> l 
          | Empty -> Empty) then
      (* Left-left case *)
      rotate_right node
    else if is_red left && is_red (match left with 
        | Node {right = r; _} -> r 
        | Empty -> Empty) then
      (* Left-right case *)
      let left' = rotate_left left in
      rotate_right (new_node Black price orders left' right)
    else if is_red right && is_red (match right with 
        | Node {left = l; _} -> l 
        | Empty -> Empty) then
      (* Right-left case *)
      let right' = rotate_right right in
      rotate_left (new_node Black price orders left right')
    else if is_red right && is_red (match right with 
        | Node {right = r; _} -> r 
        | Empty -> Empty) then
      (* Right-right case *)
      rotate_left node
    else
      node
| node -> node

(* Inserting while balancing at each layer *)
let insert_balanced tree order =
  let rec do_insert = function (* Helper function that does the insertion recursively *)
    | Empty -> new_node Red order.price [order] Empty Empty
    | Node {color; left; orders; price; right} as node ->
        if order.price < price then
          balance (new_node color price orders (do_insert left) right)
        else if order.price > price then
          balance (new_node color price orders left (do_insert right))
        else
          new_node color price (combine_orders orders order) left right
  in
  match do_insert tree with 
  | Node {left; orders; price; right; _} -> 
      new_node Black price orders left right  (* Root is always black *)
  | Empty -> Empty

(* Returns the node of the given value *)
let rec search_price tree target_price =
  match tree with
  | Empty -> None
  | Node {price; orders; left; right; _} ->
      if target_price = price then Some orders
      else if target_price < price then search_price left target_price
      else search_price right target_price

(* Finds the highest bid *)
let rec find_best_bid = function
  | Empty -> None
  | Node {price; orders; right; _} ->
      match right with
      | Empty -> Some (price, orders)
      | _ -> find_best_bid right

(* Finds the lowest ask *)
let rec find_best_ask = function
  | Empty -> None
  | Node {price; orders; left; _} ->
      match left with
      | Empty -> Some (price, orders)  
      | _ -> find_best_ask left

(* Recursive helper function *)
let rec print_tree_helper indent = function
  | Empty -> Printf.printf "%sEmpty\n" indent
  | Node {color; price; orders; left; right} ->
      Printf.printf "%sNode(color=%s, price=%.2f, orders=[%s])\n"
        indent
        (match color with Red -> "Red" | Black -> "Black")
        price
        (String.concat ", " (List.map (fun o -> 
          Printf.sprintf "{id=%d, side=%s, vol=%d}" 
            o.orderId 
            (match o.side with Buy -> "Buy" | Sell -> "Sell")
            o.volume
        ) orders));
      print_tree_helper (indent ^ "  ") left;
      print_tree_helper (indent ^ "  ") right

let print_tree tree =
  print_endline "Tree structure:";
  print_tree_helper "" tree

(** Removes a node with target_price while maintaining the Red-Black tree properties.
    Uses the standard deletion approach: 
    - For leaf (doesn't apply)/single child: Direct removal
    - For two children: Replace with successor (min value in right subtree) (find_min function)
    Then rebalances the tree*)
let rec remove_node tree target_price =
  match tree with
  | Empty -> Empty
  | Node {color; left; orders; price; right} ->
      if target_price < price then
        balance (new_node color price orders (remove_node left target_price) right)
      else if target_price > price then
        balance (new_node color price orders left (remove_node right target_price))
      else 
        match (left, right) with
        | (Empty, Empty) -> Empty
        | (node, Empty) | (Empty, node) -> node
        | _ ->
            (* Find successor (minimum in right subtree) *)
            let rec find_min = function
              | Empty -> failwith "should not happen"
              | Node {price; orders; left = Empty; _} -> (price, orders)
              | Node {left; _} -> find_min left
            in
            let (succ_price, succ_orders) = find_min right in
            balance (new_node color succ_price succ_orders left (remove_node right succ_price))

(* Execute buy order *)
let rec execute_buy market_state order =
  let (buy_tree, sell_tree) = market_state in
  match find_best_ask sell_tree with
  | None -> 
      (* No matching sell orders, add to buy tree *)
      Printf.printf "Added limit order: Buy %d shares at %.2f\n" order.volume order.price;
      (insert_balanced buy_tree order, sell_tree)
  | Some (ask_price, sell_orders) ->
      if order.price >= ask_price then
        (* We can match this order *)
        let sell_order = List.hd sell_orders in
        if sell_order.volume > order.volume then begin
          (* Partial fill - update sell order *)
          Printf.printf "Executed: Buy %d shares at %.2f\n" order.volume ask_price;
          let updated_sell = {sell_order with volume = sell_order.volume - order.volume} in
          (* Instead of remove + insert, directly update the node's orders *)
          let updated_tree = 
            match search_price sell_tree ask_price with
            | None ->     
                failwith "Internal error: Price not found in tree after successful match"
            | Some _ -> 
                let rec update_node tree =
                  match tree with
                  | Empty -> Empty
                  | Node {color; left; orders=_; price; right} when price = ask_price ->
                      new_node color price [updated_sell] left right
                  | Node {color; left; orders; price; right} ->
                      if ask_price < price then
                        new_node color price orders (update_node left) right
                      else
                        new_node color price orders left (update_node right)
                in
                update_node sell_tree
          in
          (buy_tree, updated_tree)
        end
        else if sell_order.volume = order.volume then begin
          (* Complete fill - remove sell order *)
          Printf.printf "Executed: Buy %d shares at %.2f\n" order.volume ask_price;
          (buy_tree, remove_node sell_tree ask_price)
        end
        else begin
          (* Partial fill - need to continue buying *)
          Printf.printf "Executed: Buy %d shares at %.2f\n" sell_order.volume ask_price;
          let remaining_volume = order.volume - sell_order.volume in
          let remaining_order = {order with volume = remaining_volume} in
          execute_buy 
            (buy_tree, remove_node sell_tree ask_price)
            remaining_order
        end
      else begin
        (* Best ask is too high, add to buy tree *)
        Printf.printf "Added limit order: Buy %d shares at %.2f\n" order.volume order.price;

        (insert_balanced buy_tree order, sell_tree)
      end
        

(* Execute sell order *)
let rec execute_sell market_state order =
  let (buy_tree, sell_tree) = market_state in
  match find_best_bid buy_tree with
  | None -> 
      (* No matching buy orders, add to sell tree *)
      Printf.printf "Added limit order: Sell %d shares at %.2f\n" order.volume order.price;
      (buy_tree, insert_balanced sell_tree order)
  | Some (bid_price, buy_orders) ->
      if order.price <= bid_price then
        (* We can match this order *)
        let buy_order = List.hd buy_orders in
        if buy_order.volume > order.volume then begin
          (* Partial fill - update buy order *)
          Printf.printf "Executed: Sell %d shares at %.2f\n" order.volume bid_price;
          let updated_buy = {buy_order with volume = buy_order.volume - order.volume} in
          (* Instead of remove + insert, directly update the node's orders *)
          let updated_tree = 
            match search_price buy_tree bid_price with
            | None -> 
                failwith "Internal error: Price not found in tree after successful match"
            | Some _ -> 
                let rec update_node tree =
                  match tree with
                  | Empty -> Empty
                  | Node {color; left; orders=_; price; right} when price = bid_price ->
                      new_node color price [updated_buy] left right
                  | Node {color; left; orders; price; right} ->
                      if bid_price < price then
                        new_node color price orders (update_node left) right
                      else
                        new_node color price orders left (update_node right)
                in
                update_node buy_tree
          in
          (updated_tree, sell_tree)
        end
        else if buy_order.volume = order.volume then begin
          (* Complete fill - remove buy order *)
          Printf.printf "Executed: Sell %d shares at %.2f\n" order.volume bid_price;
          (remove_node buy_tree bid_price, sell_tree)
        end
        else begin
          (* Partial fill - need to continue selling *)
          Printf.printf "Executed: Sell %d shares at %.2f\n" buy_order.volume bid_price;
          let remaining_volume = order.volume - buy_order.volume in
          let remaining_order = {order with volume = remaining_volume} in
          execute_sell
            (remove_node buy_tree bid_price, sell_tree)
            remaining_order
        end
      else begin
        (* Best bid is too low, add to sell tree *)
        Printf.printf "Added limit order: Sell %d shares at %.2f\n" order.volume order.price;
        (buy_tree, insert_balanced sell_tree order)
      end 

(* Collects all orders from the tree in sorted order by doing an in-order traversal *)
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
  
  (* Print asks from highest to lowest *)
  List.iter (fun (price, order) ->
    Printf.printf "ASK     %.2f    %d\n" price order.volume
  ) asks;
  
  print_endline "----------------------------------------";
  
  (* Print bids from highest to lowest *)
  List.iter (fun (price, order) ->
    Printf.printf "BID     %.2f    %d\n" price order.volume
  ) bids;
  
  print_endline "----------------------------------------";;

(* Parse float input *)
let read_float prompt =
  Printf.printf "%s" prompt;
  try float_of_string (read_line())
  with Failure _ -> 
    Printf.printf "Invalid input. Please enter a valid number.\n";
    nan

(* Parse integer input *)
let read_int prompt =
  Printf.printf "%s" prompt;
  try int_of_string (read_line())
  with Failure _ -> 
    Printf.printf "Invalid input. Please enter a valid number.\n";
    -1

(* Read yes/no input *)
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

(* Run *)
let () =
  let initial_state = (Empty, Empty) in
  print_endline "\nWelcome to the Interactive Orderbook Engine!";
  interact_with_orderbook initial_state 1