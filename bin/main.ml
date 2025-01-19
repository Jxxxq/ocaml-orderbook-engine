[@@@warning "-26-27-32-37-69"]  (* Disables unused variable warnings *)

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

let new_node color price orders left right = Node{color;price;orders;left;right}

let combine_orders existing_orders new_order =
  let total_volume = List.fold_left (fun acc order -> acc + order.volume) 0 existing_orders in
  [{new_order with volume = total_volume + new_order.volume}]
  
let rec insert tree order =
  match tree with
  | Empty -> new_node Red order.price [order] Empty Empty
  | Node {color; left; orders; price; right} ->
    if order.price < price then 
      new_node color price orders (insert left order) right
    else if order.price > price then
      new_node color price orders left (insert right order)
    else
      new_node color price (combine_orders orders order) left right

let rotate_left = function
  | Node {color; left; orders; price; right = Node {color = Red; left = rleft; orders = rorders; price = rprice; right = rright}} ->
      new_node color rprice rorders 
        (new_node Red price orders left rleft) 
        rright
  | _ -> failwith "rotate_left"

let rotate_right = function
  | Node {color; left = Node {color = Red; left = lleft; orders = lorders; price = lprice; right = lright}; orders; price; right} ->
      new_node color lprice lorders
        lleft
        (new_node Red price orders lright right)
  | _ -> failwith "rotate_right"

(** Flips the colors of a node and its children *)
let flip_colors = function
  | Node {color; left = Node {color = Red; _} as l; orders; price; right = Node {color = Red; _} as r} ->
      new_node Red price orders 
        (match l with Node n -> new_node Black n.price n.orders n.left n.right | _ -> Empty)
        (match r with Node n -> new_node Black n.price n.orders n.left n.right | _ -> Empty)
  | _ -> failwith "flip_colors"

let is_red = function
  | Node {color = Red; _} -> true
  | _ -> false


(** ----------- Red-Black tree properties: -----------
    1. Node Color: Each node is either red or black.
    2. Root Property: The root of the tree is always black.
    3. Red Property: Red nodes cannot have red children (no two consecutive red nodes on any path).
    4. Black Property: Every path from a node to its descendant null nodes (leaves) has the same number of black nodes.
    5. Leaf Property: All leaves (NIL nodes) are black. (No leaves in this case)
*)


(** Balances the tree after insertion to maintain Red-Black properties *)
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

let insert_balanced tree order =
  let rec do_insert = function
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

let rec search_price tree target_price =
  match tree with
  | Empty -> None
  | Node {price; orders; left; right; _} ->
      if target_price = price then Some orders
      else if target_price < price then search_price left target_price
      else search_price right target_price

let rec find_best_bid = function
  | Empty -> None
  | Node {price; orders; right; _} ->
      match right with
      | Empty -> Some (price, orders)
      | _ -> find_best_bid right

let rec find_best_ask = function
  | Empty -> None
  | Node {price; orders; left; _} ->
      match left with
      | Empty -> Some (price, orders)  
      | _ -> find_best_ask left

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
            | None -> sell_tree  (* shouldn't happen *)
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
      else
        (* Best ask is too high, add to buy tree *)
        (insert_balanced buy_tree order, sell_tree)

(* Execute sell order *)
let rec execute_sell market_state order =
  let (buy_tree, sell_tree) = market_state in
  match find_best_bid buy_tree with
  | None -> 
      (* No matching buy orders, add to sell tree *)
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
            | None -> buy_tree  (* shouldn't happen *)
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
      else
        (* Best bid is too low, add to sell tree *)
        (buy_tree, insert_balanced sell_tree order)


let rec collect_orders tree =
  match tree with
  | Empty -> []
  | Node {price; orders; left; right; _} ->
      let left_orders = collect_orders left in
      let right_orders = collect_orders right in
      left_orders @ [(price, List.hd orders)] @ right_orders

let test_Sell_order = {
  orderId = 12;
  timestamp = 0.0;
  side = Sell;
  price = 102.0;
  volume = 30
};;
let test_Buy_order = {
  orderId = 13;
  timestamp = 0.0;
  side = Buy;
  price = 100.0;
  volume = 10
};;


let buy_tree = 
  let t1 = insert_balanced Empty {orderId=1; timestamp=0.0; side=Buy; price=100.0; volume=10} in
  let t2 = insert_balanced t1 {orderId=2; timestamp=0.0; side=Buy; price=102.0; volume=20} in
  let t3 = insert_balanced t2 {orderId=3; timestamp=0.0; side=Buy; price=98.0; volume=15} in
  let t4 = insert_balanced t3 {orderId=4; timestamp=0.0; side=Buy; price=103.0; volume=25} in
  let t5 = insert_balanced t4 {orderId=5; timestamp=0.0; side=Buy; price=101.0; volume=30} in
  t5;;
let sell_tree = 
  let t1 = insert_balanced Empty {orderId=6; timestamp=0.0; side=Sell; price=104.0; volume=12} in
  let t2 = insert_balanced t1 {orderId=7; timestamp=0.0; side=Sell; price=106.0; volume=18} in
  let t3 = insert_balanced t2 {orderId=8; timestamp=0.0; side=Sell; price=105.0; volume=22} in
  let t4 = insert_balanced t3 {orderId=9; timestamp=0.0; side=Sell; price=108.0; volume=15} in
  let t5 = insert_balanced t4 {orderId=10; timestamp=0.0; side=Sell; price=107.0; volume=20} in
  let t6 = insert_balanced t5 {orderId=11; timestamp=0.0; side=Sell; price=107.0; volume=20} in
  t6;;

let (updated_buy_tree, updated_sell_tree) = 
  execute_sell (buy_tree, sell_tree) test_Sell_order;;

let (final_buy_tree, final_sell_tree) =
  execute_buy (updated_buy_tree, updated_sell_tree) test_Buy_order;;

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

print_endline "\nInitial orderbook:";;
print_orderbook (buy_tree, sell_tree);;

print_endline "\nAfter executing orders:";;
print_orderbook (final_buy_tree, final_sell_tree);;
