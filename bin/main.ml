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

let rec insert tree order =
  match tree with
  | Empty -> new_node Red order.price [order] Empty Empty
  | Node {color; left; orders; price; right} ->
    if order.price < price then 
      new_node color price orders (insert left order) right
    else if order.price > price then
      new_node color price orders left (insert right order)
    else
      new_node color price (order :: orders) left right

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
    5. Leaf Property: All leaves (NIL nodes) are black.
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
          new_node color price (order :: orders) left right
  in
  match do_insert tree with
  | Node {left; orders; price; right; _} -> 
      new_node Black price orders left right  (* Root is always black *)
  | Empty -> Empty


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

  let tree = 
    let t1 = insert_balanced Empty {orderId=1; timestamp=0.0; side=Buy; price=100.0; volume=10} in
    let t2 = insert_balanced t1 {orderId=2; timestamp=0.0; side=Buy; price=101.0; volume=20} in
    let t3 = insert_balanced t2 {orderId=3; timestamp=0.0; side=Buy; price=102.0; volume=30} in
    let t4 = insert_balanced t3 {orderId=4; timestamp=0.0; side=Buy; price=103.0; volume=20} in
    let t5 = insert_balanced t4 {orderId=5; timestamp=0.0; side=Buy; price=104.0; volume=15} in
    let t6 = insert_balanced t5 {orderId=6; timestamp=0.0; side=Buy; price=105.0; volume=35} in
    t6;;

print_tree tree;;