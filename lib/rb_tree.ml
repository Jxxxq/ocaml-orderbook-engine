[@@@warning "-26"]  (* Disables unused variable warnings, improve code later *)

open Types
open Tree_operations

let balance tree =
  match tree with
  | Node {color = Black; left; orders; price; right} as node ->
      if is_red left && is_red right then
        flip_colors node  
      else if is_red left && is_red (match left with 
          | Node {left = l; _} -> l 
          | Empty -> Empty) then
        rotate_right node
      else if is_red left && is_red (match left with 
          | Node {right = r; _} -> r 
          | Empty -> Empty) then
        let left' = rotate_left left in
        rotate_right (new_node Black price orders left' right)
      else if is_red right && is_red (match right with 
          | Node {left = l; _} -> l 
          | Empty -> Empty) then
        let right' = rotate_right right in
        rotate_left (new_node Black price orders left right')
      else if is_red right && is_red (match right with 
          | Node {right = r; _} -> r 
          | Empty -> Empty) then
        rotate_left node
      else
        node
  | node -> node

let insert_balanced tree order =
  let rec do_insert = function
    | Empty -> new_node Red order.price [order] Empty Empty
    | Node {color; left; orders; price; right} as node (*(warning 26 [unused-var]): unused variable node.*)->
        if order.price < price then
          balance (new_node color price orders (do_insert left) right)
        else if order.price > price then
          balance (new_node color price orders left (do_insert right))
        else
          new_node color price (combine_orders orders order) left right
  in
  match do_insert tree with 
  | Node {left; orders; price; right; _} -> 
      new_node Black price orders left right
  | Empty -> Empty