open Types
open Tree_operations
open Rb_tree

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
              | Empty -> failwith "Internal error: Failed to remove node"
              | Node {price; orders; left = Empty; _} -> (price, orders)
              | Node {left; _} -> find_min left
            in
            let (succ_price, succ_orders) = find_min right in
            balance (new_node color succ_price succ_orders left (remove_node right succ_price))
