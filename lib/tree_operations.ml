[@@@warning "-27"]  (* Disables unused variable warnings, improve code later *)

open Types

let new_node color price orders left right = Node{color;price;orders;left;right}

(* Change this in future so that it handles orders individually *)
let combine_orders existing_orders new_order =
  let total_volume = List.fold_left (fun acc order -> acc + order.volume) 0 existing_orders in
  [{new_order with volume = total_volume + new_order.volume}]

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
let flip_colors = function
  | Node {color; left = Node {color = Red; _} as l; orders; price; right = Node {color = Red; _} as r} ->
      (*(warning 27 [unused-var-strict]): unused variable color.*)
      new_node Red price orders 
        (match l with Node n -> new_node Black n.price n.orders n.left n.right | _ -> Empty)
        (match r with Node n -> new_node Black n.price n.orders n.left n.right | _ -> Empty)
  | _ -> failwith "flip_colors"

let is_red = function
  | Node {color = Red; _} -> true
  | _ -> false