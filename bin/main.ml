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
