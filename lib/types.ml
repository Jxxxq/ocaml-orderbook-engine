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