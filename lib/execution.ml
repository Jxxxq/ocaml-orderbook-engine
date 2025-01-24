open Types
open Order_operations
open Rb_tree
open Tree_operations

let rec execute_buy market_state order =
  let (buy_tree, sell_tree) = market_state in
  match find_best_ask sell_tree with
  | None -> 
      Printf.printf "Added limit order: Buy %d shares at %.2f\n" order.volume order.price;
      (insert_balanced buy_tree order, sell_tree)
  | Some (ask_price, sell_orders) ->
      if order.price >= ask_price then
        let sell_order = List.hd sell_orders in
        if sell_order.volume > order.volume then begin
          Printf.printf "Executed: Buy %d shares at %.2f\n" order.volume ask_price;
          let updated_sell = {sell_order with volume = sell_order.volume - order.volume} in
          let updated_tree = 
            match search_price sell_tree ask_price with
            | None -> failwith "Internal error: Price not found in tree after successful match"
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
        end else if sell_order.volume = order.volume then begin
          Printf.printf "Executed: Buy %d shares at %.2f\n" order.volume ask_price;
          (buy_tree, remove_node sell_tree ask_price)
        end else begin
          Printf.printf "Executed: Buy %d shares at %.2f\n" sell_order.volume ask_price;
          let remaining_volume = order.volume - sell_order.volume in
          let remaining_order = {order with volume = remaining_volume} in
          execute_buy (buy_tree, remove_node sell_tree ask_price) remaining_order
        end
      else begin
        Printf.printf "Added limit order: Buy %d shares at %.2f\n" order.volume order.price;
        (insert_balanced buy_tree order, sell_tree)
      end

let rec execute_sell market_state order =
  let (buy_tree, sell_tree) = market_state in
  match find_best_bid buy_tree with
  | None -> 
      Printf.printf "Added limit order: Sell %d shares at %.2f\n" order.volume order.price;
      (buy_tree, insert_balanced sell_tree order)
  | Some (bid_price, buy_orders) ->
      if order.price <= bid_price then
        let buy_order = List.hd buy_orders in
        if buy_order.volume > order.volume then begin
          Printf.printf "Executed: Sell %d shares at %.2f\n" order.volume bid_price;
          let updated_buy = {buy_order with volume = buy_order.volume - order.volume} in
          let updated_tree = 
            match search_price buy_tree bid_price with
            | None -> failwith "Internal error: Price not found in tree after successful match"
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
        end else if buy_order.volume = order.volume then begin
          Printf.printf "Executed: Sell %d shares at %.2f\n" order.volume bid_price;
          (remove_node buy_tree bid_price, sell_tree)
        end else begin
          Printf.printf "Executed: Sell %d shares at %.2f\n" buy_order.volume bid_price;
          let remaining_volume = order.volume - buy_order.volume in
          let remaining_order = {order with volume = remaining_volume} in
          execute_sell (remove_node buy_tree bid_price, sell_tree) remaining_order
        end
      else begin
        Printf.printf "Added limit order: Sell %d shares at %.2f\n" order.volume order.price;
        (buy_tree, insert_balanced sell_tree order)
      end
