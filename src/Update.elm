module Update exposing (..)

import Model exposing (..)

type Msg =
    LimitBuy | LimitSell
  | MarketBuy Int | MarketSell Int
  | LimitField String | LimitAmountField String
  | MarketAmountField String
  | Left | Right

insert_order : (Int -> Int -> Bool)
  -> LimitOrder -> List LimitOrder -> List LimitOrder
insert_order cmp o orders =
  case orders of
    [] -> [o]
    h::t ->
      if cmp o.limit h.limit then
        o::h::t
      else
        h::(insert_order cmp o t)

consume_aux orders amount orders_tmp current_amount =
  case orders_tmp of
    [] -> orders
    h::t ->
      let new_amount = current_amount+h.amount in
      if new_amount == amount then t
      else if new_amount > amount then orders
      else consume_aux orders amount t new_amount

consume : List LimitOrder -> Int -> List LimitOrder
consume orders amount = consume_aux orders amount orders 0

update : Msg -> Model -> Model
update msg model =
  case msg of
    LimitBuy ->
      case (String.toInt model.tmp_order.limit,
           String.toInt model.tmp_order.amount) of
        (Just limit, Just amount) ->
          let order = { limit = limit, amount = amount } in
          { model | buy_orders = insert_order (>=) order model.buy_orders }
        _ -> model
    LimitSell ->
      case (String.toInt model.tmp_order.limit,
           String.toInt model.tmp_order.amount) of
        (Just limit, Just amount) ->
          let order = { limit = limit, amount = amount } in
          { model | sell_orders = insert_order (<=) order model.sell_orders }
        _ -> model
    MarketBuy amount ->
      let orders = model.sell_orders in
      { model | sell_orders = consume orders amount }
    MarketSell amount ->
      let orders = model.buy_orders in
      { model | buy_orders = consume orders amount }
    LimitField s ->
      { model | tmp_order = { limit = s, amount = model.tmp_order.amount } }
    LimitAmountField s ->
      { model | tmp_order = { amount = s, limit = model.tmp_order.limit } }
    MarketAmountField s ->
      { model | tmp_market_amount = s }
    Left ->
      { model | time = if model.time > 0 then model.time-1 else 0 }
    Right ->
      { model | time = model.time + 1 }
