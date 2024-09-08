module Update exposing (..)

import Model exposing (..)

type Msg =
    LimitBuy | LimitSell
  | LimitField String | AmountField String
  | Left | Right

insert_order : LimitOrder -> List LimitOrder -> List LimitOrder
insert_order o orders =
  case orders of
    [] -> [o]
    h::t ->
      if o.limit <= h.limit then
        o::h::t
      else
        h::(insert_order o t)

update : Msg -> Model -> Model
update msg model =
  case msg of
    LimitBuy ->
      case (String.toInt model.tmp_order.limit,
           String.toInt model.tmp_order.amount) of
        (Just limit, Just amount) ->
          let order = { limit = limit, amount = amount } in
          { model | buy_orders = insert_order order model.buy_orders }
        _ -> model
    LimitSell ->
      case (String.toInt model.tmp_order.limit,
           String.toInt model.tmp_order.amount) of
        (Just limit, Just amount) ->
          let order = { limit = limit, amount = amount } in
          { model | sell_orders = insert_order order model.sell_orders }
        _ -> model
    LimitField s ->
      { model | tmp_order = { limit = s, amount = model.tmp_order.amount } }
    AmountField s ->
      { model | tmp_order = { amount = s, limit = model.tmp_order.limit } }
    Left ->
      { model | time = if model.time > 0 then model.time-1 else 0 }
    Right ->
      { model | time = model.time + 1 }
