module Main exposing(..)

import Browser
import Html exposing (Html, input, button, div, text, br)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)

-- MAIN
main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type OrderType = Sell | Buy

type alias LimitOrder =
  { limit  : Int
  , amount : Int
  }

type alias LimitOrderField =
  { limit  : String
  , amount : String
  }


type alias Model =
  { time        : Int
  , buy_orders  : List LimitOrder
  , sell_orders : List LimitOrder
  , tmp_order   : LimitOrderField
  }

init : Model
init =
  { time = 0
  , buy_orders = []
  , sell_orders = []
  , tmp_order = { limit = "", amount = "" }
  }

-- UPDATE
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

-- VIEW
viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

header : Model -> List (Html Msg)
header model =
  [ div []
    [ text ("Time: " ++ (String.fromInt model.time) ++ " ")
    , button [ onClick Left ] [ text "<" ]
    , button [ onClick Right ] [ text ">" ]
    ]
  ]

commands : Model -> List (Html Msg)
commands model =
  [ div []
    [ text "Limit order "
    , viewInput "text" "amount" model.tmp_order.amount AmountField
    , text " units for $"
    , viewInput "text" "price" model.tmp_order.limit LimitField
    , button [ onClick LimitBuy ] [ text "Buy" ]
    , button [ onClick LimitSell ] [ text "Sell" ]
    ]
  ]

format_order : LimitOrder -> String
format_order order =
  (String.fromInt order.amount) ++
  " units for $" ++
  (String.fromInt order.limit)

orderbook : Model -> List (Html Msg)
orderbook model =
  [ br [] [] ] ++
  [ div [] [text ("Orderbook (Bid)")] ] ++
  List.map (\o ->
    div [] [text (format_order o)]
  ) model.buy_orders
  ++ [ br [] [] ] ++
  [ div [] [text ("Orderbook (Ask)")] ] ++
  List.map (\o ->
    div [] [text (format_order o)]
  ) model.sell_orders

view : Model -> Html Msg
view model =
  div [] (
    (header model) ++
    (commands model) ++
    (orderbook model)
  )
