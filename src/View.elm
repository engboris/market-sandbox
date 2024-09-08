module View exposing (..)

import Model exposing (..)
import Update exposing (..)

import Html exposing (Html, input, button, div, text, br)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Model exposing (..)

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
