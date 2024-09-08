module Model exposing (..)

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
  { time              : Int
  , buy_orders        : List LimitOrder
  , sell_orders       : List LimitOrder
  , tmp_order         : LimitOrderField
  , tmp_market_amount : String
  }

init : Model
init =
  { time = 0
  , buy_orders = []
  , sell_orders = []
  , tmp_order = { limit = "", amount = "" }
  , tmp_market_amount = ""
  }

get_bid : Model -> Maybe Int
get_bid model =
  case List.head model.buy_orders of
    Nothing -> Nothing
    Just o -> Just o.limit

get_ask : Model -> Maybe Int
get_ask model =
  case List.head model.sell_orders of
    Nothing -> Nothing
    Just o -> Just o.limit
