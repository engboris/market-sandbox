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
