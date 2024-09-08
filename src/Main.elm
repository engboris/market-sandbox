module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)

import Browser

main = Browser.sandbox { init = init, update = update, view = view }
