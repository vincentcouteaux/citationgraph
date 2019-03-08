module Main exposing (..)

import Browser
import Html
import Model
import View


--main : Program Never Model.Model Model.Msg
main =
    Browser.element
        { init = Model.init
        , update = Model.update
        , subscriptions = Model.subscriptions
        , view = View.view
        }
