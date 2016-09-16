module FeatureFlax exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import List exposing (..)

type ToggleState = On | Off

type alias Toggle =
  { name : String,
    state : ToggleState
  }

type alias Model = List Toggle
type Msg = None

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
  ( [{ name = "NewLogin", state = Off }, { name = "ReorderButtons", state = On }], Cmd.none )

update msg model =
  ([], Cmd.none)

view : Model -> Html Msg
view model =
  table [ class "toggles"]
    ( model
      |> map drawToggle
    )
drawToggle toggle =
  case toggle.state of
    On ->
      tr [ class "toggleOn" ]
      [
        td [] [ text toggle.name ],
        td [] [ text "On" ]
      ]
    Off -> 
      tr [ class "toggleOff" ]
      [
        td [] [ text toggle.name ],
        td [] [ text "Off" ]
      ]

subscriptions model =
  Sub.none


