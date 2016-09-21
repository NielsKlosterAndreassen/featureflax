module FeatureFlax exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import List exposing (..)

type ToggleState = On | Off

type Environment = Development | Staging | Production

type alias Toggle =
  { name : String,
    state : ToggleState,
    environment : Environment
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
  ( [{ name = "NewLogin", state = Off, environment = Development },
     { name = "ReorderButtons", state = On, environment = Staging }], Cmd.none )

update msg model =
  ([], Cmd.none)

view : Model -> Html Msg
view model =
  table [ class "toggles" ]
    ( model
      |> map drawToggle
    )
drawToggle toggle =
  tr [ class "toggleOn" ] [
    td [] [ text toggle.name ],
    td [] [ drawSlider toggle ]
  ]

drawSlider toggle =
  label [ class "switch"] [ 
    input [ type' "checkbox", checked (if toggle.state == On then True else False ) ] [],
    div [ class "slider" ] []
  ]

subscriptions model =
  Sub.none


