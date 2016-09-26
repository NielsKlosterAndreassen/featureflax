module FeatureFlax exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import List exposing (..)

type alias Environment = String

type alias Feature =
  { name : String,
    environments : List Environment
  }

type alias Model = List Feature
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
  ( [{ name = "NewLogin", environments = [ "Staging" ]  },
     { name = "ReorderButtons", environments = [ "Staging", "Production" ] }], Cmd.none )

update msg model =
  ([], Cmd.none)

view : Model -> Html Msg
view model =
  let environments = [ "Staging", "Production" ] in
  table [ class "toggles"] (
    drawHeader environments :: 
    (model |> map (drawFeature environments))
  )

drawHeader environments =
  thead [] ( "" :: environments |> map (\ header -> th [] [ text header ]) )

drawFeature environments feature =
  tr [ ] ( td [] [text feature.name] :: (environments |> map (drawSlider feature)))

drawSlider feature environment =
  let isChecked = feature.environments |> any (\ x -> x == environment) in
  td [] [ label [ class "switch"] [ 
    input [ type' "checkbox", checked isChecked ] [],
    div [ class "slider" ] []
  ]]

subscriptions model =
  Sub.none
