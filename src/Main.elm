module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        column [ width fill, height fill ]
            [ header
            , middle
            , footer
            ]


header : Element msg
header =
    row [ explain Debug.todo, width fill ]
        [ el [ alignLeft ] <| text "Left"
        , el [ centerX ] <| text "Center"
        , el [ alignRight ] <| text "Right"
        ]


middle : Element msg
middle =
    row [ explain Debug.todo, width fill, height fill ]
        [ column [ explain Debug.todo, height fill ]
            [ text "Item 1"
            , text "Item 2"
            ]
        , el [ width fill, height fill ] <| el [ centerX, centerY ] <| text "Content"
        ]


footer : Element msg
footer =
    row [ explain Debug.todo, width fill ]
        [ el [ alignLeft ] <| text "Left"
        , el [ centerX ] <| text "Center"
        , el [ alignRight ] <| text "Right"
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
