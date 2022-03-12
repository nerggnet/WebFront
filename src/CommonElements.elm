module CommonElements exposing (..)

import Colors as C
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import ModelMessage as M


editButton : M.Model -> M.Msg -> Element M.Msg
editButton model msg =
    el
        [ Font.size <| round <| toFloat model.fontSize * 0.7
        , paddingXY 3 3
        , Border.rounded 4
        , pointer
        , mouseOver [ Background.color C.headerHighlightColor ]
        , Events.onClick msg
        ]
    <|
        text "📝"


addButton : M.Model -> M.Msg -> Element M.Msg
addButton model msg =
    el
        [ Font.size <| round <| toFloat model.fontSize * 0.7
        , paddingXY 3 3
        , Border.rounded 4
        , pointer
        , mouseOver [ Background.color C.headerHighlightColor ]
        , Events.onClick msg
        ]
    <|
        text "➕"
