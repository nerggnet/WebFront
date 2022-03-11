module MenuElements exposing (..)

import Colors as C
import Domain as D
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import ModelMessage as M


menuSelector : M.Model -> Element M.Msg
menuSelector model =
    column
        [ Border.width 1
        , Border.rounded 4
        , paddingXY 2 3
        ]
        [ row []
            [ el [ Font.italic, Border.widthEach { top = 0, bottom = 1, right = 0, left = 0 } ] <| text "Menu name"
            ]
        , el [] <|
            table
                [ scrollbarY
                , spacing 4
                ]
                { data = model.menus
                , columns =
                    [ { header = none
                      , width = maximum 500 fill
                      , view = \menu -> menuListItem menu
                      }
                    ]
                }
        ]


menuListItem : D.Menu -> Element M.Msg
menuListItem menu =
    el [ mouseOver [ Background.color C.mainContentHighlightColor ], Events.onClick (M.DisplayMenuDetails menu) ] <| text menu.name


menuDetails : M.Model -> Element M.Msg
menuDetails model =
    case model.menuToFocus of
        Nothing ->
            Element.none

        Just menu ->
            column
                [ width fill
                , Border.width 1
                , Border.rounded 4
                , paddingXY 2 3
                ]
                [ menuBaseInfo menu
                ]


menuBaseInfo : D.Menu -> Element M.Msg
menuBaseInfo menu =
    column []
        [ el [ Font.size 24 ] <| text menu.name
        ]
