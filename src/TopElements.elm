module TopElements exposing (..)

import Colors as C
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import MenuElements as ME
import ModelMessage as M
import RecipeElements as RE


header : Element M.Msg
header =
    row
        [ Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
        , Border.color C.headerBorderColor
        , Background.color C.headerBackgroundColor
        , paddingXY 5 5
        , Font.size 16
        , width fill
        ]
        [ el [ alignLeft ] <| homeButton
        , el [ centerX, Font.italic ] <| text "Meals by nerggnet"
        , el [ alignRight, Font.size 24 ] <| aboutButton
        ]


homeButton : Element M.Msg
homeButton =
    el
        [ Font.size 24
        , paddingXY 3 3
        , Border.rounded 4
        , mouseOver [ Background.color C.headerHighlightColor ]
        , Events.onClick M.DisplayTitle
        ]
    <|
        text "⟲ Home"


aboutButton : Element M.Msg
aboutButton =
    el
        [ Font.size 24
        , paddingXY 3 3
        , Border.rounded 4
        , mouseOver [ Background.color C.headerHighlightColor ]
        , Events.onClick M.DisplayAbout
        ]
    <|
        text "👀💭"


middle : M.Model -> Element M.Msg
middle model =
    row
        [ paddingXY 0 0
        , spacing 0
        , width fill
        , height fill
        ]
        [ leftList
        , mainContent model
        ]


leftList : Element M.Msg
leftList =
    column
        [ Border.widthEach { top = 0, bottom = 0, left = 0, right = 1 }
        , height fill
        , spacing 4
        , paddingXY 4 4
        , Border.color C.leftListBorderColor
        , Background.color C.leftListBackgroundColor
        , Font.size 20
        ]
        [ listItem "Recipes" M.LoadRecipes
        , listItem "Menus" M.LoadMenus
        ]


listItem : String -> M.Msg -> Element M.Msg
listItem name msg =
    el
        [ width fill
        , Border.width 1
        , Border.color C.leftListItemBorderColor
        , Background.color C.leftListItemBackgroundColor
        , Border.rounded 4
        , paddingXY 3 3
        , pointer
        , mouseOver [ Background.color C.leftListItemHighlightColor ]
        , Events.onClick msg
        ]
    <|
        text name


mainContent : M.Model -> Element M.Msg
mainContent model =
    el
        [ width fill
        , height fill
        , Background.color C.mainContentBackgroundColor
        , Font.size <| model.fontSize
        ]
    <|
        case model.page of
            M.TitlePage ->
                blankPage

            M.RecipesPage ->
                let
                    smallBoxSpacing =
                        px (model.fontSize // 4)

                    bigBoxSpacing =
                        px (model.fontSize // 2)
                in
                column
                    [ width fill ]
                    [ el [ width fill, height smallBoxSpacing ] none
                    , row [ width fill ] [ el [ width smallBoxSpacing ] none, RE.recipeSelector model, el [ width smallBoxSpacing ] none ]
                    , el [ width fill, height bigBoxSpacing ] none
                    , row [ width fill ] [ el [ width smallBoxSpacing ] none, RE.recipeDetails model, el [ width smallBoxSpacing ] none ]
                    , el [ width fill, height bigBoxSpacing ] none
                    , row [ width fill ] [ el [ width smallBoxSpacing ] none, RE.addRecipeInput model, el [ width smallBoxSpacing ] none ]
                    ]

            M.MenusPage ->
                column
                    [ width fill ]
                    [ ME.menuSelector model
                    , ME.menuDetails model
                    ]

            M.AboutPage ->
                column
                    [ width fill, height fill ]
                    [ aboutPage model ]


blankPage : Element M.Msg
blankPage =
    el [] Element.none


aboutPage : M.Model -> Element M.Msg
aboutPage _ =
    column
        [ width fill
        , height fill
        , Border.width 1
        , Border.rounded 4
        , paddingXY 2 3
        ]
        [ el [ paddingEach { top = 0, bottom = 0, left = 5, right = 0 }, Font.size 24 ] <| text "About Meals by nerggnet"
        , el [ paddingEach { top = 6, bottom = 0, left = 0, right = 0 } ] <|
            column [ spacing 5, padding 5 ]
                [ paragraph []
                    [ text "Front in Elm ("
                    , newTabLink [ Font.underline, Font.color C.linkColor ] { url = "https://elm-lang.org/", label = text "elm-lang.org" }
                    , text ") and Elm-UI ("
                    , newTabLink [ Font.underline, Font.color C.linkColor ] { url = "https://github.com/mdgriffith/elm-ui", label = text "github.com/mdgriffith/elm-ui" }
                    , text ")"
                    ]
                , paragraph [] [ text "Backend in F# (", newTabLink [ Font.underline, Font.color C.linkColor ] { url = "https://fsharp.org/", label = text "fsharp.org" }, text ")" ]
                , paragraph [] [ text "Source: ", newTabLink [ Font.underline, Font.color C.linkColor ] { url = "https://github.com/nerggnet/", label = text "github.com/nerggnet" } ]
                ]
        , el [ width fill, height (px 1), Background.color C.mainContentDividerColor ] <| none
        , el [ width (fill |> maximum 800), height (fill |> maximum 239), Background.uncropped "black_transparent_logo.png" ] <| none
        ]


footer : M.Model -> Element M.Msg
footer model =
    row
        [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        , Border.color C.footerBorderColor
        , Background.color C.footerBackgroundColor
        , paddingXY 5 5
        , Font.size 16
        , width fill
        ]
        [ el [ alignLeft, Font.size 24 ] <| text "《"
        , el [ centerX ] <| text model.footerMessage
        , el [ alignRight, Font.size 24 ] <| text "》"
        ]
