module Elements exposing (..)

import Colors as C
import Domain as D
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ModelMessage as M


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
        , mouseOver [ Background.color C.darkerGreyishTealColor ]
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
        , mouseOver [ Background.color C.darkerGreyishTealColor ]
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
        , spacing 2
        , paddingXY 2 2
        , Border.color C.leftListBorderColor
        , Background.color C.leftListBackgroundColor
        , Font.size 18
        ]
        [ listItem "Recipes" M.LoadRecipes
        , listItem "Menus" M.LoadMenus
        ]


mainContent : M.Model -> Element M.Msg
mainContent model =
    el
        [ width fill
        , height fill
        , Border.width 2
        , Border.color C.mainContentBorderColor
        , Background.color C.mainContentBackgroundColor
        ]
    <|
        case model.page of
            M.TitlePage ->
                renderBlankPage

            M.RecipesPage ->
                column
                    [ width fill ]
                    [ renderRecipeSelector model
                    , renderRecipeDetails model
                    , renderAddRecipeInput model
                    ]

            M.MenusPage ->
                column
                    [ width fill ]
                    [ renderMenuSelector model
                    , renderMenuDetails model
                    ]

            M.AboutPage ->
                column
                    [ width fill ]
                    [ renderAboutPage model ]


renderBlankPage : Element M.Msg
renderBlankPage =
    el [] Element.none


renderRecipeSelector : M.Model -> Element M.Msg
renderRecipeSelector model =
    column
        [ Border.width 1
        , Border.rounded 4
        , paddingXY 2 3
        ]
        [ row []
            [ el [ Font.italic, Border.widthEach { top = 0, bottom = 1, right = 0, left = 0 } ] <| text "Recipe name"
            ]
        , el [] <|
            table
                [ scrollbarY
                , spacing 4
                ]
                { data = model.recipes
                , columns =
                    [ { header = none
                      , width = maximum 500 fill
                      , view = \recipe -> renderRecipeListItem recipe
                      }
                    ]
                }
        ]


renderRecipeListItem : D.Recipe -> Element M.Msg
renderRecipeListItem recipe =
    el [ mouseOver [ Background.color C.selectorItemHighlightColor ], Events.onClick (M.DisplayRecipeDetails recipe) ] <| text recipe.name


renderRecipeDetails : M.Model -> Element M.Msg
renderRecipeDetails model =
    case model.recipeToFocus of
        Nothing ->
            Element.none

        Just recipe ->
            column
                [ width fill
                , Border.width 1
                , Border.rounded 4
                , paddingXY 2 3
                ]
                [ renderRecipeBaseInfo recipe
                , renderRecipeIngredients recipe
                , renderRecipeInstructions recipe
                , renderRecipeComments recipe
                ]


renderRecipeBaseInfo : D.Recipe -> Element M.Msg
renderRecipeBaseInfo recipe =
    let
        recipeLink =
            case recipe.link of
                Just link ->
                    link

                Nothing ->
                    "#"
    in
    column []
        [ el [ Font.size 24 ] <| text recipe.name
        , el [] <| text <| "No. of portions: " ++ String.fromInt recipe.portions
        , el [] <| newTabLink [ Font.underline, Font.color C.linkColor ] { url = recipeLink, label = text "Recipe link" }
        ]


renderRecipeIngredients : D.Recipe -> Element M.Msg
renderRecipeIngredients _ =
    Element.none


renderRecipeInstructions : D.Recipe -> Element M.Msg
renderRecipeInstructions _ =
    Element.none


renderRecipeComments : D.Recipe -> Element M.Msg
renderRecipeComments _ =
    Element.none


renderAddRecipeInput : M.Model -> Element M.Msg
renderAddRecipeInput model =
    el
        [ width fill
        , Border.width 1
        , Border.rounded 4
        , paddingXY 2 2
        ]
    <|
        Input.text [ width <| maximum 300 fill ]
            { onChange = M.UserTypedText
            , text = model.userText
            , placeholder = Just <| Input.placeholder [] <| text "Type here"
            , label = Input.labelAbove [] <| text "Text input"
            }


renderMenuSelector : M.Model -> Element M.Msg
renderMenuSelector model =
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
                      , view = \menu -> renderMenuListItem menu
                      }
                    ]
                }
        ]


renderMenuListItem : D.Menu -> Element M.Msg
renderMenuListItem menu =
    el [ mouseOver [ Background.color C.selectorItemHighlightColor ], Events.onClick (M.DisplayMenuDetails menu) ] <| text menu.name


renderMenuDetails : M.Model -> Element M.Msg
renderMenuDetails model =
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
                [ renderMenuBaseInfo menu
                ]


renderMenuBaseInfo : D.Menu -> Element M.Msg
renderMenuBaseInfo menu =
    column []
        [ el [ Font.size 24 ] <| text menu.name
        ]


renderAboutPage : M.Model -> Element M.Msg
renderAboutPage _ =
    column
        [ width fill
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
