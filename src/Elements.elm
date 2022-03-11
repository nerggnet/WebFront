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
                    , row [ width fill ] [ el [ width smallBoxSpacing ] none, recipeSelector model, el [ width smallBoxSpacing ] none ]
                    , el [ width fill, height bigBoxSpacing ] none
                    , row [ width fill ] [ el [ width smallBoxSpacing ] none, recipeDetails model, el [ width smallBoxSpacing ] none ]
                    , el [ width fill, height bigBoxSpacing ] none
                    , row [ width fill ] [ el [ width smallBoxSpacing ] none, addRecipeInput model, el [ width smallBoxSpacing ] none ]
                    ]

            M.MenusPage ->
                column
                    [ width fill ]
                    [ menuSelector model
                    , menuDetails model
                    ]

            M.AboutPage ->
                column
                    [ width fill, height fill ]
                    [ aboutPage model ]


blankPage : Element M.Msg
blankPage =
    el [] Element.none


recipeSelector : M.Model -> Element M.Msg
recipeSelector model =
    column
        [ width fill
        , Background.color C.mainContentSectionBackgroundColor
        , Border.rounded 6
        , paddingXY 5 3
        ]
        [ row []
            [ el [ Font.italic, Font.underline ] <| text "Recipe name"
            ]
        , el [] <|
            table
                [ scrollbarY
                , spacing 4
                , paddingEach { top = 2, bottom = 4, left = 0, right = 0 }
                ]
                { data = model.recipes
                , columns =
                    [ { header = none
                      , width = fill
                      , view = \recipe -> recipeListItem recipe
                      }
                    ]
                }
        ]


recipeListItem : D.Recipe -> Element M.Msg
recipeListItem recipe =
    el [ mouseOver [ Background.color C.mainContentHighlightColor ], Events.onClick (M.DisplayRecipeDetails recipe) ] <| text recipe.name


recipeDetails : M.Model -> Element M.Msg
recipeDetails model =
    case model.recipeToFocus of
        Nothing ->
            Element.none

        Just recipe ->
            column
                [ width fill
                , Background.color C.mainContentSectionBackgroundColor
                , Border.rounded 6
                , paddingXY 5 3
                ]
                [ recipeBaseInfo model recipe
                , recipeIngredients model recipe
                , recipeInstructions model recipe
                , recipeComments model recipe
                ]


recipeBaseInfo : M.Model -> D.Recipe -> Element M.Msg
recipeBaseInfo model recipe =
    let
        recipeLink =
            case recipe.link of
                Just link ->
                    link

                Nothing ->
                    "#"

        headerFontSize =
            round <| toFloat model.fontSize * 1.3
        infoFontSize =
            round <| toFloat model.fontSize * 0.7
    in
    column [ width fill ]
        [ paragraph [ width fill ] [ el [ Font.size headerFontSize ] <| text recipe.name ]
        , paragraph [ width fill ]
            [ el [ Font.size infoFontSize ] <| text <| "No. of portions: " ++ String.fromInt recipe.portions
            , el [ width fill, paddingXY (model.fontSize // 3) 0 ] <| el [ width (px 1), height (px (model.fontSize // 2)), Background.color C.mainContentDividerColor ] none
            , el [ Font.size infoFontSize ] <| newTabLink [ Font.underline, Font.color C.linkColor ] { url = recipeLink, label = text "Recipe link" }
            ]
        ]


recipeIngredients : M.Model -> D.Recipe -> Element M.Msg
recipeIngredients model recipe =
    let
        headerFontSize =
            round <| toFloat model.fontSize * 0.7
    in
    column [ width fill ]
        [ el [ Font.bold, Font.size headerFontSize, paddingEach { top = model.fontSize // 2, bottom = 0, left = 0, right = 0 } ] <| text "Ingredients"
        , el [ width fill, paddingXY 0 (model.fontSize // 16) ] <| el [ width fill, height (px 1), Background.color C.mainContentDividerColor ] none
        , el [ width fill ] <|
            table
                [ scrollbarY
                , spacingXY 15 4
                , paddingEach { top = 0, bottom = 4, left = 0, right = 0 }
                ]
                { data = recipe.ingredients
                , columns =
                    [ { header = el [ Font.italic, Font.underline ] <| text "Product name"
                      , width = fillPortion 4
                      , view = \ingredient -> ingredientTableItemProductName ingredient
                      }
                    , { header = el [ Font.italic, Font.underline ] <| text "Quantity"
                      , width = fillPortion 1
                      , view = \ingredient -> ingredientTableItemQuantityAmount model ingredient
                      }
                    , { header = el [ Font.italic, Font.underline ] <| text "Unit"
                      , width = fillPortion 1
                      , view = \ingredient -> ingredientTableItemQuantityUnit ingredient
                      }
                    ]
                }
        ]


ingredientTableItemProductName : D.Ingredient -> Element M.Msg
ingredientTableItemProductName ingredient =
    el
        [ mouseOver [ Background.color C.mainContentHighlightColor ]

        --, Events.onClick (M.DisplayRecipeDetails recipe)
        ]
    <|
        text ingredient.product.name


ingredientTableItemQuantityAmount : M.Model -> D.Ingredient -> Element M.Msg
ingredientTableItemQuantityAmount _ ingredient =
    el
        [ mouseOver [ Background.color C.mainContentHighlightColor ]

        --, Events.onClick (M.DisplayRecipeDetails recipe)
        ]
    <|
        el [] <|
            text <|
                String.fromFloat ingredient.quantity.amount


ingredientTableItemQuantityUnit : D.Ingredient -> Element M.Msg
ingredientTableItemQuantityUnit ingredient =
    el
        [ mouseOver [ Background.color C.mainContentHighlightColor ]

        --, Events.onClick (M.DisplayRecipeDetails recipe)
        ]
    <|
        text <|
            D.stringFromRecipeUnit ingredient.quantity.unit


recipeInstructions : M.Model -> D.Recipe -> Element M.Msg
recipeInstructions model recipe =
    let
        headerFontSize =
            round <| toFloat model.fontSize * 0.7
    in
    column [ width fill ]
        [ el [ Font.bold, Font.size headerFontSize, paddingEach { top = model.fontSize // 2, bottom = 0, left = 0, right = 0 } ] <| text "Instructions"
        , el [ width fill, paddingXY 0 (model.fontSize // 16) ] <| el [ width fill, height (px 1), Background.color C.mainContentDividerColor ] none
        , el [ width fill ] <|
            table
                [ scrollbarY
                , spacingXY 15 4
                , paddingEach { top = 0, bottom = 4, left = 0, right = 0 }
                ]
                { data = recipe.instructions
                , columns =
                    [ { header = none
                      , width = fill
                      , view = \instruction -> instructionTableItem model instruction
                      }
                    ]
                }
        ]


instructionTableItem : M.Model -> D.Instruction -> Element M.Msg
instructionTableItem _ instruction =
    paragraph
        [ mouseOver [ Background.color C.mainContentHighlightColor ]

        --, Events.onClick (M.DisplayRecipeDetails recipe)
        ]
        [ el [] <|
            text "• "
        , el [] <|
            text instruction.instruction
        ]


recipeComments : M.Model -> D.Recipe -> Element M.Msg
recipeComments model recipe =
    let
        headerFontSize =
            round <| toFloat model.fontSize * 0.7
    in
    column [ width fill ]
        [ el [ Font.bold, Font.size headerFontSize, paddingEach { top = model.fontSize // 2, bottom = 0, left = 0, right = 0 } ] <| text "Comments"
        , el [ width fill, paddingXY 0 (model.fontSize // 16) ] <| el [ width fill, height (px 1), Background.color C.mainContentDividerColor ] none
        , el [ width fill ] <|
            table
                [ scrollbarY
                , spacingXY 15 4
                , paddingEach { top = 0, bottom = 4, left = 0, right = 0 }
                ]
                { data = recipe.comments
                , columns =
                    [ { header = none
                      , width = fill
                      , view = \comment -> commentTableItem model comment
                      }
                    ]
                }
        ]


commentTableItem : M.Model -> D.Comment -> Element M.Msg
commentTableItem _ comment =
    paragraph
        [ mouseOver [ Background.color C.mainContentHighlightColor ]

        --, Events.onClick (M.DisplayRecipeDetails recipe)
        ]
        [ el [] <|
            text "• "
        , el [] <|
            text comment.comment
        ]


addRecipeInput : M.Model -> Element M.Msg
addRecipeInput model =
    el
        [ width fill
        , Background.color C.mainContentSectionBackgroundColor
        , Border.rounded 6
        , paddingXY 5 3
        ]
    <|
        Input.text [ width <| maximum 300 fill ]
            { onChange = M.UserTypedText
            , text = model.userText
            , placeholder = Just <| Input.placeholder [] <| text "Type here"
            , label = Input.labelAbove [] <| text "Text input"
            }


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
