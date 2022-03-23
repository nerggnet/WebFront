module RecipeElements exposing (recipesPage)

import Colors as C
import CommonElements as CE
import Domain as D
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ModelMessage as M


recipesPage : M.Model -> Element M.Msg
recipesPage model =
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
        , addIngredientInput model
        ]


recipeSelector : M.Model -> Element M.Msg
recipeSelector model =
    column
        [ width fill
        , Background.color C.mainContentSectionBackgroundColor
        , Border.rounded 6
        , paddingXY 5 3
        ]
        [ row []
            [ el [ Font.italic, Font.underline ] <| text "Recipes"
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
    el [ mouseOver [ Background.color C.mainContentHighlightColor ], pointer, Events.onClick (M.DisplayRecipeDetails recipe) ] <| text recipe.name


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
            , el [ width fill, paddingXY (model.fontSize // 3) 0 ] <| el [ width (px 1), height (px (model.fontSize // 2)), Background.color C.mainContentDividerColor ] none
            , el [] <| CE.editButton model M.ChangeRecipeBaseInfo
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
                    [ { header = el [ Font.italic, Font.underline ] <| text "Product"
                      , width = fillPortion 20
                      , view = \ingredient -> ingredientTableItemProductName ingredient
                      }
                    , { header = el [ Font.italic, Font.underline ] <| text "Quantity"
                      , width = fillPortion 4
                      , view = \ingredient -> ingredientTableItemQuantityAmount model ingredient
                      }
                    , { header = el [ Font.italic, Font.underline ] <| text "Unit"
                      , width = fillPortion 4
                      , view = \ingredient -> ingredientTableItemQuantityUnit ingredient
                      }
                    , { header = CE.addButton model M.DisplayAddIngredientToRecipe
                      , width = fillPortion 1
                      , view = \ingredient -> CE.editButton model <| M.ChangeRecipeIngredient ingredient
                      }
                    ]
                }
        ]


ingredientTableItemProductName : D.Ingredient -> Element M.Msg
ingredientTableItemProductName ingredient =
    el [] <| text ingredient.product.name


ingredientTableItemQuantityAmount : M.Model -> D.Ingredient -> Element M.Msg
ingredientTableItemQuantityAmount _ ingredient =
    el [] <| text <| String.fromFloat ingredient.quantity.amount


ingredientTableItemQuantityUnit : D.Ingredient -> Element M.Msg
ingredientTableItemQuantityUnit ingredient =
    el [] <| text <| D.stringFromRecipeUnit ingredient.quantity.unit


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
    paragraph []
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
    paragraph []
        [ el [] <|
            text "• "
        , el [] <|
            text comment.comment
        ]


addIngredientInput : M.Model -> Element M.Msg
addIngredientInput model =
    if model.showAddIngredientInput then
        let
            smallBoxSpacing =
                px (model.fontSize // 4)

            bigBoxSpacing =
                px (model.fontSize // 2)
        in
        column [ width fill ]
            [ el [ width fill, height bigBoxSpacing ] none
            , row [ width fill ]
                [ el [ width smallBoxSpacing ] none
                , el
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
                , el [ width smallBoxSpacing ] none
                ]
            ]

    else
        Element.none
