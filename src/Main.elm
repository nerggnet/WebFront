module Main exposing (main)

--import Json.Encode

import Browser
import Domain
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import HttpJsonController
import Json.Decode



---- MODEL ----


type alias Model =
    { page : Page
    , recipes : List Domain.Recipe
    , recipeNameToFind : String
    , recipeToInsert : Maybe Domain.Recipe
    , userText : String
    , flags : Flags
    }


type Page
    = BlankPage
    | RecipesPage


type alias Flags =
    { environment : String, mealsUrl : String }


emptyFlags : Flags
emptyFlags =
    { environment = "", mealsUrl = "" }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decodedFlags =
            case Json.Decode.decodeValue flagsDecoder flags of
                Ok okFlags ->
                    okFlags

                Err _ ->
                    emptyFlags
    in
    ( { page = BlankPage
      , recipes = []
      , recipeNameToFind = ""
      , recipeToInsert = Nothing
      , userText = ""
      , flags = decodedFlags
      }
    , Cmd.none
    )


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map2 Flags
        (Json.Decode.field "environment" Json.Decode.string)
        (Json.Decode.field "mealsUrl" Json.Decode.string)



---- UPDATE ----


type Msg
    = FindRecipes String
    | FindRecipesExecute
    | GotRecipes (Result Http.Error HttpJsonController.ResponseJson)
    | InsertRecipe Domain.Recipe
    | InsertRecipeExecute
    | GotInsertRecipeResponse (Result Http.Error HttpJsonController.ResponseJson)
    | LoadRecipes
    | LoadRecipesExecute
    | DisplayRecipes
    | UserTypedText String


postFindRecipes : Model -> Cmd Msg
postFindRecipes model =
    Http.post
        { url = model.flags.mealsUrl
        , body = Http.jsonBody <| HttpJsonController.findRecipeEncoder model.recipeNameToFind
        , expect = Http.expectJson GotRecipes HttpJsonController.responseDecoder
        }


postGetAllRecipes : Model -> Cmd Msg
postGetAllRecipes model =
    Http.post
        { url = model.flags.mealsUrl
        , body = Http.jsonBody <| HttpJsonController.getAllRecipesEncoder
        , expect = Http.expectJson GotRecipes HttpJsonController.responseDecoder
        }


postInsertRecipe : Model -> Cmd Msg
postInsertRecipe model =
    case model.recipeToInsert of
        Just recipe ->
            Http.post
                { url = model.flags.mealsUrl
                , body = Http.jsonBody <| HttpJsonController.insertRecipeEncoder recipe
                , expect = Http.expectJson GotInsertRecipeResponse HttpJsonController.responseDecoder
                }

        Nothing ->
            Cmd.none



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FindRecipes name ->
            update FindRecipesExecute { model | recipeNameToFind = name }

        FindRecipesExecute ->
            ( model, postFindRecipes model )

        InsertRecipe recipe ->
            update InsertRecipeExecute { model | recipeToInsert = Just recipe }

        InsertRecipeExecute ->
            ( model, postInsertRecipe model )

        GotRecipes result ->
            case result of
                Ok response ->
                    update DisplayRecipes { model | recipes = response.recipes }

                Err _ ->
                    ( { model | recipes = [] }, Cmd.none )

        GotInsertRecipeResponse result ->
            case result of
                Ok _ ->
                    update DisplayRecipes model

                Err _ ->
                    update DisplayRecipes { model | recipes = [] }

        LoadRecipes ->
            update LoadRecipesExecute { model | recipeNameToFind = "" }

        LoadRecipesExecute ->
            ( model, postGetAllRecipes model )

        DisplayRecipes ->
            ( { model | page = RecipesPage }, Cmd.none )

        UserTypedText text ->
            ( { model | userText = text }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        column
            [ width fill
            , height fill
            , spacing -1
            ]
            [ header
            , middle model
            , footer model
            ]


header : Element Msg
header =
    row
        [ Border.width 1
        , Border.color darkerGreenishColor
        , Background.color greyishTealColor
        , paddingXY 5 5
        , Font.size 16
        , width fill
        ]
        [ el [ alignLeft ] <| text "Left"
        , el [ centerX ] <| text "Center"
        , el [ alignRight ] <| text "Right"
        ]


middle : Model -> Element Msg
middle model =
    row
        [ paddingXY 0 0
        , spacing -1
        , width fill
        , height fill
        ]
        [ leftList
        , mainContent model
        ]


leftList : Element Msg
leftList =
    column
        [ Border.width 1
        , height fill
        , spacing 2
        , paddingXY 2 2
        , Border.color greenishColor
        , Background.color magentaColor
        , Font.size 18
        ]
        [ listItem "Recipes" LoadRecipes
        ]


mainContent : Model -> Element Msg
mainContent model =
    el
        [ width fill
        , height fill
        , Border.width 1
        , Border.color greenishColor
        , Background.color darkerGreenishColor
        ]
    <|
        case model.page of
            BlankPage ->
                renderBlankPage

            RecipesPage ->
                column
                    [ width fill ]
                    [ renderTableOfRecipes model
                    , renderAddRecipeInput model
                    ]


renderBlankPage : Element Msg
renderBlankPage =
    el [] Element.none


renderTableOfRecipes : Model -> Element Msg
renderTableOfRecipes model =
    table
        [ width fill
        , centerX
        , centerY
        , Font.color darkerMagentaColor
        , Border.width 1
        , Border.rounded 4
        , paddingXY 2 2
        , spacing 4
        ]
        { data = model.recipes
        , columns =
            [ { header = el [ Font.italic, Font.underline ] <| text "Name"
              , width = fillPortion 3
              , view =
                    \recipe ->
                        paragraph [] [ text recipe.name ]
              }
            , { header = el [ Font.italic, Font.underline ] <| text "Link"
              , width = fillPortion 3
              , view =
                    \recipe ->
                        paragraph []
                            [ text
                                (case recipe.link of
                                    Just link ->
                                        link

                                    Nothing ->
                                        ""
                                )
                            ]
              }
            , { header = el [ Font.italic, Font.underline ] <| text "Portions"
              , width = fillPortion 1
              , view =
                    \recipe ->
                        paragraph [] [ text <| String.fromInt recipe.portions ]
              }
            ]
        }


renderAddRecipeInput : Model -> Element Msg
renderAddRecipeInput model =
    el
        [ width fill
        , Font.color darkerMagentaColor
        , Border.width 1
        , Border.rounded 4
        , paddingXY 2 2
        ]
    <|
        Input.text [ width <| maximum 300 fill ]
            { onChange = UserTypedText
            , text = model.userText
            , placeholder = Just <| Input.placeholder [] <| text "Type here"
            , label = Input.labelAbove [] <| text "Text input"
            }


listItem : String -> Msg -> Element Msg
listItem name msg =
    el
        [ width fill
        , Border.width 1
        , Border.color brightMagentaColor
        , Background.color slightlyBrighterMagentaColor
        , Border.rounded 4
        , paddingXY 3 3
        , pointer
        , mouseOver [ Background.color brightestMagentaColor ]
        , Events.onClick msg
        ]
    <|
        text name


footer : Model -> Element Msg
footer _ =
    row
        [ Border.width 1
        , Border.color evenDarkerGreenishColor
        , Background.color greyishTealColor
        , paddingXY 5 5
        , Font.size 16
        , width fill
        ]
        [ el [ alignLeft ] <| text "Left"
        , el [ centerX ] <| text "Center"
        , el [ alignRight ] <| text "Right"
        ]



---- COLORS ----


greyishTealColor : Color
greyishTealColor =
    rgb255 160 190 190


magentaColor : Color
magentaColor =
    rgb255 190 160 190


darkerMagentaColor : Color
darkerMagentaColor =
    rgb255 80 50 80


slightlyBrighterMagentaColor : Color
slightlyBrighterMagentaColor =
    rgb255 200 170 200


brightMagentaColor : Color
brightMagentaColor =
    rgb255 210 180 210


brightestMagentaColor : Color
brightestMagentaColor =
    rgb255 240 210 240


greenishColor : Color
greenishColor =
    rgb255 160 190 160


darkerGreenishColor : Color
darkerGreenishColor =
    rgb255 150 180 150


evenDarkerGreenishColor : Color
evenDarkerGreenishColor =
    rgb255 120 140 120



---- PROGRAM ----


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
