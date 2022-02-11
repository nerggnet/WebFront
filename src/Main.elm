module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode
import Json.Encode



---- MODEL ----


type alias Model =
    { recipes : List Recipe
    , books : List Book
    , recipeNameToFind : String
    , recipeToInsert : Maybe Recipe
    , flags : Flags
    }


type alias Recipe =
    { name : String
    , link : String
    , portions : Int
    }


type alias Book =
    { author : String
    , title : String
    , isFavorite : Bool
    }


type alias Flags =
    { environment : String, mealsUrl : String, booksUrl : String }


emptyFlags =
    { environment = "", mealsUrl = "", booksUrl = "" }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( case Json.Decode.decodeValue flagsDecoder flags of
        Ok decodedFlags ->
            { recipes = []
            , books = []
            , recipeNameToFind = ""
            , recipeToInsert = Nothing
            , flags = decodedFlags
            }

        Err _ ->
            { recipes = []
            , books = []
            , recipeNameToFind = ""
            , recipeToInsert = Nothing
            , flags = emptyFlags
            }
    , Cmd.none
    )


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map3 Flags
        (Json.Decode.field "environment" Json.Decode.string)
        (Json.Decode.field "mealsUrl" Json.Decode.string)
        (Json.Decode.field "booksUrl" Json.Decode.string)



---- UPDATE ----


type Msg
    = FindRecipes String
    | InsertRecipe Recipe
    | FindBooks String
    | InsertBook Book
    | GotRecipes (Result Http.Error (List Recipe))
    | GotInsertResponse (Result Http.Error (List Recipe))


recipeDecoder : Json.Decode.Decoder Recipe
recipeDecoder =
    Json.Decode.map3 Recipe
        (Json.Decode.field "Name" Json.Decode.string)
        (Json.Decode.field "Link" Json.Decode.string)
        (Json.Decode.field "Portions" Json.Decode.int)


recipeListDecoder : Json.Decode.Decoder (List Recipe)
recipeListDecoder =
    Json.Decode.list recipeDecoder


findRecipeEncoder : String -> Json.Encode.Value
findRecipeEncoder name =
    Json.Encode.object
        [ ( "Action", Json.Encode.string "Find" )
        , ( "Name", Json.Encode.string name )
        ]


insertRecipeEncoder : Recipe -> Json.Encode.Value
insertRecipeEncoder recipe =
    Json.Encode.object
        [ ( "Action", Json.Encode.string "Insert" )
        , ( "Name", Json.Encode.string recipe.name )
        , ( "Link", Json.Encode.string recipe.link )
        , ( "Portions", Json.Encode.int recipe.portions )
        ]


postFindRecipes : Model -> Cmd Msg
postFindRecipes model =
    Http.post
        { url = model.flags.mealsUrl
        , body = Http.jsonBody <| findRecipeEncoder model.recipeNameToFind
        , expect = Http.expectJson GotRecipes recipeListDecoder
        }


postInsertRecipe : Model -> Cmd Msg
postInsertRecipe model =
    case model.recipeToInsert of
        Just recipe ->
            Http.post
                { url = model.flags.mealsUrl
                , body = Http.jsonBody <| insertRecipeEncoder recipe
                , expect = Http.expectJson GotInsertResponse recipeListDecoder
                }

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FindRecipes name ->
            ( { model | recipeNameToFind = name }, postFindRecipes model )

        InsertRecipe recipe ->
            ( { model | recipeToInsert = Just recipe }, postInsertRecipe model )

        GotRecipes result ->
            case result of
                Ok recipes ->
                    ( { model | recipes = recipes }, Cmd.none )

                Err _ ->
                    ( { model | recipes = [] }, Cmd.none )

        _ ->
            ( model, Cmd.none )



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
            , middle
            , footer model
            ]


header : Element msg
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


middle : Element msg
middle =
    row
        [ paddingXY 0 0
        , spacing -1
        , width fill
        , height fill
        ]
        [ leftList
        , mainContent
        ]


leftList : Element msg
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
        [ listItem "Recipes"
        , listItem "Books"
        ]


mainContent : Element msg
mainContent =
    el
        [ width fill
        , height fill
        , Border.width 1
        , Border.color greenishColor
        , Background.color darkerGreenishColor
        ]
    <|
        el
            [ centerX
            , centerY
            ]
        <|
            text "Content"


listItem : String -> Element msg
listItem name =
    el
        [ width fill
        , Border.width 1
        , Border.color brightMagentaColor
        , Background.color slightlyBrighterMagentaColor
        , Border.rounded 4
        , paddingXY 3 3
        , pointer
        , mouseOver [ Background.color brightestMagentaColor ]
        ]
    <|
        text name


footer : Model -> Element msg
footer model =
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


greyishTealColor =
    rgb255 160 190 190


magentaColor =
    rgb255 190 160 190


darkerMagentaColor =
    rgb255 180 150 180


slightlyBrighterMagentaColor =
    rgb255 200 170 200


brightMagentaColor =
    rgb255 210 180 210


brightestMagentaColor =
    rgb255 240 210 240


greenishColor =
    rgb255 160 190 160


darkerGreenishColor =
    rgb255 150 180 150


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
