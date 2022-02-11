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
        column [ width fill, height fill ]
            [ header
            , middle
            , footer model
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


footer : Model -> Element msg
footer model =
    row [ explain Debug.todo, width fill ]
        [ el [ alignLeft ] <| text "Left"
        , el [ centerX ] <| text model.flags.mealsUrl
        , el [ alignRight ] <| text "Right"
        ]



---- PROGRAM ----


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
