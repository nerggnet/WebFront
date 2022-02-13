module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode
import Json.Encode



---- MODEL ----


type alias Model =
    { page : Page
    , recipes : List Recipe
    , books : List Book
    , recipeNameToFind : String
    , recipeToInsert : Maybe Recipe
    , booksByAuthorToFind : String
    , bookToInsert : Maybe Book
    , userText : String
    , flags : Flags
    }


type Page
    = BlankPage
    | RecipesPage
    | BooksPage


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


emptyFlags : Flags
emptyFlags =
    { environment = "", mealsUrl = "", booksUrl = "" }


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
      , books = []
      , recipeNameToFind = ""
      , recipeToInsert = Nothing
      , booksByAuthorToFind = ""
      , bookToInsert = Nothing
      , userText = ""
      , flags = decodedFlags
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
    | FindRecipesExecute
    | GotRecipes (Result Http.Error (List Recipe))
    | InsertRecipe Recipe
    | InsertRecipeExecute
    | GotInsertRecipeResponse (Result Http.Error (List Recipe))
    | FindBooks String
    | GotBooks (Result Http.Error (List Book))
    | InsertBook Book
    | GotInsertBookResponse (Result Http.Error (List Book))
    | LoadRecipes
    | LoadRecipesExecute
    | LoadBooks
    | LoadBooksExecute
    | DisplayRecipes
    | DisplayBooks
    | UserTypedText String


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


getAllRecipesEncoder : Json.Encode.Value
getAllRecipesEncoder =
    Json.Encode.object
        [ ( "Action", Json.Encode.string "Find" )
        ]


insertRecipeEncoder : Recipe -> Json.Encode.Value
insertRecipeEncoder recipe =
    Json.Encode.object
        [ ( "Action", Json.Encode.string "Insert" )
        , ( "Name", Json.Encode.string recipe.name )
        , ( "Link", Json.Encode.string recipe.link )
        , ( "Portions", Json.Encode.int recipe.portions )
        ]


bookDecoder : Json.Decode.Decoder Book
bookDecoder =
    Json.Decode.map3 Book
        (Json.Decode.field "Author" Json.Decode.string)
        (Json.Decode.field "Title" Json.Decode.string)
        (Json.Decode.field "IsFavorite" Json.Decode.bool)


bookListDecoder : Json.Decode.Decoder (List Book)
bookListDecoder =
    Json.Decode.list bookDecoder


getAllBooksEncoder : Json.Encode.Value
getAllBooksEncoder =
    Json.Encode.object
        [ ( "Action", Json.Encode.string "Find" )
        ]


insertBookEncoder : Book -> Json.Encode.Value
insertBookEncoder book =
    Json.Encode.object
        [ ( "Action", Json.Encode.string "Insert" )
        , ( "Auhtor", Json.Encode.string book.author )
        , ( "Title", Json.Encode.string book.title )
        , ( "IsFavorite", Json.Encode.bool book.isFavorite )
        ]


postFindRecipes : Model -> Cmd Msg
postFindRecipes model =
    Http.post
        { url = model.flags.mealsUrl
        , body = Http.jsonBody <| findRecipeEncoder model.recipeNameToFind
        , expect = Http.expectJson GotRecipes recipeListDecoder
        }


postGetAllRecipes : Model -> Cmd Msg
postGetAllRecipes model =
    Http.post
        { url = model.flags.mealsUrl
        , body = Http.jsonBody <| getAllRecipesEncoder
        , expect = Http.expectJson GotRecipes recipeListDecoder
        }


postInsertRecipe : Model -> Cmd Msg
postInsertRecipe model =
    case model.recipeToInsert of
        Just recipe ->
            Http.post
                { url = model.flags.mealsUrl
                , body = Http.jsonBody <| insertRecipeEncoder recipe
                , expect = Http.expectJson GotInsertRecipeResponse recipeListDecoder
                }

        Nothing ->
            Cmd.none


postGetAllBooks : Model -> Cmd Msg
postGetAllBooks model =
    Http.post
        { url = model.flags.booksUrl
        , body = Http.jsonBody <| getAllBooksEncoder
        , expect = Http.expectJson GotBooks bookListDecoder
        }


postInsertBook : Model -> Cmd Msg
postInsertBook model =
    case model.bookToInsert of
        Just book ->
            Http.post
                { url = model.flags.booksUrl
                , body = Http.jsonBody <| insertBookEncoder book
                , expect = Http.expectJson GotInsertBookResponse bookListDecoder
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
                Ok recipes ->
                    update DisplayRecipes { model | recipes = recipes }

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

        FindBooks name ->
            ( { model | booksByAuthorToFind = name }, Cmd.none )

        GotBooks result ->
            case result of
                Ok books ->
                    update DisplayBooks { model | books = books }

                Err _ ->
                    ( { model | books = [] }, Cmd.none )

        LoadBooks ->
            update LoadBooksExecute { model | booksByAuthorToFind = "" }

        LoadBooksExecute ->
            ( model, postGetAllBooks model )

        DisplayBooks ->
            ( { model | page = BooksPage }, Cmd.none )

        InsertBook book ->
            ( { model | bookToInsert = Just book }, postInsertBook model )

        GotInsertBookResponse result ->
            case result of
                Ok _ ->
                    update DisplayBooks model

                Err _ ->
                    update DisplayBooks { model | books = [] }

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
        , listItem "Books" LoadBooks
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

            BooksPage ->
                renderTableOfBooks model


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
                        paragraph [] [ text recipe.link ]
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


renderTableOfBooks : Model -> Element Msg
renderTableOfBooks model =
    table
        [ centerX
        , centerY
        , Font.color darkerMagentaColor
        , Border.width 1
        , Border.rounded 4
        , paddingXY 2 2
        ]
        { data = model.books
        , columns =
            [ { header = el [ Font.italic, Font.underline ] <| text "Author"
              , width = fillPortion 2
              , view =
                    \book ->
                        text book.author
              }
            , { header = el [ Font.italic, Font.underline ] <| text "Title"
              , width = fillPortion 3
              , view =
                    \book ->
                        text book.title
              }
            ]
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
