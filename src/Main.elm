module Main exposing (main)

import Browser
import Colors as C
import Domain as D
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
    , recipes : List D.Recipe
    , recipeNameToFind : D.RecipeName
    , recipeToInsert : Maybe D.Recipe
    , recipeToFocus : Maybe D.Recipe
    , userText : String
    , footerMessage : String
    , flags : Flags
    }


type Page
    = TitlePage
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
    ( { page = TitlePage
      , recipes = []
      , recipeNameToFind = ""
      , recipeToInsert = Nothing
      , recipeToFocus = Nothing
      , userText = ""
      , footerMessage = ""
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
    = DisplayTitle
    | FindRecipes String
    | FindRecipesExecute
    | GotRecipes (Result Http.Error HttpJsonController.ResponseJson)
    | InsertRecipe D.Recipe
    | InsertRecipeExecute
    | GotInsertRecipeResponse (Result Http.Error HttpJsonController.ResponseJson)
    | LoadRecipes
    | LoadRecipesExecute
    | DisplayRecipes
    | DisplayRecipeDetails D.Recipe
    | UserTypedText String


expectJson_ : (Result Http.Error a -> msg) -> Json.Decode.Decoder a -> Http.Expect msg
expectJson_ toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err _ ->
                            Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Json.Decode.errorToString err))


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
        , expect = expectJson_ GotRecipes HttpJsonController.responseDecoder
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
        DisplayTitle ->
            ( { model | page = TitlePage, recipeToFocus = Nothing }, Cmd.none )

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
                    let
                        newFooterMessage =
                            case response.message of
                                Just message ->
                                    message

                                Nothing ->
                                    ""
                    in
                    update DisplayRecipes { model | recipes = response.recipes, footerMessage = newFooterMessage }

                Err httpError ->
                    case httpError of
                        Http.BadBody badBodyMsg ->
                            ( { model | recipes = [], footerMessage = badBodyMsg }, Cmd.none )

                        _ ->
                            ( { model | recipes = [], footerMessage = "Unknown Error" }, Cmd.none )

        GotInsertRecipeResponse result ->
            case result of
                Ok response ->
                    let
                        newFooterMessage =
                            case response.message of
                                Just message ->
                                    message

                                Nothing ->
                                    ""
                    in
                    update DisplayRecipes { model | footerMessage = newFooterMessage }

                Err httpError ->
                    case httpError of
                        Http.BadBody badBodyMsg ->
                            update DisplayRecipes { model | recipes = [], footerMessage = badBodyMsg }

                        _ ->
                            update DisplayRecipes { model | recipes = [], footerMessage = "Unknown Error" }

        LoadRecipes ->
            update LoadRecipesExecute { model | recipeNameToFind = "" }

        LoadRecipesExecute ->
            ( model, postGetAllRecipes model )

        DisplayRecipes ->
            ( { model | page = RecipesPage }, Cmd.none )

        DisplayRecipeDetails recipe ->
            ( { model | recipeToFocus = Just recipe }, Cmd.none )

        UserTypedText text ->
            ( { model | userText = text }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        column
            [ width fill
            , height fill
            , spacing 0
            ]
            [ header
            , middle model
            , footer model
            ]


header : Element Msg
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
        , el [ centerX ] <| text "Center"
        , el [ alignRight ] <| text "Right"
        ]


homeButton : Element Msg
homeButton =
    el
        [ Font.size 20
        , paddingXY 3 3
        , Border.rounded 4
        , mouseOver [ Background.color C.darkerGreyishTealColor ]
        , Events.onClick DisplayTitle
        ]
    <|
        text "Home"


middle : Model -> Element Msg
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


leftList : Element Msg
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
        [ listItem "Recipes" LoadRecipes
        ]


mainContent : Model -> Element Msg
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
            TitlePage ->
                renderBlankPage

            RecipesPage ->
                column
                    [ width fill ]
                    [ renderRecipeSelector model
                    , renderRecipeDetails model
                    , renderAddRecipeInput model
                    ]


renderBlankPage : Element Msg
renderBlankPage =
    el [] Element.none


renderRecipeSelector : Model -> Element Msg
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


renderRecipeListItem : D.Recipe -> Element Msg
renderRecipeListItem recipe =
    el [ mouseOver [ Background.color C.recipeSelectorItemHighlightColor ], Events.onClick (DisplayRecipeDetails recipe) ] <| text recipe.name


renderRecipeDetails : Model -> Element Msg
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


renderRecipeBaseInfo : D.Recipe -> Element Msg
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


renderRecipeIngredients : D.Recipe -> Element Msg
renderRecipeIngredients recipe =
    Element.none


renderRecipeInstructions : D.Recipe -> Element Msg
renderRecipeInstructions recipe =
    Element.none


renderRecipeComments : D.Recipe -> Element Msg
renderRecipeComments recipe =
    Element.none


renderAddRecipeInput : Model -> Element Msg
renderAddRecipeInput model =
    el
        [ width fill
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


footer : Model -> Element Msg
footer model =
    row
        [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        , Border.color C.footerBorderColor
        , Background.color C.footerBackgroundColor
        , paddingXY 5 5
        , Font.size 16
        , width fill
        ]
        [ el [ alignLeft ] <| text "Left"
        , el [ centerX ] <| text model.footerMessage
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
