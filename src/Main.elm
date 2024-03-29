module Main exposing (main)

import Browser
import Browser.Events as BE
import Domain as D
import Dropdown
import Element exposing (..)
import Html exposing (Html)
import Http
import HttpJsonController as H
import Json.Decode as JD
import ModelMessage as M
import RecipeElements as RE
import TopElements as TE



---- INIT ----


init : JD.Value -> ( M.Model, Cmd M.Msg )
init flags =
    let
        decodedFlags =
            case JD.decodeValue flagsDecoder flags of
                Ok okFlags ->
                    okFlags

                Err _ ->
                    emptyFlags
    in
    ( { page = M.TitlePage
      , recipes = []
      , recipeNameToFind = ""
      , recipeToInsert = Nothing
      , recipeToFocus = Nothing
      , editRecipeBaseInfo = False
      , showAddIngredientInput = False
      , selectedIngredientDropdownState = Dropdown.init "dropdown"
      , selectedIngredientInDropdown = Nothing
      , recipeIngredientToEdit = Nothing
      , menus = []
      , menuNameToFind = ""
      , menuToFocus = Nothing
      , userText = ""
      , footerMessage = ""
      , mealsUrl = decodedFlags.mealsUrl
      , width = decodedFlags.width
      , height = decodedFlags.height
      , fontSize = max (min (decodedFlags.width // 32) 32) 16
      }
    , Cmd.none
    )



---- FLAGS ----


flagsDecoder : JD.Decoder Flags
flagsDecoder =
    JD.map4 Flags
        (JD.field "environment" JD.string)
        (JD.field "mealsUrl" JD.string)
        (JD.field "width" JD.int)
        (JD.field "height" JD.int)


emptyFlags : Flags
emptyFlags =
    { environment = "", mealsUrl = "", width = 0, height = 0 }


type alias Flags =
    { environment : String, mealsUrl : String, width : Int, height : Int }



---- UPDATE ----


update : M.Msg -> M.Model -> ( M.Model, Cmd M.Msg )
update msg model =
    case msg of
        M.ViewportChange width height ->
            ( { model | width = width, height = height, fontSize = max (min (width // 32) 32) 16 }, Cmd.none )

        M.DisplayTitle ->
            ( { model | page = M.TitlePage, recipeToFocus = Nothing, editRecipeBaseInfo = False, showAddIngredientInput = False, menuToFocus = Nothing, footerMessage = "" }, Cmd.none )

        M.DisplayAbout ->
            ( { model | page = M.AboutPage, recipeToFocus = Nothing, editRecipeBaseInfo = False, showAddIngredientInput = False, menuToFocus = Nothing, footerMessage = "" }, Cmd.none )

        M.FindRecipes name ->
            update M.FindRecipesExecute { model | recipeNameToFind = name }

        M.FindRecipesExecute ->
            ( model, postFindRecipes model )

        M.InsertRecipe recipe ->
            update M.InsertRecipeExecute { model | recipeToInsert = Just recipe }

        M.InsertRecipeExecute ->
            ( model, postInsertRecipe model )

        M.GotRecipes result ->
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
                    update M.DisplayRecipes { model | recipes = response.recipes, footerMessage = newFooterMessage }

                Err httpError ->
                    case httpError of
                        Http.BadBody badBodyMsg ->
                            ( { model | recipes = [], footerMessage = badBodyMsg }, Cmd.none )

                        _ ->
                            ( { model | recipes = [], footerMessage = "Unknown Error" }, Cmd.none )

        M.GotInsertRecipeResponse result ->
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
                    update M.DisplayRecipes { model | footerMessage = newFooterMessage }

                Err httpError ->
                    case httpError of
                        Http.BadBody badBodyMsg ->
                            update M.DisplayRecipes { model | recipes = [], footerMessage = badBodyMsg }

                        _ ->
                            update M.DisplayRecipes { model | recipes = [], footerMessage = "Unknown Error" }

        M.LoadRecipes ->
            update M.LoadRecipesExecute { model | recipeNameToFind = "" }

        M.LoadRecipesExecute ->
            ( model, postGetAllRecipes model )

        M.DisplayRecipes ->
            ( { model | page = M.RecipesPage }, Cmd.none )

        M.DisplayRecipeDetails recipe ->
            ( { model | recipeToFocus = Just recipe, showAddIngredientInput = False }, Cmd.none )

        M.ChangeRecipeBaseInfo ->
            ( { model | editRecipeBaseInfo = True, showAddIngredientInput = False }, Cmd.none )

        M.UpdateRecipeBaseInfo recipeName portions httpLink ->
            ( { model | editRecipeBaseInfo = False }, postUpdateRecipeBaseInfo model recipeName portions httpLink )

        M.GotUpdateRecipeBaseInfoResponse result ->
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
                    ( { model | footerMessage = newFooterMessage }, Cmd.none )

                Err httpError ->
                    case httpError of
                        Http.BadBody badBodyMsg ->
                            ( { model | footerMessage = badBodyMsg }, Cmd.none )

                        _ ->
                            ( { model | footerMessage = "Unknown Error" }, Cmd.none )

        M.ChangeRecipeIngredient ingredient ->
            ( { model | recipeIngredientToEdit = Just ingredient, showAddIngredientInput = False }, Cmd.none )

        M.DisplayAddIngredientToRecipe ->
            ( { model | showAddIngredientInput = True }, Cmd.none )

        M.AddIngredientDropdownMsg dropdownMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update (RE.ingredientDropdownConfig model) dropdownMsg model model.selectedIngredientDropdownState
            in
            ( { model | selectedIngredientDropdownState = state }, cmd )

        M.AddIngredientDropdownSelectMsg maybeProductName ->
            ( { model | selectedIngredientInDropdown = maybeProductName }, Cmd.none )

        M.LoadMenus ->
            update M.LoadMenusExecute { model | menuNameToFind = "", showAddIngredientInput = False }

        M.LoadMenusExecute ->
            ( model, postGetAllMenus model )

        M.DisplayMenus ->
            ( { model | page = M.MenusPage, editRecipeBaseInfo = False }, Cmd.none )

        M.UserTypedText text ->
            ( { model | userText = text }, Cmd.none )

        M.GotMenus result ->
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
                    update M.DisplayMenus { model | menus = response.menus, footerMessage = newFooterMessage }

                Err httpError ->
                    case httpError of
                        Http.BadBody badBodyMsg ->
                            ( { model | menus = [], footerMessage = badBodyMsg }, Cmd.none )

                        _ ->
                            ( { model | menus = [], footerMessage = "Unknown Error" }, Cmd.none )

        M.DisplayMenuDetails menu ->
            ( { model | menuToFocus = Just menu }, Cmd.none )



---- HTTP HELPERS ----


expectJson_ : (Result Http.Error a -> msg) -> JD.Decoder a -> Http.Expect msg
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
                    case JD.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err _ ->
                            Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case JD.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (JD.errorToString err))


postFindRecipes : M.Model -> Cmd M.Msg
postFindRecipes model =
    Http.post
        { url = model.mealsUrl
        , body = Http.jsonBody <| H.findRecipeEncoder model.recipeNameToFind
        , expect = Http.expectJson M.GotRecipes H.responseDecoder
        }


postGetAllRecipes : M.Model -> Cmd M.Msg
postGetAllRecipes model =
    Http.post
        { url = model.mealsUrl
        , body = Http.jsonBody <| H.getAllRecipesEncoder
        , expect = expectJson_ M.GotRecipes H.responseDecoder
        }


postInsertRecipe : M.Model -> Cmd M.Msg
postInsertRecipe model =
    case model.recipeToInsert of
        Just recipe ->
            Http.post
                { url = model.mealsUrl
                , body = Http.jsonBody <| H.insertRecipeEncoder recipe
                , expect = Http.expectJson M.GotInsertRecipeResponse H.responseDecoder
                }

        Nothing ->
            Cmd.none


postGetAllMenus : M.Model -> Cmd M.Msg
postGetAllMenus model =
    Http.post
        { url = model.mealsUrl
        , body = Http.jsonBody <| H.getAllMenusEncoder
        , expect = expectJson_ M.GotMenus H.responseDecoder
        }


postUpdateRecipeBaseInfo : M.Model -> D.RecipeName -> D.Portions -> D.HttpLink -> Cmd M.Msg
postUpdateRecipeBaseInfo model recipeName portions httpLink =
    case model.recipeToFocus of
        Just recipe ->
            Http.post
                { url = model.mealsUrl
                , body = Http.jsonBody <| H.updateRecipeBaseInfoEncoder recipe recipeName portions httpLink
                , expect = expectJson_ M.GotUpdateRecipeBaseInfoResponse H.responseDecoder
                }

        Nothing ->
            Cmd.none



---- VIEW ----


view : M.Model -> Html M.Msg
view model =
    layout [] <|
        column
            [ width fill
            , height fill
            , spacing 0
            ]
            [ TE.header
            , TE.middle model
            , TE.footer model
            ]



---- SUBSCRIPTIONS ----


subscriptions : M.Model -> Sub M.Msg
subscriptions _ =
    BE.onResize <|
        \width height ->
            M.ViewportChange width height



---- MAIN ----


main : Program JD.Value M.Model M.Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
