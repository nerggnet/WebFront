module ModelMessage exposing (..)

import Domain as D
import Http
import HttpJsonController as H


type alias Model =
    { page : Page
    , recipes : List D.Recipe
    , recipeNameToFind : D.RecipeName
    , recipeToInsert : Maybe D.Recipe
    , recipeToFocus : Maybe D.Recipe
    , menus : List D.Menu
    , menuNameToFind : D.MenuName
    , menuToFocus : Maybe D.Menu
    , userText : String
    , footerMessage : String
    , mealsUrl : String
    , width : Int
    , height : Int
    , fontSize : Int
    }


type Page
    = TitlePage
    | RecipesPage
    | MenusPage
    | AboutPage


type Msg
    = ViewportChange Int Int
    | DisplayTitle
    | DisplayAbout
    | FindRecipes String
    | FindRecipesExecute
    | GotRecipes (Result Http.Error H.ResponseJson)
    | InsertRecipe D.Recipe
    | InsertRecipeExecute
    | GotInsertRecipeResponse (Result Http.Error H.ResponseJson)
    | LoadRecipes
    | LoadRecipesExecute
    | DisplayRecipes
    | DisplayRecipeDetails D.Recipe
    | LoadMenus
    | LoadMenusExecute
    | GotMenus (Result Http.Error H.ResponseJson)
    | DisplayMenus
    | DisplayMenuDetails D.Menu
    | UserTypedText String
