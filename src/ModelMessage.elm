module ModelMessage exposing (..)

import Domain as D
import Http
import Dropdown


type alias Model =
    { page : Page
    , recipes : List D.Recipe
    , recipeNameToFind : D.RecipeName
    , recipeToInsert : Maybe D.Recipe
    , recipeToFocus : Maybe D.Recipe
    , editRecipeBaseInfo : Bool
    , showAddIngredientInput : Bool
    , selectedIngredientDropdownState : Dropdown.State D.ProductName
    , selectedIngredientInDropdown : Maybe D.ProductName
    , recipeIngredientToEdit : Maybe D.Ingredient
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
    | GotRecipes (Result Http.Error ResponseJson)
    | InsertRecipe D.Recipe
    | InsertRecipeExecute
    | GotInsertRecipeResponse (Result Http.Error ResponseJson)
    | LoadRecipes
    | LoadRecipesExecute
    | DisplayRecipes
    | DisplayRecipeDetails D.Recipe
    | ChangeRecipeBaseInfo
    | UpdateRecipeBaseInfo D.RecipeName D.Portions D.HttpLink
    | GotUpdateRecipeBaseInfoResponse (Result Http.Error ResponseJson)
    | ChangeRecipeIngredient D.Ingredient
    | DisplayAddIngredientToRecipe
    | AddIngredientDropdownMsg (Dropdown.Msg D.ProductName)
    | AddIngredientDropdownSelectMsg (Maybe D.ProductName)
    | LoadMenus
    | LoadMenusExecute
    | GotMenus (Result Http.Error ResponseJson)
    | DisplayMenus
    | DisplayMenuDetails D.Menu
    | UserTypedText String


type alias ResponseJson =
    { message : Maybe String
    , recipes : List D.Recipe
    , menus : List D.Menu
    , shoppingLists : List D.ShoppingList
    }
