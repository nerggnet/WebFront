module HelperFunctions exposing (..)

import Domain as D
import ModelMessage as M


extractAllIngredientNamesFromModel : M.Model -> List D.ProductName
extractAllIngredientNamesFromModel model =
    let
        ingredients =
            extractAllIngredientsFromRecipes model.recipes
    in
    extractAllIngredientNamesFromIngredients ingredients


extractAllIngredientsFromRecipes : List D.Recipe -> List D.Ingredient
extractAllIngredientsFromRecipes recipes =
    case recipes of
        [] ->
            []

        r :: [] ->
            r.ingredients

        r :: rs ->
            r.ingredients ++ extractAllIngredientsFromRecipes rs


extractAllIngredientNamesFromIngredients : List D.Ingredient -> List D.ProductName
extractAllIngredientNamesFromIngredients ingredients =
    case ingredients of
        [] ->
            []

        i :: [] ->
            [ i.product.name ]

        i :: is ->
            i.product.name :: extractAllIngredientNamesFromIngredients is


stringFromRecipeUnit : D.RecipeUnit -> String
stringFromRecipeUnit unit =
    case unit of
        D.Piece ->
            "Piece"

        D.Teaspoon ->
            "Teaspoon"

        D.Tablespoon ->
            "Tablespoon"

        D.Deciliter ->
            "Deciliter"

        D.Liter ->
            "Liter"

        D.Gram ->
            "Gram"

        D.Hectogram ->
            "Hectogram"

        D.Kilogram ->
            "Kilogram"

        _ ->
            "NotDefined"


stringFromWeekDay : D.WeekDay -> String
stringFromWeekDay unit =
    case unit of
        D.Monday ->
            "Monday"

        D.Tuesday ->
            "Tuesday"

        D.Wednesday ->
            "Wednesday"

        D.Thursday ->
            "Thursday"

        D.Friday ->
            "Friday"

        D.Saturday ->
            "Saturday"

        D.Sunday ->
            "Sunday"
