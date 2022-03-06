module HttpJsonController exposing (..)

import Domain as D
import Json.Decode as JD
import Json.Encode as JE


type Action
    = Unknown
    | FindRecipe
    | InsertRecipe
    | RemoveRecipe
    | ChangeRecipeName
    | UpdateRecipeLink
    | ChangeRecipePortions
    | AddIngredientToRecipe
    | AddInstructionToRecipe
    | AddCommentToRecipe
    | RemoveIngredientFromRecipe
    | RemoveInstructionFromRecipe
    | RemoveCommentFromRecipe
    | FindMenu
    | InsertMenu
    | RemoveMenu
    | ChangeMenuName
    | AddItemToMenu
    | RemoveItemFromMenu
    | FindShoppingList
    | InsertShoppingList
    | RemoveShoppingList
    | ChangeShoppingListName
    | AddItemToShoppingList
    | RemoveItemFromShoppingList



-- type alias CommandJson =
--     { action : Maybe Action
--     }
-- type alias RecipeNameJson =
--     { recipeName : Maybe String
--     }
-- type alias NewRecipeNameJson =
--     { newRecipeName : Maybe String
--     }
-- type alias IngredientNameJson =
--     { ingredientName : Maybe String
--     }
-- type alias PortionsJson =
--     { portions : Maybe Int
--     }
-- type alias LinkJson =
--     { link : Maybe D.HttpLink
--     }
-- type alias MenuNameJson =
--     { menuName : Maybe String
--     }
-- type alias NewMenuNameJson =
--     { newMenuName : Maybe String
--     }
-- type alias ShoppingListNameJson =
--     { shoppingListName : Maybe String
--     }
-- type alias NewShoppingListNameJson =
--     { newShoppingListName : Maybe String
--     }
-- type alias ShoppingItemNameJson =
--     { shoppingItemName : Maybe String
--     }
-- type alias OperationResponse =
--     { recipes : List D.Recipe
--     , menus : List D.Menu
--     , shoppingLists : List D.ShoppingList
--     , success : Maybe String
--     , error : Maybe String
--     }


type alias ResponseJson =
    { message : Maybe String
    , recipes : List D.Recipe
    , menus : List D.Menu
    , shoppingLists : List D.ShoppingList
    }


responseDecoder : JD.Decoder ResponseJson
responseDecoder =
    JD.map4 ResponseJson
        (JD.field "Message" (JD.maybe JD.string))
        (JD.field "Recipes" (JD.list recipeDecoder))
        (JD.field "Menus" (JD.list menuDecoder))
        (JD.field "ShoppingLists" (JD.list shoppingListDecoder))


shoppingListDecoder : JD.Decoder D.ShoppingList
shoppingListDecoder =
    JD.map2 D.ShoppingList
        (JD.field "Name" JD.string)
        (JD.field "Items" (JD.list shoppingItemDecoder))


shoppingItemDecoder : JD.Decoder D.ShoppingItem
shoppingItemDecoder =
    JD.map3 D.ShoppingItem
        (JD.field "Name" JD.string)
        (JD.field "Item" quantifiedProductDecoder)
        (JD.field "Comments" (JD.list commentDecoder))


menuDecoder : JD.Decoder D.Menu
menuDecoder =
    JD.map2 D.Menu
        (JD.field "Name" JD.string)
        (JD.field "Items" (JD.list menuItemDecoder))


menuItemDecoder : JD.Decoder D.MenuItem
menuItemDecoder =
    JD.map2 D.MenuItem
        (JD.field "RecipeName" JD.string)
        (JD.field "WeekDay" weekDayDecoder)


weekDayDecoder : JD.Decoder D.WeekDay
weekDayDecoder =
    JD.string |> JD.andThen weekDayDecoderFromString


weekDayDecoderFromString : String -> JD.Decoder D.WeekDay
weekDayDecoderFromString string =
    case string of
        "Monday" ->
            JD.succeed D.Monday

        "Tuesday" ->
            JD.succeed D.Tuesday

        "Wednesday" ->
            JD.succeed D.Wednesday

        "Thursday" ->
            JD.succeed D.Thursday

        "Friday" ->
            JD.succeed D.Friday

        "Saturday" ->
            JD.succeed D.Saturday

        "Sunday" ->
            JD.succeed D.Sunday

        _ ->
            JD.succeed D.Monday


recipeDecoder : JD.Decoder D.Recipe
recipeDecoder =
    JD.map6 D.Recipe
        (JD.field "Name" JD.string)
        (JD.field "Link" (JD.maybe JD.string))
        (JD.field "Portions" JD.int)
        (JD.field "Ingredients" (JD.list quantifiedProductDecoder))
        (JD.field "Instructions" (JD.list instructionDecoder))
        (JD.field "Comments" (JD.list commentDecoder))


quantifiedProductDecoder : JD.Decoder D.QuantifiedProduct
quantifiedProductDecoder =
    JD.map2 D.QuantifiedProduct
        (JD.field "Product" productDecoder)
        (JD.field "Quantity" quantityDecoder)


productDecoder : JD.Decoder D.Product
productDecoder =
    JD.map3 D.Product
        (JD.field "Name" JD.string)
        (JD.field "Link" (JD.maybe JD.string))
        (JD.field "Comments" (JD.list commentDecoder))


quantityDecoder : JD.Decoder D.Quantity
quantityDecoder =
    JD.map2 D.Quantity
        (JD.field "Amount" JD.float)
        (JD.field "Unit" recipeUnitDecoder)


recipeUnitDecoder : JD.Decoder D.RecipeUnit
recipeUnitDecoder =
    JD.string |> JD.andThen recipeUnitDecoderFromString


recipeUnitDecoderFromString : String -> JD.Decoder D.RecipeUnit
recipeUnitDecoderFromString string =
    case string of
        "Piece" ->
            JD.succeed D.Piece

        "Teaspoon" ->
            JD.succeed D.Teaspoon

        "Tablespoon" ->
            JD.succeed D.Tablespoon

        "Deciliter" ->
            JD.succeed D.Deciliter

        "Liter" ->
            JD.succeed D.Liter

        "Gram" ->
            JD.succeed D.Gram

        "Hectogram" ->
            JD.succeed D.Hectogram

        "Kilogram" ->
            JD.succeed D.Kilogram

        _ ->
            JD.succeed D.NotDefined


commentDecoder : JD.Decoder D.Comment
commentDecoder =
    JD.map D.Comment
        (JD.field "Comment" JD.string)


instructionDecoder : JD.Decoder D.Instruction
instructionDecoder =
    JD.map D.Instruction
        (JD.field "Instruction" JD.string)


findRecipeEncoder : String -> JE.Value
findRecipeEncoder name =
    JE.object
        [ ( "Action", JE.string "FindRecipe" )
        , ( "RecipeName", JE.string name )
        ]


getAllRecipesEncoder : JE.Value
getAllRecipesEncoder =
    JE.object
        [ ( "Action", JE.string "FindRecipe" )
        ]


insertRecipeEncoder : D.Recipe -> JE.Value
insertRecipeEncoder recipe =
    JE.object
        [ ( "Action", JE.string "InsertRecipe" )
        , ( "Name", JE.string recipe.name )
        , ( "Link"
          , JE.string
                (case recipe.link of
                    Just link ->
                        link

                    Nothing ->
                        ""
                )
          )
        , ( "Portions", JE.int recipe.portions )
        , ( "Ingredients", JE.list ingredientEncoder recipe.ingredients )
        , ( "Instructions", JE.list instructionEncoder recipe.instructions )
        , ( "Comments", JE.list commentEncoder recipe.comments )
        ]


recipeEncoder : D.Recipe -> JE.Value
recipeEncoder recipe =
    JE.object
        [ ( "Name", JE.string recipe.name )
        , ( "Link"
          , JE.string
                (case recipe.link of
                    Just link ->
                        link

                    Nothing ->
                        ""
                )
          )
        , ( "Portions", JE.int recipe.portions )
        , ( "Ingredients", JE.list ingredientEncoder recipe.ingredients )
        , ( "Instructions", JE.list instructionEncoder recipe.instructions )
        , ( "Comments", JE.list commentEncoder recipe.comments )
        ]


ingredientEncoder : D.Ingredient -> JE.Value
ingredientEncoder ingredient =
    JE.object
        [ ( "Product", productEncoder ingredient.product )
        , ( "Quantity", quantityEncoder ingredient.quantity )
        ]


productEncoder : D.Product -> JE.Value
productEncoder product =
    JE.object
        [ ( "Name", JE.string product.name )
        , ( "Link"
          , JE.string
                (case product.link of
                    Just link ->
                        link

                    Nothing ->
                        ""
                )
          )
        , ( "Comments", JE.list commentEncoder product.comments )
        ]


quantityEncoder : D.Quantity -> JE.Value
quantityEncoder quantity =
    JE.object
        [ ( "Amount", JE.float quantity.amount )
        , ( "Unit"
          , JE.string
                (case quantity.unit of
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
                )
          )
        ]


instructionEncoder : D.Instruction -> JE.Value
instructionEncoder instruction =
    JE.object
        [ ( "Instruction", JE.string instruction.instruction )
        ]


commentEncoder : D.Comment -> JE.Value
commentEncoder comment =
    JE.object
        [ ( "Comment", JE.string comment.comment )
        ]
