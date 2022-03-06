module Domain exposing (..)


type alias ProductName =
    String


type alias ShoppingItemName =
    String


type alias ShoppingListName =
    String


type alias RecipeName =
    String


type alias MenuName =
    String


type RecipeUnit
    = Piece
    | Teaspoon
    | Tablespoon
    | Deciliter
    | Liter
    | Gram
    | Hectogram
    | Kilogram
    | NotDefined


type alias Quantity =
    { amount : Float
    , unit : RecipeUnit
    }


type alias HttpLink =
    String


type alias Comment =
    { comment : String
    }


type alias Instruction =
    { instruction : String
    }


type alias Portions =
    Int


type WeekDay
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


type alias Product =
    { name : ProductName
    , link : Maybe HttpLink
    , comments : List Comment
    }


type alias QuantifiedProduct =
    { product : Product
    , quantity : Quantity
    }


type alias Ingredient =
    QuantifiedProduct


type alias ShoppingItem =
    { name : ShoppingItemName
    , item : QuantifiedProduct
    , comments : List Comment
    }


type alias Recipe =
    { name : RecipeName
    , link : Maybe HttpLink
    , portions : Portions
    , ingredients : List Ingredient
    , instructions : List Instruction
    , comments : List Comment
    }


type alias ShoppingList =
    { name : ShoppingListName
    , items : List ShoppingItem
    }


type alias MenuItem =
    { recipeName : RecipeName
    , weekDay : WeekDay
    }


type alias Menu =
    { name : MenuName
    , items : List MenuItem
    }
