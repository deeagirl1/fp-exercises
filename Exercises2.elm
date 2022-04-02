module Exercises2 exposing (..)
import Html
import Html exposing (Html)
import Html exposing (s)
import Html.Attributes exposing (list)
import Html exposing (b)
import Tuple exposing (..)
import String
import PosProduct exposing (posProduct)


insert : comparable -> List comparable -> List comparable
insert value inputList =
    case inputList of
        []->
            [value]
        x :: xs ->
            if x > value then
                value :: insert x xs
            else if x < value then
                x :: insert value xs
            else
                value :: insert x xs

insertionSort : List comparable -> List comparable -> List comparable
insertionSort input tempSorted =
    case input of
        [] ->
          tempSorted
        x :: xs ->
          insertionSort xs (insert x tempSorted)

posProduct : List (Int, Int) -> List Int
posProduct list =
    case list of
        [] ->
         []
        (x,y) :: xs ->
            if x * y > 0 then
              x * y :: posProduct xs
            else
              posProduct xs

type Expression
  = Plus  Expression Expression
  | Minus Expression Expression
  | Mult  Expression Expression
  | Fact  Expression
  | Val   Int

fact : Int -> Int
fact x =
    if x <=0 then
     1
    else
        x * fact (x - 1)
toString : Expression -> String
toString expression =
    case expression of 
        Plus a b ->
            "(" ++ toString a ++ "+" ++ toString b ++ ")"
        Minus a b -> 
            "(" ++ toString a ++ "-" ++ toString b ++ ")"
        Mult a b ->
            "(" ++ toString a ++ "*" ++ toString b ++ ")"
        Fact a ->
            toString a ++ "!"
        Val a ->
            String.fromInt a
          
eval : Expression -> Int
eval expression =
    case expression of
        Plus a b ->
            eval a + eval b
        Minus a b ->
            eval a - eval b
        Mult a b ->
            eval a * eval b
        Fact a  ->
           fact(eval a)
        Val a ->
            a

prune : Expression -> Expression
prune expression =
     case expression of
        Val expr ->
          Val expr

        Minus (Val expr1) (Val expr2) ->
            Val (expr1 - expr2)
        Minus expr1 expr2 ->
          if (expr1 == Val 0 ) || (prune expr1 == Val 0) then
            prune expr2
          else if (expr2 == Val 0) || (prune expr2 == Val 0) then
            prune expr1
          else
            Minus (prune expr1) (prune expr2)

        Plus (Val expr1) (Val expr2) ->
            Val (expr1 + expr2)
        Plus expr1 expr2 ->
          if (expr1 == Val 0 ) || (prune expr1 == Val 0) then
            prune expr2
          else if (expr2 == Val 0) || (prune expr2 == Val 0) then
            prune expr1
          else
            Plus (prune expr1) (prune expr2)

        Mult (Val expr1) (Val expr2) ->
            Val (expr1 * expr2)
        Mult expr1 expr2 ->
          if (expr1 == Val 1 ) || (prune expr1 == Val 1) then
            prune expr2
          else if (expr2 == Val 1) || (prune expr2 == Val 1) then
            prune expr1
          else if (expr1 == Val 0) || (prune expr1 == Val 0) then
            Val 0
          else if (expr2 == Val 0) || (prune expr2 == Val 0) then
            Val 0
          else
            Mult (prune expr1) (prune expr2)
        
        Fact (Val expr1) ->
            Val (fact expr1)

        Fact expr1  ->
            if expr1 == (Val 0) then
              Val 1
            else
              Fact expr1
            
        
e0 : Expression
e0 = Fact (Val 5)
e1 : Expression
e1 = Mult (Plus e0 (Val -110)) (Val 3) 

my_results: List String
my_results =
    [
        "-- exam-211005 examples --\n",
        "TEST - insert -",
        pr <| insert 4 [1,3,5,5,9],
        pr <| insert 2 [5,3,6,2],
        pr <| insert 0 [2,1,1,5,2],
        pr <| insert 1 [1,3,7,8,10],
        pr <| insert 2 [7,6,5,3,2,1],
        pr <| insert 6 [],
        pr <| insert 3 [1,2,3,4],
        pr <| insert 4 [1,3,5,7,9],
        pr <| insert -4 [5,3,6,2] ,
        pr <| insert 73 [5,3,6,2],
        "TEST - insertionSort",
        pr <| insertionSort [3,7,4,9,1]  [],
        pr <| insertionSort [-4,-6,7,2,1] [],
        pr <| insertionSort [7,6,5,4,3,2,1] [],
        pr <| insertionSort [] [],
        pr <| insertionSort [2,2,1,2,2] [],
        "TEST - posProduct",
        pr <| posProduct [ (3,-4),(2,5),(7,0),(-1,-9) ],
        pr <| posProduct [],
        pr <| posProduct [ (-3,-4),(2,-5),(7,0),(-1,-9) ],
        pr <| posProduct [(-3,4), (-2,5), (7, 0) ,(-1 ,9)],
        "Test - expressions",
        pr <| toString e0,
        pr <| eval e0,
        pr <| toString e1,
        pr <| eval e1,
        "Test - prune",
        toString (prune e0),
        toString (prune e1),


        "\n-- end --"
    ]

-- Boiler-plate below:
-- update this values for long output lines, or small browser windows
page_width : number
page_width = 1000 

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr : a -> String
pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)