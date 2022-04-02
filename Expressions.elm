module Expressions exposing (..)
import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Html exposing (Html)
import Html exposing (s)
import Html.Attributes exposing (list)
import Html exposing (b)
import Tuple exposing (..)
import String exposing (fromFloat)
import Html.Events exposing (custom)
import String exposing (fromInt)

----code----
type Expression
  = Plus Expression Expression
  | Minus Expression Expression
  | Mult Expression Expression
  | Fact Expression
  | Val Int



print : Expression -> String
print expr = 
   case expr of
       Plus a b ->
         "(" ++ print a ++ "+" ++ print b ++ ")"
       Minus a  b ->
         "(" ++ print a ++ "-" ++ print b ++ ")"
       Mult a b ->
         "(" ++ print a ++ "*" ++ print b ++ ")"
       Fact a ->
         print a ++ "!"
       Val a ->
         fromInt a

eval : Expression -> Int
eval expr = 
   case expr of 
     Mult expr1 expr2 ->
        eval expr1 * eval expr2
     Plus expr1 expr2 ->
        eval expr1 + eval expr2
     Minus expr1 expr2 ->
        eval expr1 - eval expr2
     Fact expr1 ->
        factorial(eval expr1)
     Val expr1 ->
        expr1

factorial : Int -> Int
factorial x = 
    if (x==1) then
        1
    else
        x * factorial (x - 1)

      
prune : Expression -> Expression
prune expres = 
    case expres of
        Val expr ->
          Val expr

        Minus (Val expr1) (Val expr2) ->
            Val (expr1 - expr2)
        Minus expr1 expr2 ->
          if (expr1 == Val 1 ) || (prune expr1 == Val 1) then
            prune expr2
          else if (expr2 == Val 1) || (prune expr2 == Val 1) then
            prune expr1
          else if (expr1 == Val 0 ) || (prune expr1 == Val 0) then
            prune expr2
          else if (expr2 == Val 0) || (prune expr2 == Val 0) then
            prune expr1
          else
            Minus (prune expr1) (prune expr2)

        Plus (Val expr1) (Val expr2) ->
            Val (expr1 + expr2)
        Plus expr1 expr2 ->
          if (expr1 == Val 1 ) || (prune expr1 == Val 1) then
            prune expr2
          else if (expr2 == Val 1) || (prune expr2 == Val 1) then
            prune expr1
          else if (expr1 == Val 0 ) || (prune expr1 == Val 0) then
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
            Val (factorial expr1)

        Fact expr1  ->
            if expr1 == (Val 0) then
              Val 1
            else
              Fact expr1
      


-- collecting results for printing:
e0 = Fact (Val 5)
e1 = Mult (Plus e0 (Val -110)) (Val 3) 
e2 = Mult (Val 42) (Minus (Val 73) (Minus (Mult (Plus (Val 0) (Val 0)) (Val 1)) (Val 0)))
e3 = Minus (Val 0) (Val 1)
my_results: List String
my_results =
    [
        "-- exam-211005 examples --\n",
        print e0,
        pr <| eval e0,
        pr <| print e1,
        pr <| eval e1,
        print (prune e2),
        print (prune e3),
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