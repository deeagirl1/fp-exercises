module Exercises exposing (..)
import Html
import Html exposing (Html)
import Html exposing (s)
import Html.Attributes exposing (list)
import Html exposing (b)
import Tuple exposing (..)
import String

isSorted : List number -> Bool
isSorted inputList =
    case inputList of
        [] -> 
          True
        [_] ->
          True
        x1 :: x2 :: xs ->
            if (x1 > x2) then -- [2,2,1,2,2]
                False
            else if (x1 == x2) then
                isSorted xs
            else
                isSorted xs

bubbleOnePass : List number -> List number
bubbleOnePass input =
    case input of
        [] ->
          []
        [_] ->
          input
        x :: y :: xs ->
            if x < y then
                x :: bubbleOnePass (y :: xs)
            else if x > y then
                y :: bubbleOnePass (x :: xs)
            else
                x :: y :: bubbleOnePass xs
bubbleSort : List number -> List number
bubbleSort input = 
    case input of
        [] ->
          []
        _ :: _ ->
           repeatUntil isSorted bubbleOnePass input


repeatUntil: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil predicate operation input =
    if (predicate input) then
        input
    else
        repeatUntil predicate operation (operation input)      


takeWhile : (a -> Bool) -> (List a) -> (List a)
takeWhile predicate list =
  case list of
    []  -> 
        []
    x::xs -> 
        if (predicate x) then 
            x :: takeWhile predicate xs
        else
            []

dropWhile : (a -> Bool) -> (List a) -> (List a)
dropWhile predicate list =
  case list of
    []  -> 
        []
    x::xs  -> 
        if  (predicate x) then 
            dropWhile predicate xs
        else 
            xs

lookup : String -> List (String, Int) -> Int
lookup name list =
    case list of 
        [] ->
          0
        (x,y) :: xs ->
          if x == name then
            y
          else
            lookup name xs


type Expression
   = Plus Expression Expression
   | Mult Expression Expression
   | Val Int
   | Var String

toString : Expression -> String
toString expression =
    case expression of
        Plus a b ->
            "(" ++ toString a ++ ")" ++ "+" ++ "(" ++ toString b ++ ")"
        Mult a b ->
            "(" ++ toString a ++ ")" ++ "*" ++ "(" ++ toString b ++ ")"
        Val a ->
            String.fromInt a
        Var a ->
            a

eval : List (String, Int) -> Expression -> Int
eval list expression = 
    case expression of
        Mult a b ->
            (eval list a) * (eval list b)
        Plus a b ->
            (eval list a) + (eval list b)
        Var a ->
            lookup a list
        Val a ->
            a



-- toStringPrio : Expression -> String
-- toStringPrio expression = 
--     case expression of
--         Val a ->
--             String.fromInt a
--         Var a ->
--             a
--         Plus a b ->
--              toStringPrio a ++ "+" ++ toStringPrio b
--         Mult (Val a) (Val b) -> 
--             String.fromInt a ++ "*" ++ String.fromInt b
--         Mult (Var a) (Val b) ->
--              a ++ "*" ++ String.fromInt b
--         Mult (Val a) (Var b) ->
--             String.fromInt a ++ "*" ++ b 
--         Mult (Var a) (Var b) ->
--             "(" ++ a ++ "*" ++  b ++ ")"
--         Mult a b ->
--             "(" ++ toStringPrio a ++ ")" ++ "*" ++ "(" ++ toStringPrio b ++ ")"

        
toStringPrio: Expression -> String
toStringPrio expr = 
    case expr of
        Plus a b ->
             toStringPrio a ++ " + " ++  toStringPrio b 

        Mult (Var a) (Var b) ->
          "(" ++ a ++ "*" ++ b ++ ")"

        Mult (Val a) (Val b) ->
           String.fromInt a ++ "*" ++ String.fromInt b 

        Mult (Var a) (Val b) ->
           a ++ "*" ++ String.fromInt b 

        Mult (Val a) (Var b) ->
           String.fromInt a ++ "*" ++ b 

        Mult a b ->
          "(" ++ toStringPrio a ++ ")" ++ " * " ++ "(" ++toStringPrio b ++ ")"
        
        Val a ->
             String.fromInt a 
        
        Var a ->
            a 
        

lookupB : String -> List (String, Int) -> Maybe Int
lookupB name list =
    if lookup name list /= 0 then
        Just (lookup name list)
    else
        Nothing

-- if 
evalB : List (String, Int) -> Expression -> Maybe Int
evalB list expression =
   if eval list expression /= 0 then
      Just (eval list expression)
   else
      Nothing

          
ex0 : Expression
ex0 = Plus (Val 5) (Var "B")
ex1 : Expression
ex1 = Plus (Mult ex0 (Var "Z")) (Var "A") 
ex2 : Expression
ex2 = Plus (Var "C") (Val -73) 
ex3 : Expression
ex3 = Mult ex0 (Mult (Plus (Val 6) (Var "C")) 
            (Plus (Val 7) (Var "D")))
ex4 : Expression
ex4 = Plus (Mult (Val 5) (Var "B")) (Plus (Mult (Val 6) (Var "C"))
            (Mult (Val 7) (Var "D"))) 
memory : List (String, number)
memory = [("A",2),("B",3)]
my_results: List String
my_results =
    [
        "-- exam-211005 examples --\n",
        "TEST - isSorted -",
        pr <| isSorted [1,3,5,5,9],
        pr <| isSorted [5,3,6,2],
        pr <| isSorted [2,1,1,5,2],
        pr <| isSorted [1,3,7,8,10],
        pr <| isSorted [7,6,5,3,2,1],
        pr <| isSorted [],
        pr <| isSorted [1,2,3,4],
        pr <| isSorted [2,2,1,2,2],
        "TEST - bubbleOnePass -",
        pr <| bubbleOnePass [5,1,4,2,8],
        pr <| bubbleOnePass [],
        pr <| bubbleOnePass [2,1,5,76,2],
        "TEST - bubbleSort -",
        pr <| bubbleSort [2,1,5,76,2],
        pr <| bubbleSort [],
        pr <| bubbleSort [1,2,3,4,5],
        pr <| bubbleSort [2,2,1,2,2,2],
        pr <| bubbleSort [7,6,5,3,2,1],
        "TEST - lookup -",
        pr <| lookup "B" [("A",5),("B",42),("Z",-5)],
        pr <| lookup "A" [("A",5),("B",42),("Z",-5)],
        pr <| lookup "Z" [("A",5),("B",42),("Z",-5)], 
        pr <| lookup "K" [("A",5),("B",42),("Z",-5)],
        pr <| lookup "" [("A",5),("B",42),("Z",-5)],
        pr <| lookup "a" [],
        "TEST - toString -",
        pr <| toString     ex0,
        pr <| toString     ex1,
        pr <| toString     ex2,
        pr <| toString     ex3,
        pr <| toString     ex4,
        "TEST - eval -",
        pr <| eval memory  ex0,
        pr <| eval memory  ex1,
        pr <| eval memory  ex2,
        "TEST - toStringPrio -",
        pr <| toStringPrio ex3,
        pr <| toStringPrio ex4,
        "TEST - lookupB -",
        pr <| lookupB "B" [("A",5),("B",42),("Z",-5)],
        pr <| lookupB "A" [("A",5),("B",42),("Z",-5)],
        pr <| lookupB "Z" [("A",5),("B",42),("Z",-5)], 
        pr <| lookupB "K" [("A",5),("B",42),("Z",-5)],
        pr <| lookupB "" [("A",5),("B",42),("Z",-5)],
        "TEST - evalB -",
        pr <| evalB [("X", 5)] (Plus (Var "X") (Val 73)),
        pr <| evalB [("X", 5)] (Plus (Var "Y") (Val 73)),
        "TEST- dropWhile & takeWhile",
        pr <| dropWhile (isSorted [1,2,6,8,2,1]),
        pr <| takeWhile ((<=) 5) [1,2,4,8,2,1],
        

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