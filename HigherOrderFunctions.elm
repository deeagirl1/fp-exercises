module HigherOrderFunctions exposing (..)
import Html exposing (Html)
import Char exposing (..)
import String exposing (..)
import List exposing (..)
import Tuple exposing (..)
above100: Int -> Bool 
above100 x =
    x > 100

double: Int -> Int
double x =
    x * 2

repeatUntil: (a -> Bool) -> (a -> a) -> a -> a -- We pass in a predicate and a function with a value we want to use
repeatUntil predicate fun initVal =
    if (predicate initVal) then -- Once the predicate is true, we stop the process
      initVal
    else
      repeatUntil predicate fun (fun initVal) -- Recursive call to continue until predicate is true

aboveValue: Int -> Int -> Bool -- Above function for custom values
aboveValue x y =
    x > y

repeatUntilValue: (a -> a -> Bool) -> (a -> a) -> a -> a -> a -- Same code as above,
repeatUntilValue predicate fun n initVal = -- however done to fit the aboveValue function (custom value)
  let 
    x1 = fun initVal
  in
    if (predicate n x1) /= True then
        repeatUntilValue predicate fun n x1
    else
      x1

collatzNr : List Int -> List Int -- The collatz sequence
collatzNr list =
    case list of
      [] ->
        []
      (x :: xs) ->
        if (modBy 2 x == 0) then -- if the number is even, divide by two
          x // 2 :: x :: xs
        else
          ((3 * x) + 1) :: x :: collatzNr xs -- else, time it by 3 and add one

equalToOne : List Int -> Bool -- predicate that sets all of the x's to one
equalToOne list =
    case list of 
      [] ->
        False
      (x :: xs) ->
        x == 1
      
calculateLog: Float -> Float -> Float -- Calculating logarithm
calculateLog x y =
     logBase x y


smallerOrEqualTo1: Float -> Bool
smallerOrEqualTo1 x =
    x < 1 || x == 1

my_results: List String
my_results =
    [
      "Testing 1st example",
      pr <| repeatUntil above100 double 7,
      "Testing 2nd example",
      pr <| repeatUntil above100 ((+) 1) 42,
      "Testing repeating until given value",
      pr <| repeatUntilValue aboveValue double 200 7,
      "Testing myCollatz function with 1 number",
      pr <| repeatUntil equalToOne collatzNr [19],
      "LOG TESTING",
      pr <| repeatUntil (smallerOrEqualTo1) (calculateLog 2) 256
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