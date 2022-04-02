module InsertSort exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html exposing (Html)
import Html exposing (s)
import Html.Attributes exposing (list)
import Html exposing (b)
import Tuple exposing (..)
import String exposing (fromFloat)
import Html.Events exposing (custom)
import Html exposing (input)

-- list recursion
-- custom types

----code----

insert : comparable -> List comparable -> List comparable
insert val inputList = 
  case inputList of
        [] ->
          [val]
        (x :: xs) ->
            if (x <= val) then
                x :: insert val xs
            else
                val :: insert x xs

insertionSort : List comparable -> List comparable -> List comparable
insertionSort input tempSorted = 
   case input of
    [] ->
      tempSorted
    x :: xs ->
       insert x (insertionSort xs tempSorted) 





        
-- collecting results for printing:
my_results: List String
my_results =
    [
        pr <| insert 4 [2,3,5,6],
        pr <| insertionSort [3,7,4,9,1]  [] 
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