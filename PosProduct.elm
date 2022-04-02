module PosProduct exposing (..)
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

----code----
posProduct: List (Int, Int) -> List Int
posProduct inputList = 
    case inputList of
        [] ->
          []
        (x,y) :: xs ->
            if ( x * y > 0 ) then
              x * y :: posProduct xs
            else
              posProduct xs   
            


        
-- collecting results for printing:
my_results: List String
my_results =
    [
        pr <| posProduct [ (3,-4),(2,5),(7,0),(-1,-9) ] 
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