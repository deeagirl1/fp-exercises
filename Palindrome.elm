module Palindrome exposing (..)
import Html exposing (Html)

-- your functions:

isPalindrome : List a -> Bool
isPalindrome list = 
    let
        reversedList = List.foldl(\x acc -> x::acc) [] list
    in
        if reversedList == list then
            True
        else
            False
    


-- collecting results for printing:

-- arbitrary list:
my_list = [1,0,1]

my_results: List String
my_results =
    [
        "-- Hello-Elm output --\n\n  cube calculations:",
        pr <| isPalindrome my_list,
        "\n-- end --"
    ] 
    
-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 80

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)
    
