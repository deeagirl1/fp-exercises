module Propositions exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html exposing (Html)
import Html exposing (s)
import Html.Attributes exposing (list)
import Html exposing (b)
import Tuple exposing (..)
import String exposing (fromFloat)
import Html.Events exposing (custom)
import Bitwise exposing (or)
import Html exposing (a)
import MathFunction exposing (lst)

----code----
type alias Variable = String
type Proposition
  = Or Proposition Proposition
  | And Proposition Proposition
  | Impl Proposition Proposition
  | Bi Proposition Proposition
  | Not Proposition 
  | Var Variable


print : Proposition -> String
print expr = 
   case expr of
       Or a b ->
         "(" ++ print a ++ " or " ++ print b ++ ")"
       And a b ->
         "(" ++ print a ++ " and " ++ print b ++ ")"
       Impl a  b ->
         "(" ++ print a ++ " => " ++ print b ++ ")"
       Bi a b ->
         "(" ++ print a ++ " <=> " ++ print b ++ ")"
       Not a ->
         "not " ++ print a ++ ""
       Var a ->
         a

unique : List a -> List a
unique list = 
    let
        incUnique : a -> List a -> List a
        incUnique element lst =
          if (List.member element lst) then 
              lst
          else
              element :: lst    
    in

       List.foldr incUnique [] list
    
-- members : Proposition -> String
-- members p =
--     case p of
--         Var  a     -> a 
--         Not  a    -> members a
--         Or   a b -> (members a) ++ (members b)
--         And  a b -> (members a) ++ (members b)
--         Bi   a b -> (members a) ++ (members b)
--         Impl a b ->  (members a) ++ (members b)

membersHelper : Proposition -> List String -> List String
membersHelper p list =
    case p of
        Var  a     -> a :: list
        Not  a    -> membersHelper a list
        Or   a b -> (membersHelper a list ) ++ (membersHelper b list)
        And  a b -> (membersHelper a list ) ++ (membersHelper b list)
        Bi   a b -> (membersHelper a list ) ++ (membersHelper b list)
        Impl a b -> (membersHelper a list ) ++ (membersHelper b list)

members : Proposition -> List String
members prop =
    unique(membersHelper prop [])  

switcheroo : Proposition -> Proposition
switcheroo expr = 
   case expr of 
     Impl a b ->
         Or (Not (switcheroo a)) (switcheroo b)
     Bi a b ->
         Or (And (switcheroo a) (switcheroo b)) (And (Not (switcheroo a)) (Not (switcheroo b)))
     Or a b ->
         Or (switcheroo a) (switcheroo b)
     And a b ->
         And (switcheroo a) (switcheroo b)
     Not a  ->
        Not (switcheroo a)
     Var a ->
         Var a

deMorgan : Proposition -> Proposition
deMorgan expression =
  case expression of
     Impl a b ->
       Impl (deMorgan a) (deMorgan b)   
     Bi a b ->
       Bi (deMorgan a) (deMorgan b)
     
     (Not (Or a b)) ->
        (And (Not (deMorgan a)) (Not (deMorgan b)))
     (Not (And a b)) ->
        (Or (Not (deMorgan a)) (Not (deMorgan b)))
     Or a b ->
        (Or (deMorgan a) (deMorgan b))
     And a b ->
        (And (deMorgan a) (deMorgan b))
     Not a  ->
        Not (deMorgan a) 
     Var a ->
         Var a
    
p0 = Not (Or (Var "A") (Var "B"))
p1 = Or (Var "B") p0
p2 = Not (Or p0 (Or p1 (Var "C")))

p3 = Not (Bi (Var "D") (Or (And (Var "C") p1) (Impl (Bi (Var "B") (Var "A")) (And (And (Impl (Or p2 (Var "D")) (Var "A")) (Or (Var "A") (Var "D"))) (Bi (And (Var "C") (Var "D")) p1)))))

p4= (Or (Impl (Var "B") (Not (Var "C"))) (Bi (Not (Var "D")) (Var "E")))
p5 = (Impl (Var"P") (Var "Q"))
p6 = (Bi (Var"P") (Var "Q"))
my_results: List String
my_results =
    [
       print (switcheroo p5),
       print (switcheroo p6),
       print <| deMorgan p3,
       print (switcheroo (Or (Impl (Var "B") (Not (Var "C"))) (Bi (Not (Var "D")) (Var "E")))),
       pr <| members p3 

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