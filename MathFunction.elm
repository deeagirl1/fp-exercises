module MathFunction exposing (..)
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

----code----
type Function
  = Poly Function Float
  | Mult Function Function
  | Div Function Function
  | Plus Function Function
  | Minus Function Function
  | Const Float
  | X 


print : Function -> String
print expr = 
   case expr of
       Mult a b ->
         "(" ++ print a ++ "*" ++ print b ++ ")"
       Plus a b ->
         "(" ++ print a ++ "+" ++ print b ++ ")"
       Minus a  b ->
         "(" ++ print a ++ "-" ++ print b ++ ")"
       Poly a b ->
         "(" ++ print a ++ "^" ++ fromFloat b ++ ")"
       Div a b ->
         "(" ++ print a ++ "/" ++ print b ++ ")"
       Const a ->
         fromFloat a
       X ->
        "x"

eval : Float -> Function -> Float
eval y expr = 
   case expr of 
     Mult a b ->
        eval y a * eval y b
     Plus a b ->
        eval y a + eval y b
     Minus a b ->
        eval y a - eval y b
     Poly a b ->
        eval y a ^ b
     Div a b ->
        eval y a / eval y b
     Const a ->
         a
     X ->
      y

derivative : Function -> Function
derivative expr = 
  case expr of
        Const _ ->
            Const 0
        X ->
            Const 1
        Plus a b ->
            Plus (derivative a) (derivative b)
        Minus a b ->
            Minus (derivative a) (derivative b)
        Mult a b ->
            Plus (Mult (derivative a) b) (Mult a (derivative b))
        Div a b ->
            Div
                (Minus (Mult (derivative a) b) (Mult a (derivative b)))
                (Mult b b)
        Poly a n ->
            Mult (derivative a) (Mult (Const n) (Poly a (n - 1)))


simplify : Function -> Function
simplify function = 
    case function of
        X ->
            X

        Const a ->
          Const a

        Poly a b ->
          if b == 0 then
            Const 1
          else 
            if b == 1 then
              simplify a
            else
              Poly (simplify a) b

        
        Mult (Const a) (Const b)->
          Const (a * b)

        Mult a b ->
          if a == (Const 0) || b == (Const 0) then
             Const 0
          else 
            if a == (Const 1) || (simplify a) == (Const 1)  then
             simplify b
            else 
              if b == (Const 1) || (simplify b) == (Const 1) then
                simplify a
              else
                Mult (simplify a) (simplify b)

        Div (Const a) (Const b)->
          Const (a / b)

        Div a b ->
         if a == (Const 1) || (simplify a) == (Const 1)  then
             simplify b

          else if b == (Const 1) || (simplify b) == (Const 1) then
             simplify a
          else
             Div (simplify a) (simplify b)


        Minus (Const a) (Const b)->
          Const (a - b)
          
        Minus a b ->
          if a == (Const 0) || (simplify a) == (Const 0) then
             simplify b

          else if b == (Const 0) || (simplify b) == (Const 0) then
             simplify a
         
          else
             Minus (simplify a) (simplify b)

        Plus (Const a) (Const b)->
          Const (a + b)

        Plus a b ->
          if a == (Const 0) || (simplify a) == (Const 0)  then
            simplify b
          else if b == (Const 0) || (simplify b) == (Const 0) then
            simplify a
          else
            Plus (simplify a) (simplify b)
        
-- this function is used to calculate y-axis values
-- which will be used to store the needed values to generate the graph
calculateYValues: Function -> Float -> Float -> List Float
calculateYValues expr minX maxX= 
    if(minX < maxX) then
       eval minX expr :: calculateYValues expr (minX + 1) maxX
    else
      []


-- this function keeps generating lines with either *, - or both
-- depending on the result given by the function evaluation (used in the graph function)
-- we check the following conditions to generate a line :
-- -> if the y_min is smaller than the function evalution and we are still in the range,
--    then we put a * 
--    else if we are still within the range, then put a - 

line : Float -> Float -> Float -> String
line value upper_range current = 
  if current < value && current <= upper_range then
    "*" ++ line value upper_range (current + 1)
  else 
    if current <= upper_range then
      "-" ++ line value upper_range (current + 1)
    else
      ""

-- function used for creating the graph for the function
-- the list of float stores the results obtained from the function evaluations
-- for each element of the list, we draw a line according to what String the "line" function returns
graph : List Float -> Float -> Float -> Float -> Float -> String
graph ls x_min x_max y_min y_max = 
      case ls of 
      [] ->
        ""
      x :: xs -> 
       line x y_max y_min ++ "\n" ++ graph xs x_min x_max y_min y_max
       

-- collecting results for printing:

f : Function
f = Plus (Minus (Poly (Minus (Div (X) (Const 5)) (Const 1)) 4) (Poly (Plus (Div (X) (Const -2)) (Const 2)) 2))(Const 6)
f2 : Function
f2 =  Plus X (Const 2)
f3 : Function
f3 = Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2)
f4 : Function 
f4 = Plus X (Const 1)

f5 : Function
f5 = Poly X 2
lst : List Float
lst = calculateYValues f -10 20
ls2 : List Float
ls2 = calculateYValues (derivative f) -10 20

my_results: List String
my_results =
    [
       "Test - simplify f",
       print f,
       print (derivative f),
       print (simplify (derivative f)),
       "Test - simplify f2",
       print f2,
       print (derivative f2),
       print (simplify (derivative f2)),
       "Test - simplify f3",
       print f3,
       print (derivative f3),
       print (simplify (derivative f3)),
       "Test - simplify f4",
       print f4,
       print (derivative f4),
       print (simplify (derivative f4)),
      "Test - simplify f5",
       print f5,
       print (derivative f5),   
       print (simplify (derivative f5)),
       
       graph ls2 -10 20 -10 10

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