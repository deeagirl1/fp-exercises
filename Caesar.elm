module Caesar exposing (..)
import Html exposing (Html)
import Html exposing (Html)
import Char exposing (fromCode, isUpper,  toCode, isAlpha)
import Html.Attributes exposing (list)
import Tuple exposing (..)
import Browser exposing (element)


----code----
offset: Char -> Int
offset letter =
        if (isUpper letter) then
            toCode 'A'
        else
            toCode 'a'

encode: Int -> Char -> Char
encode key_value letter =
    fromCode ( modBy 26 ((toCode letter - offset letter) + key_value) + offset letter)

decode: Int -> Char -> Char
decode key_value letter =
    encode -key_value letter

--- End Part 1 ---

--- Part 2 ---

filter : (a -> Bool) -> List a -> List a
filter p list = 
  case list of
    [] -> []

    (x::xs) ->
        if p x then
            x :: filter p xs
        else
            filter p xs


normalize : String -> String
normalize string=
    String.fromList(filter isAlpha (String.toList string))


map : (a -> b) -> List a -> List b
map function list = 
  case list of
    [] -> []
    (x::xs) -> function x :: map function xs

encrypt : Int -> String -> String
encrypt key_value=
    String.fromList << map(encode key_value) << String.toList



decrypt : Int -> String -> String
decrypt key_value=
    String.fromList << map(decode key_value) << String.toList



encodeListCharRec: Int -> List Char -> List Char
encodeListCharRec key_value list=
        case list of 
            [] ->
                []
            element :: elements ->
                encode key_value element :: encodeListCharRec key_value elements



encryptRec: Int -> String -> String
encryptRec key_value string =
       String.fromList(encodeListCharRec key_value (String.toList string))

decryptRec: Int -> String -> String
decryptRec key_value string =
       String.fromList(encodeListCharRec -key_value (String.toList string))

--- End Part 2 --- 

---- Part 3 ----

startsWithFunc: List Char -> List Char -> Bool
startsWithFunc word1 word2 =
       case (word1, word2) of 
        ([],[]) ->
            True
        (_ :: _, [])->
            False
        ([], _ :: _) ->
            False
        (x :: xs, y::ys) ->
           if(xs /= [] && ys /= []) then
                x == y && startsWithFunc xs ys
            else
               startsWithFunc [] []

startsWith: String -> String -> Bool
startsWith word1 word2 =
    startsWithFunc(String.toList(word1)) (String.toList(word2))


containsFunc: List a -> List a -> List a -> Bool
containsFunc canaryConstant canary decriptedStr =
       case (canary, decriptedStr) of 
        ([], []) ->                              
          True
        (_ :: _, [])->                             
          False
        ([], _ :: _) ->                            
          True                    
        (x :: xs, y::ys) ->                         
          if (x == y) then                          
            containsFunc canaryConstant xs ys                         
          else                                     
            containsFunc canaryConstant canaryConstant ys                          

contains: String -> String -> Bool
contains canary decriptedStr = 
    containsFunc((String.toList canary)) ((String.toList canary)) ((String.toList decriptedStr))



containsList: List String -> String -> Bool
containsList ls str = 
    case ls of
        [] -> 
           True
        (x :: xs) ->
              contains x str && containsList xs str


candidates : List String -> String -> List (List (Int, String))
candidates canaries inputValue =
   List.map(\canary -> 
                    (List.filter 
                        (\c -> (contains canary (second c))) 
                        (decriptedString 1 inputValue)
                    )
            ) canaries



allResultInOneList: List (List (Int, String)) -> List (Int, String)
allResultInOneList list =
       case list of
        [] -> 
           []
        (element :: elements) ->
            case element of 
              [] -> allResultInOneList elements
              x :: xs -> 
                x :: allResultInOneList (xs :: elements)


decriptedString: Int -> String -> List (Int, String)
decriptedString keyValue inputValue =
    if(keyValue < 26 ) then
        (keyValue, decrypt keyValue inputValue) :: decriptedString (keyValue + 1) inputValue
    else
       []
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list = 
  case list of
    [] -> []
    (x::xs) -> 
      if predicate x then 
        dropWhile predicate xs 
      else list

takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list = 
    case list of
    [] -> 
        []
    (x::xs) ->
        if p x then 
             x :: takeWhile p xs 
        else 
            []

removeElementN : a -> List a -> List a
removeElementN n list =
      case list of
         [] ->  []
         x:: xs -> 
           if (n /= x) then
              x :: removeElementN n xs
           else
              removeElementN n xs

-- removeAt : Int -> List a -> List a
-- removeAt index list =
--      removeAtHelper index 0 list

-- removeAtHelper :Int -> Int -> List a -> List a
-- removeAtHelper index counter list =
--     case list of 
--         [] -> 
--            []
--         x :: xs -> 
--             if (index /= counter) then
--                x :: removeAtHelper index (counter + 1) xs
--             else
--                 removeAtHelper index (counter + 1) xs

-- splitSting : String -> (String, String)
-- splitSting str =
--      let
--         listChar =  String.toList(str)
--         dropVar = removeAt 1 (dropWhile ((/=) '/') listChar)
--      in
--       (String.fromList(takeWhile ((/=) '/') listChar) , String.fromList(dropVar))

splitString : String -> (String, String)
splitString str =
     let
        listChar =  String.toList(str)
        dropVar =  (dropWhile ((/=) '/') listChar)
     in
      (String.fromList(takeWhile ((/=) '/') listChar) , String.fromList(dropVar))
--- End Part 3 ---


my_results: List String
my_results =
    [
        "\n Hello Fontys: Encrypt" , 
        pr <| encrypt 7 (normalize "Hello, 0...Fontys") ,
        "\n OlssvMvuafz: Decrypt" , 
        pr <| decrypt 5 (normalize "DGGADBCOOCZYMJHZYVMTOJOCZHVS"),

        "\n StartWith ['a', 'd','v','v'] ['a', 'd','v'] -> True",
        pr <| startsWithFunc ['a', 'd','v','v'] ['a', 'd','v'],

        "\n StartWithString Hello --> HelloFontys -> True",
        pr <| startsWith "Hello" "HelloFontys",

        "\nContains THE -> BEEYBZAMMAXWKHFXWTKRMHMAXFTQ -> False",
        pr <| contains "THE" "BEEYBZAMMAXWKHFXWTKRMHMAXFTQ",

        "\nList Contains -> Uni -> tys -> FontysUniversity -> True",
        pr <| containsList ["Uni", "tys"] "FontysUniversity",

        "\nList Contains test ['n','t','y','s'] ['f','o','n','t','y','s'] -> True",
        pr <| containsFunc ['n','t','y','s'] ['n','t','y','s'] ['f','o','n','t','y','s'],

         "\ncandidates [THE, AND] DGGADBCOOCZYMJHZYVMTOJOCZHVS ",
        pr <| candidates ["THE", "AND"] "DGGADBCOOCZYMJHZYVMTOJOCZHVS",

         "\ncandidates [THE, AND] DGGADBCOOCZYMJHZYVMTOJOCZHVS \n",
        pr <| allResultInOneList(candidates ["THE", "AND"] "DGGADBCOOCZYMJHZYVMTOJOCZHVS"),

        "\ncontains (encrypt nr str) x -> False",
         pr <| contains "THE" (decrypt 6 "DGGADBCOOCZYMJHZYVMTOJOCZHVS"),

         pr <| splitString "Hello/Fontys"

    ] 
    


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