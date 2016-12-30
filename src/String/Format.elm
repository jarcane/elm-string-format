module String.Format exposing (
  format1,
  format2,
  format3,
  format4,
  format5,
  format6,
  format7,
  format8,
  format9
  )

{-| String.Format provides methods to use a format String
    with a tuple providing values for the place holders.

@docs format1, format2, format3, format4, format5, format6, format7, format8, format9
-}

import Regex exposing (replace, regex, Regex)
import String exposing (startsWith)

{-| format1 will format a string with one variable.
  val = 1
  format1 "val: {1}" val
  will create the string: val: 1
-}

format1: String -> a -> String
format1 fmt val =
   replaceFormat fmt 1 val

{-| format2 will format a string with a 2 tuple
  values = (1, 2)
  format2 "{1}, {2}" values
  will create the string: "1, 2"
-}
format2: String -> (a, b) -> String
format2 fmt tuple =
  let
    (first, second) = tuple
    fmt_ = replaceFormat fmt 2 second
  in
    format1 fmt_ first

{-| format3 will format a string with a 3 tuple
  values = (1, 2, 3)
  format3 "{1}, {2}, {3}" values
  will create the string: "1, 2, 3"
-}
format3: String -> (a, b, c) -> String
format3 fmt tuple =
  let
    (first, second, third) = tuple
    fmt_ = replaceFormat fmt 3 third
  in
    format2 fmt_ (first, second)

{-| format4 will format a string with a 4 tuple
  values = (1, 2, 3, 4)
  format4 "{1}, {2}, {3}, {4}" values
  will create the string: "1, 2, 3, 4"
-}
format4: String -> (a, b, c, d) -> String
format4 fmt tuple =
  let
    (first, second, third, fourth) = tuple
    fmt_ = replaceFormat fmt 4 fourth
  in
    format3 fmt_ (first, second, third)

{-| format5 will format a string with a 5 tuple
  values = (1, 2, 3, 4, 5)
  format5 "{1}, {2}, {3}, {4}, {5}" values
  will create the string: "1, 2, 3, 4, 5"
-}
format5: String -> (a, b, c, d, e) -> String
format5 fmt tuple =
  let
    (first, second, third, fourth, fifth) = tuple
    fmt_ = replaceFormat fmt 5 fifth
  in
    format4 fmt_ (first, second, third, fourth)

{-| format6 will format a string with a 6 tuple
  values = (1, 2, 3, 4, 5, 6)
  format6 "{1}, {2}, {3}, {4}, {5}, {6}" values
  will create the string: "1, 2, 3, 4, 5, 6"
-}
format6: String -> (a, b, c, d, e, f) -> String
format6 fmt tuple =
  let
    (first, second, third, fourth, fifth, sixth) = tuple
    fmt_ = replaceFormat fmt 6 sixth
  in
    format5 fmt_ (first, second, third, fourth, fifth)

{-| format7 will format a string with a 7 tuple
  values = (1, 2, 3, 4, 5, 6, 7)
  format7 "{1}, {2}, {3}, {4}, {5}, {6}, {7}" values
  will create the string: "1, 2, 3, 4, 5, 6, 7"
-}
format7: String -> (a, b, c, d, e, f, g) -> String
format7 fmt tuple =
  let
    (first, second, third, fourth, fifth, sixth, seventh) = tuple
    fmt_ = replaceFormat fmt 7 seventh
  in
    format6 fmt_ (first, second, third, fourth, fifth, sixth)

{-| format8 will format a string with a 8 tuple
  values = (1, 2, 3, 4, 5, 6, 7, 8)
  format8 "{1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}" values
  will create the string: "1, 2, 3, 4, 5, 6, 7, 8"
-}
format8: String -> (a, b, c, d, e, f, g, h) -> String
format8 fmt tuple =
  let
    (first, second, third, fourth, fifth, sixth, seventh, eighth) = tuple
    fmt_ = replaceFormat fmt 8 eighth
  in
    format7 fmt_ (first, second, third, fourth, fifth, sixth, seventh)

{-| format9 will format a string with a 8 tuple
  values = (1, 2, 3, 4, 5, 6, 7, 8, 9)
  format9 "{1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}" values
  will create the string: "1, 2, 3, 4, 5, 6, 7, 8, 9"
-}
format9: String -> (a, b, c, d, e, f, g, h, i) -> String
format9 fmt tuple =
  let
    (first, second, third, fourth, fifth, sixth, seventh, eighth, ninth) = tuple
    fmt_ = replaceFormat fmt 9 ninth
  in
    format8 fmt_ (first, second, third, fourth, fifth, sixth, seventh, eighth)

{-| This is not ideal, but because of the Elm type inference for the annotation
    the value must be converted to string and back if originally a string.
-}
convert: a -> String
convert val =
  let
    str = toString val
  in
    if (str |> String.startsWith "\"") && (str |> String.endsWith "\"") then
      str |> String.dropLeft 1 |> String.dropRight 1
    else
     str

replaceFormat: String -> Int -> a -> String
replaceFormat fmt position val =
  replace Regex.All (regex ("\\{" ++ toString position ++ "\\}")) (\_ -> convert val) fmt
