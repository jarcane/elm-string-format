module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import String.Format exposing (..)


all : Test
all =
    describe "Format tests"
        [ test "format1 for Int" <|
            \() ->
                Expect.equal (format1 "val: {1}" 1) "val: 1"
        , test "format1 for String" <|
            \() ->
                Expect.equal (format1 "val: {1}" "1") "val: 1"
        , test "format1 for Float" <|
            \() ->
                Expect.equal (format1 "val: {1}" 1.2) "val: 1.2"
        , test "format1 for tuple" <|
            \() ->
                Expect.equal (format1 "val: {1}" ( 1, 1 )) "val: (1,1)"
        , test "format1 for record" <|
            \() ->
                Expect.equal (format1 "val: {1}" { x = 1 }) "val: { x = 1 }"
        , test "format1 for list" <|
            \() ->
                Expect.equal (format1 "val: {1}" [1, 2]) "val: [1,2]"
        , test "format1 multiple placeholders for same place holder" <|
            \() ->
                Expect.equal (format1 "val: {1}, again: {1}" 1) "val: 1, again: 1"
        , test "format2" <|
            \() ->
                Expect.equal (format2 "{1}, {2}" (1, 2)) "1, 2"
        , test "format3" <|
            \() ->
                Expect.equal (format3 "{1}, {2}, {3}" (1, 2, 3)) "1, 2, 3"
        , test "format4" <|
            \() ->
                Expect.equal (format4 "{1}, {2}, {3}, {4}" (1, 2, 3, 4)) "1, 2, 3, 4"
        , test "format5" <|
            \() ->
                Expect.equal (format5 "{1}, {2}, {3}, {4}, {5}" (1, 2, 3, 4, 5)) "1, 2, 3, 4, 5"
        , test "format6" <|
            \() ->
                Expect.equal (format6 "{1}, {2}, {3}, {4}, {5}, {6}" (1, 2, 3, 4, 5, 6)) "1, 2, 3, 4, 5, 6"
        , test "format7" <|
            \() ->
                Expect.equal (format7 "{1}, {2}, {3}, {4}, {5}, {6}, {7}" (1, 2, 3, 4, 5, 6, 7)) "1, 2, 3, 4, 5, 6, 7"
        , test "format8" <|
            \() ->
                Expect.equal (format8 "{1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}" (1, 2, 3, 4, 5, 6, 7, 8)) "1, 2, 3, 4, 5, 6, 7, 8"
        , test "format9" <|
            \() ->
                Expect.equal (format9 "{1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}" (1, 2, 3, 4, 5, 6, 7, 8, 9)) "1, 2, 3, 4, 5, 6, 7, 8, 9"
        , test "format9 mix" <|
            \() ->
              let
                x = 1
                y = "test y"
                z = { z = 1 }
              in
                Expect.equal (format9 "{1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}" (1, "test", [1, 2], (1, 2), 5.5, { x = 1 }, x, y, z)) "1, test, [1,2], (1,2), 5.5, { x = 1 }, 1, test y, { z = 1 }"
        ]
