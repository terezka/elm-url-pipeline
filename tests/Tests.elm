module Tests exposing (Admin(..), Route(..), decodeAdmin, decodeArticle, decodeBlue, decodeGreen, decodeHome, decodePages, decodeSearch, suite, testUrl, testUrlFail)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Url
import Url.Decode as U
import Url.Query.Decode as Q


type Route
    = Home
    | Blue Int
    | Green String
    | Admin Admin Int
    | Search Int String
    | Pages (List Int)
    | Article String
    | Person Person


type Admin
    = Special
    | Settings Int
    | Profile String String


type Person
    = You
    | Me


decodeHome : U.Decoder Route
decodeHome =
    U.succeed Home


decodeBlue : U.Decoder Route
decodeBlue =
    U.succeed Blue
        |> U.const "blue"
        |> U.int


decodeGreen : U.Decoder Route
decodeGreen =
    U.succeed Green
        |> U.const "green"
        |> U.string


decodeAdmin : U.Decoder Route
decodeAdmin =
    U.succeed Admin
        |> U.const "admin"
        |> U.oneOf
            [ U.succeed Profile
                |> U.const "profile"
                |> U.string
                |> U.string
            , U.succeed Special
                |> U.const "settings"
                |> U.const "3"
            , U.succeed Settings
                |> U.const "settings"
                |> U.int
            ]
        |> U.int


decodeSearch : U.Decoder Route
decodeSearch =
    U.succeed Search
        |> U.const "search"
        |> U.query "page" Q.int
        |> U.query "search" Q.string


decodePages : U.Decoder Route
decodePages =
    U.succeed Pages
        |> U.const "pages"
        |> U.query "page" (Q.list Q.int)


decodeArticle : U.Decoder Route
decodeArticle =
    U.succeed Article
        |> U.const "article"
        |> U.fragment


decodePerson : U.Decoder Route
decodePerson =
    U.succeed Person
        |> U.const "person"
        |> U.option [ ( "you", You ), ( "me", Me ) ]


suite : Test
suite =
    describe "elm-url-pipeline"
        [ test "decodes a home path" <|
            \_ ->
                testUrl "/" Home <|
                    U.decode
                        [ decodeBlue
                        , decodeHome
                        , decodeGreen
                        ]

        --
        , test "decodes a path" <|
            \_ ->
                testUrl "/blue/2" (Blue 2) <|
                    U.decode
                        [ decodeBlue ]

        --
        , test "decodes a path, only if complete (start)" <|
            \_ ->
                testUrlFail "/something/blue/2" <|
                    U.decode
                        [ decodeBlue ]

        --
        , test "decodes a path, only if complete (end)" <|
            \_ ->
                testUrlFail "/blue/2/something" <|
                    U.decode
                        [ decodeBlue ]

        --
        , test "decodes a path amongst others (first succeeding)" <|
            \_ ->
                testUrl "/" Home <|
                    U.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeGreen
                        ]

        --
        , test "decodes a path amongst others (second succeeding)" <|
            \_ ->
                testUrl "/blue/3" (Blue 3) <|
                    U.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeGreen
                        ]

        --
        , test "decodes a path amongst others (thrid succeeding)" <|
            \_ ->
                testUrl "/green/hello" (Green "hello") <|
                    U.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeGreen
                        ]

        --
        , test "decodes with oneOf in the middle (first option)" <|
            \_ ->
                testUrl "/admin/settings/2/5" (Admin (Settings 2) 5) <|
                    U.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeGreen
                        ]

        --
        , test "decodes with oneOf in the middle (second option)" <|
            \_ ->
                testUrl "/admin/profile/elm/evan/8" (Admin (Profile "elm" "evan") 8) <|
                    U.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeGreen
                        ]

        --
        , test "decodes with oneOf with two valid paths" <|
            \_ ->
                testUrl "/admin/settings/3/8" (Admin Special 8) <|
                    U.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeGreen
                        ]

        --
        , test "decodes a query" <|
            \_ ->
                testUrl "/search?search=hello&page=4" (Search 4 "hello") <|
                    U.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeSearch
                        , decodeGreen
                        ]

        --
        , test "decodes a query with a list" <|
            \_ ->
                testUrl "/pages?page=4&page=9" (Pages [ 4, 9 ]) <|
                    U.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeSearch
                        , decodeGreen
                        , decodePages
                        ]

        --
        , test "decodes a fragment" <|
            \_ ->
                testUrl "/article#header" (Article "header") <|
                    U.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeSearch
                        , decodeArticle
                        , decodeGreen
                        , decodePages
                        ]

        --
        , test "decodes an option" <|
            \_ ->
                testUrl "/person/you" (Person You) <|
                    U.decode
                        [ decodeHome
                        , decodePerson
                        , decodeBlue
                        , decodeAdmin
                        ]
        ]


testUrl : String -> a -> (Url.Url -> Maybe a) -> Expectation
testUrl url result decoder =
    case Url.fromString ("https://fruits.com" ++ url) of
        Just ok ->
            Expect.equal (Just result) (decoder ok)

        Nothing ->
            Expect.fail "Invalid URL"


testUrlFail : String -> (Url.Url -> Maybe a) -> Expectation
testUrlFail url decoder =
    case Url.fromString ("https://fruits.com" ++ url) of
        Just ok ->
            Expect.equal Nothing (decoder ok)

        Nothing ->
            Expect.fail "Invalid URL"
