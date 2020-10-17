module Tests exposing (Admin(..), Route(..), decodeAdmin, decodeArticle, decodeBlue, decodeGreen, decodeHome, decodePages, decodeSearch, suite, testUrl, testUrlFail)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Query.Decode as Query
import Test exposing (..)
import Url as Url_
import Url.Decode as Url


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


decodeHome : Url.Decoder Route
decodeHome =
    Url.succeed Home


decodeBlue : Url.Decoder Route
decodeBlue =
    Url.succeed Blue
        |> Url.const "blue"
        |> Url.int


decodeGreen : Url.Decoder Route
decodeGreen =
    Url.succeed Green
        |> Url.const "green"
        |> Url.string


decodeAdmin : Url.Decoder Route
decodeAdmin =
    Url.succeed Admin
        |> Url.const "admin"
        |> Url.oneOf
            [ Url.succeed Profile
                |> Url.const "profile"
                |> Url.string
                |> Url.string
            , Url.succeed Special
                |> Url.const "settings"
                |> Url.const "3"
            , Url.succeed Settings
                |> Url.const "settings"
                |> Url.int
            ]
        |> Url.int


decodeSearch : Url.Decoder Route
decodeSearch =
    Url.succeed Search
        |> Url.const "search"
        |> Url.query "page" Query.int
        |> Url.query "search" Query.string


decodePages : Url.Decoder Route
decodePages =
    Url.succeed Pages
        |> Url.const "pages"
        |> Url.query "page" (Query.list Query.int)


decodeArticle : Url.Decoder Route
decodeArticle =
    Url.succeed Article
        |> Url.const "article"
        |> Url.fragment


decodePerson : Url.Decoder Route
decodePerson =
    Url.succeed Person
        |> Url.const "person"
        |> Url.option [("you", You), ("me", Me)]


suite : Test
suite =
    describe "elm-url-pipeline"
        [ test "decodes a home path" <|
            \_ ->
                testUrl "/" Home <|
                    Url.decode
                        [ decodeHome ]

        --
        , test "decodes a path" <|
            \_ ->
                testUrl "/blue/2" (Blue 2) <|
                    Url.decode
                        [ decodeBlue ]

        --
        , test "decodes a path, only if complete (start)" <|
            \_ ->
                testUrlFail "/something/blue/2" <|
                    Url.decode
                        [ decodeBlue ]

        --
        , test "decodes a path, only if complete (end)" <|
            \_ ->
                testUrlFail "/blue/2/something" <|
                    Url.decode
                        [ decodeBlue ]

        --
        , test "decodes a path amongst others (first succeeding)" <|
            \_ ->
                testUrl "/" Home <|
                    Url.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeGreen
                        ]

        --
        , test "decodes a path amongst others (second succeeding)" <|
            \_ ->
                testUrl "/blue/3" (Blue 3) <|
                    Url.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeGreen
                        ]

        --
        , test "decodes a path amongst others (thrid succeeding)" <|
            \_ ->
                testUrl "/green/hello" (Green "hello") <|
                    Url.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeGreen
                        ]

        --
        , test "decodes with oneOf in the middle (first option)" <|
            \_ ->
                testUrl "/admin/settings/2/5" (Admin (Settings 2) 5) <|
                    Url.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeGreen
                        ]

        --
        , test "decodes with oneOf in the middle (second option)" <|
            \_ ->
                testUrl "/admin/profile/elm/evan/8" (Admin (Profile "elm" "evan") 8) <|
                    Url.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeGreen
                        ]

        --
        , test "decodes with oneOf with two valid paths" <|
            \_ ->
                testUrl "/admin/settings/3/8" (Admin Special 8) <|
                    Url.decode
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeGreen
                        ]

        --
        , test "decodes a query" <|
            \_ ->
                testUrl "/search?search=hello&page=4" (Search 4 "hello") <|
                    Url.decode
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
                    Url.decode
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
                    Url.decode
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
                    Url.decode
                        [ decodeHome
                        , decodePerson
                        , decodeBlue
                        , decodeAdmin
                        ]
        ]


testUrl : String -> a -> (Url_.Url -> Maybe a) -> Expectation
testUrl url result decoder =
    case Url_.fromString ("https://fruits.com" ++ url) of
        Just ok ->
            Expect.equal (Just result) (decoder ok)

        Nothing ->
            Expect.fail "Invalid URL"


testUrlFail : String -> (Url_.Url -> Maybe a) -> Expectation
testUrlFail url decoder =
    case Url_.fromString ("https://fruits.com" ++ url) of
        Just ok ->
            Expect.equal Nothing (decoder ok)

        Nothing ->
            Expect.fail "Invalid URL"
