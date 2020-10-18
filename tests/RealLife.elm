module RealLife exposing (Route(..), suite, testUrl, toRoute)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Url
import Url.Decode as U
import Url.Query.Decode as Q


-- TODO add maybe to query

type Route
    = Home
    | User String
    | Article Int
    | Search String (Maybe Int)


toRoute : U.Url -> Maybe Route
toRoute =
    U.decode
        [ -- /
          U.succeed Home

        -- /user/:username
        , U.succeed User
            |> U.const "user"
            |> U.string

        -- /article/:id
        , U.succeed Article
            |> U.const "article"
            |> U.int

        -- /search?query=hello&page=2
        , U.succeed Search
            |> U.const "search"
            |> U.query "query" Q.string
            |> U.query "page" (Q.maybe Q.int)
        ]


suite : Test
suite =
    describe "elm-url-pipeline"
        [ test "Home" <|
            \_ ->
                testUrl "/" Home
        , test "User" <|
            \_ ->
                testUrl "/user/terezka" (User "terezka")
        , test "Article" <|
            \_ ->
                testUrl "/article/34" (Article 34)
        , test "Search" <|
            \_ ->
                testUrl "/search?query=hello&page=3" (Search "hello" (Just 3))
        , test "Search, empty query" <|
            \_ ->
                testUrl "/search?query=&page=3" (Search "" (Just 3))
        , test "Search, no query" <|
            \_ ->
                testUrlFailed "/search?page=3"
        , test "Search, no page" <|
            \_ ->
                testUrl "/search?query=nkrumah" (Search "nkrumah" Nothing)
        ]


testUrl : String -> Route -> Expectation
testUrl url expected =
    case Url.fromString ("https://fruits.com" ++ url) of
        Just ok ->
            Expect.equal (Just expected) (toRoute ok)

        Nothing ->
            Expect.fail "Invalid URL"


testUrlFailed : String -> Expectation
testUrlFailed url =
    case Url.fromString ("https://fruits.com" ++ url) of
        Just ok ->
            Expect.equal Nothing (toRoute ok)

        Nothing ->
            Expect.fail "Invalid URL"
