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
    | Article (Maybe String)
    | Person Person
    | User (Maybe String)


type Admin
    = Special
    | Settings Int
    | Profile String String


type Person
    = You
    | Me


decodeHome : U.Decoder Route
decodeHome =
    U.decode Home


decodeBlue : U.Decoder Route
decodeBlue =
    U.decode Blue
        |> U.path "blue"
        |> U.int


decodeGreen : U.Decoder Route
decodeGreen =
    U.decode Green
        |> U.path "green"
        |> U.string


decodeAdmin : U.Decoder Route
decodeAdmin =
    U.decode Admin
        |> U.path "admin"
        |> U.oneOf
            [ U.decode Profile
                |> U.path "profile"
                |> U.string
                |> U.string
            , U.decode Special
                |> U.path "settings"
                |> U.path "3"
            , U.decode Settings
                |> U.path "settings"
                |> U.int
            ]
        |> U.int


decodeSearch : U.Decoder Route
decodeSearch =
    U.decode Search
        |> U.path "search"
        |> U.query "page" Q.int
        |> U.query "search" Q.string


decodePages : U.Decoder Route
decodePages =
    U.decode Pages
        |> U.path "pages"
        |> U.query "page" (Q.list Q.int)


decodeArticle : U.Decoder Route
decodeArticle =
    U.decode Article
        |> U.path "article"
        |> U.fragment


decodePerson : U.Decoder Route
decodePerson =
    U.decode Person
        |> U.path "person"
        |> U.option [ ( "you", You ), ( "me", Me ) ]


decodeUser : U.Decoder Route
decodeUser =
    U.decode User
        |> U.path "user"
        |> U.oneOf
            [ U.decode Just
                |> U.string
            , U.decode Nothing
            ]


suite : Test
suite =
    describe "elm-url-pipeline"
        [ test "decodes a home path" <|
            \_ ->
                testUrl "/" Home <|
                    U.fromUrl
                        [ decodeBlue
                        , decodeHome
                        , decodeGreen
                        ]

        --
        , test "decodes a path" <|
            \_ ->
                testUrl "/blue/2" (Blue 2) <|
                    U.fromUrl
                        [ decodeBlue ]

        --
        , test "decodes a path, only if complete (start)" <|
            \_ ->
                testUrlFail "/something/blue/2" <|
                    U.fromUrl
                        [ decodeBlue ]

        --
        , test "decodes a path, only if complete (end)" <|
            \_ ->
                testUrlFail "/blue/2/something" <|
                    U.fromUrl
                        [ decodeBlue ]

        --
        , test "decodes a path amongst others (first decodeing)" <|
            \_ ->
                testUrl "/" Home <|
                    U.fromUrl
                        [ decodeHome
                        , decodeBlue
                        , decodeGreen
                        ]

        --
        , test "decodes a path amongst others (second decodeing)" <|
            \_ ->
                testUrl "/blue/3" (Blue 3) <|
                    U.fromUrl
                        [ decodeHome
                        , decodeBlue
                        , decodeGreen
                        ]

        --
        , test "decodes a path amongst others (thrid decodeing)" <|
            \_ ->
                testUrl "/green/hello" (Green "hello") <|
                    U.fromUrl
                        [ decodeHome
                        , decodeBlue
                        , decodeGreen
                        ]

        --
        , test "decodes with oneOf in the middle (first option)" <|
            \_ ->
                testUrl "/admin/settings/2/5" (Admin (Settings 2) 5) <|
                    U.fromUrl
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeGreen
                        ]

        --
        , test "decodes with oneOf in the middle (second option)" <|
            \_ ->
                testUrl "/admin/profile/elm/evan/8" (Admin (Profile "elm" "evan") 8) <|
                    U.fromUrl
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeGreen
                        ]

        --
        , test "decodes with oneOf with two valid paths" <|
            \_ ->
                testUrl "/admin/settings/3/8" (Admin Special 8) <|
                    U.fromUrl
                        [ decodeHome
                        , decodeBlue
                        , decodeAdmin
                        , decodeGreen
                        ]

        --
        , test "decodes a query" <|
            \_ ->
                testUrl "/search?search=hello&page=4" (Search 4 "hello") <|
                    U.fromUrl
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
                    U.fromUrl
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
                testUrl "/article#header" (Article (Just "header")) <|
                    U.fromUrl
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
                    U.fromUrl
                        [ decodeHome
                        , decodePerson
                        , decodeBlue
                        , decodeAdmin
                        ]

        --
        , test "decodes a maybe path (Nothing)" <|
            \_ ->
                testUrl "/user" (User Nothing) <|
                    U.fromUrl
                        [ decodeHome
                        , decodePerson
                        , decodeBlue
                        , decodeUser
                        , decodeAdmin
                        ]

        --
        , test "decodes a maybe path (Just)" <|
            \_ ->
                testUrl "/user/kwame" (User (Just "kwame")) <|
                    U.fromUrl
                        [ decodeHome
                        , decodePerson
                        , decodeBlue
                        , decodeUser
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
