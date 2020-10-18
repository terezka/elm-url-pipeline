module Url.Decode exposing (Decoder, Url, const, decode, fragment, int, map, oneOf, option, query, string, succeed)

{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:
```
  https://example.com:8042/over/there?name=ferret#nose
  \___/   \______________/\_________/ \_________/ \__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

These are the names we will use to talk about segments of a URL in this library.

# Decoding
@docs Url, Decoder, decode, succeed

# Segments
@docs const, string, int, option, fragment, query

# Advanced
@docs map, oneOf

-}

import Dict exposing (Dict)
import Internal
import Url
import Url.Query.Decode as Query


{-| A re-export of [`Url`](https://package.elm-lang.org/packages/elm/url/latest/Url#Url). -}
type alias Url =
    Url.Url


{-| -}
type Decoder a
    = Decoder (State -> Maybe ( State, a ))


type alias State =
    { path : List String
    , params : Dict String (List String)
    , fragment : Maybe String
    }


{-| Decode a [`Url`](https://package.elm-lang.org/packages/elm/url/latest/Url#Url).

    import Url.Decode as U
    import Url.Query.Decode as Q

    type Route
        = Home
        | User String
        | Article Int
        | Search String (Maybe Int)

    toRoute : Url -> Route
    toRoute =
        U.decode
            [ -- /  ==> Just Home
              U.succeed Home

              -- /user/terezka  ==> Just (User "terezka")
            , U.succeed User
                |> U.const "user"
                |> U.string

              -- /article/34  ==> Just (Article 34)
            , U.succeed Article
                |> U.const "article"
                |> U.int

              -- /search?query=nkrumah&page=2  ==> Just (Search "nkrumah" (Just 2))
              -- /search                       ==> Nothing
            , U.succeed Search
                |> U.const "search"
                |> U.query "query" Q.string
                |> U.query "page" (Q.maybe Q.int)
            ]

-}
decode : List (Decoder a) -> Url -> Maybe a
decode decoders url =
    let
        init =
            State (preparePath url.path) (prepareQuery url.query) url.fragment

        run (Decoder d) =
            case d init of
                Just ( state, value ) ->
                    if List.isEmpty state.path
                        then Just value
                        else Nothing

                Nothing ->
                    Nothing
    in
    case List.filterMap run decoders of
        [ value ] ->
            Just value

        _ ->
            Nothing


{-| Apply a function to your decoded value.

    type Route
        = Article Article.Params

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- /article/who-is-nkrumah#early-life ==> Just (Article { slug = "who-is-nkrumah", header = Just "early-life" })
              -- /article/who-is-nkrumah            ==> Just (Article { slug = "who-is-nkrumah", header = Nothing })
              -- /hello                             ==> Nothing
              --
              U.succeed Article.Params
                |> U.const "article"
                |> U.string
                |> U.fragment
                |> U.map Article
            ]

    -- Article.elm

    type alias Params =
        { slug : String
        , header : Maybe String
        }

-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder a) =
    Decoder <| \state ->
        case a state of
            Just ( state_, value ) ->
                Just ( state_, f value )

            Nothing ->
                Nothing


{-| When a route can go in two different valid directions.

    type Route
        = Article ArticleId

    type ArticleId
        = Categorized String Int
        | Single Int

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- Matches both:
              --
              -- /article/12/editing                ==> Just (Article (Single 12))
              -- /article/category/cats/32/editing  ==> Just (Article (Categorized "cats" 32))
              --
              U.succeed Article
                |> U.const "article"
                |> U.oneOf
                    [ U.succeed Single
                        |> U.int
                    , U.succeed Categorized
                        |> U.const "category"
                        |> U.string
                        |> U.int
                    ]
                |> U.const "editing"
            ]

Though, this is fine, it is usually nices to flatten your decoder, like so:

    type Route
        = Article ArticleId

    type ArticleId
        = Categorized String Int
        | Single Int

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- /article/12/editing  ==> Just (Article (Single 12))
              -- /article/56/editing  ==> Just (Article (Single 56))
              -- /article/56          ==> Nothing
              -- /article/hello       ==> Nothing
              -- /what                ==> Nothing
              U.succeed Single
                |> U.const "article"
                |> U.int
                |> U.const "editing"
                |> U.map Article

              -- /article/category/cats/32/editing           ==> Just (Article (Categorized "cats" 32))
              -- /article/category/pan-africanism/6/editing  ==> Just (Article (Categorized "pan-africanism" 6))
              -- /article/category/pan-africanism/6          ==> Nothing
              -- /article                                    ==> Nothing
            , U.succeed Categorized
                |> U.const "article"
                |> U.const "category"
                |> U.string
                |> U.int
                |> U.const "editing"
                |> U.map Article
            ]

Much more readable! It may still be useful in smaller dosages:

    type Route
        = Profile (Maybe String)

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- /user/terezka  ==> Just (User (Just "terezka"))
              -- /user          ==> Just (User Nothing)
              -- /hello         ==> Nothing
              U.succeed User
                |> U.const "user"
                |> U.oneOf
                    [ U.succeed Just
                        |> U.string
                    , U.succeed Nothing
                    ]
            ]

NOTE: The order of the decoders in the list matter! The first one to succeed wins. If
the two decoders in example above were flipped, the `oneOf` would always return `Nothing`,
because it would be first and would always pass.

-}
oneOf : List (Decoder a) -> Decoder (a -> b) -> Decoder b
oneOf decoders decoder =
    withNext decoder <| \state next ->
        case List.filterMap (\(Decoder d) -> d state) decoders of
            ( state_, value ) :: _ ->
                Just (state_, next value)

            _ ->
                Nothing


{-| A decoder which always succeeds! Use to begin a pipeline.

    type Route
        = Home
        | Article Article.Id

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- /           ==> Just Home
              -- /article/23 ==> Just (Article 23)
              -- /bla        ==> Nothing
              U.succeed Home
            , U.succeed Article
                |> U.const "article"
                |> U.int
            ]

-}
succeed : a -> Decoder a
succeed a =
    Decoder (\state -> Just ( state, a ))


{-| A required piece of the path.

    type Route
        = Profile

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- /profile/me ==> Just Profile
              -- /profile    ==> Nothing
              -- /hello      ==> Nothing
              U.succeed Profile
                |> U.const "profile"
                |> U.const "me"
            ]
-}
const : String -> Decoder a -> Decoder a
const wanted decoder =
    withNext decoder <| \state next ->
        segment state <| \str ->
            if str == wanted then Just next else Nothing


{-| Read a variable integer in the path.

    type Route
        = Profile Int

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- /profile/34 ==> Just (Profile 34)
              -- /profile/5  ==> Just (Profile 5)
              -- /profile    ==> Nothing
              U.succeed Profile
                |> U.const "profile"
                |> U.int
            ]

-}
int : Decoder (Int -> a) -> Decoder a
int decoder =
    withNext decoder <| \state next ->
        segment state <| \str ->
            Maybe.map next (String.toInt str)


{-| Read a variable string in the path.

    type Route
        = Profile String

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- /profile/terezka ==> Just (Profile "terezka")
              -- /profile/kwame   ==> Just (Profile "kwame")
              -- /profile/23      ==> Just (Profile "23")
              -- /profile         ==> Nothing
              -- /hello           ==> Nothing
              U.succeed Profile
                |> U.const "profile"
                |> U.string
            ]
-}
string : Decoder (String -> a) -> Decoder a
string decoder =
    withNext decoder <| \state next ->
        segment state <| \str ->
            Just (next str)


{-| Read a custom type in the path.

    type Route
        = Profile Person

    type Person
        = You
        | Me

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- /profile/you   ==> Just (Profile You)
              -- /profile/me    ==> Just (Profile Me)
              -- /profile/hello ==> Nothing
              U.succeed Profile
                |> U.const "profile"
                |> U.string
            ]
-}
option : List ( String, b ) -> Decoder (b -> a) -> Decoder a
option options decoder =
    withNext decoder <| \state next ->
        segment state <| \str ->
            Maybe.map next (amongst options str)


{-| Decode a query parameter.

    type Route
        = Search String (Maybe Int)

    toRoute : Url -> Route
    toRoute =
        U.decode
            [ -- /search?query=nkrumah&page=2  ==> Just (Search "nkrumah" (Just 2))
              -- /search?query=&page=2         ==> Just (Search "" (Just 2))
              -- /search?query=nkrumah         ==> Just (Search "nkrumah" Nothing)
              -- /search                       ==> Nothing
            , U.succeed Search
                |> U.const "search"
                |> U.query "query" Q.string
                |> U.query "page" (Q.maybe Q.int)
            ]

-}
query : String -> Query.Decoder b -> Decoder (b -> a) -> Decoder a
query field (Internal.Decoder queryDecoder) decoder =
    withNext decoder <| \state next ->
        Dict.get field state.params
            |> Maybe.withDefault []
            |> queryDecoder
            |> Maybe.map next
            |> Maybe.map (Tuple.pair state)


{-| Decode a fragment.

    type Route
        = Dashboard (Maybe String)

    toRoute : Url -> Maybe Route
    toRoute =
        U.decode
            [ -- /#about  ==> Just (Dashboard (Just "about"))
              -- /#prices ==> Just (Dashboard (Just "prices"))
              -- /        ==> Just (Dashboard Nothing)
              -- /hello   ==> Nothing
              U.succeed Dashboard
                |> U.fragment
            ]

-}
fragment : Decoder (Maybe String -> a) -> Decoder a
fragment decoder =
    withNext decoder <| \state next ->
        Just (state, next state.fragment)



-- INTERNAL


segment : State -> (String -> Maybe a) -> Maybe (State, a)
segment state func =
    case state.path of
        str :: rest ->
            Maybe.map (Tuple.pair (State rest state.params state.fragment)) (func str)

        _ ->
            Nothing


amongst : List ( String, b ) -> String -> Maybe b
amongst options wanted =
    case List.filter (Tuple.first >> (==) wanted) options of
        [ ( _, opt ) ] ->
            Just opt

        _ ->
            Nothing



withNext : Decoder a -> (State -> a -> Maybe ( State, b )) -> Decoder b
withNext (Decoder decoder) func =
    Decoder <| \state1 ->
        case decoder state1 of
            Just ( state2, next ) ->
                func state2 next

            Nothing ->
                Nothing



-- PREPARE PATH


preparePath : String -> List String
preparePath path =
    case String.split "/" path of
        "" :: segments ->
            removeFinalEmpty segments

        segments ->
            removeFinalEmpty segments


removeFinalEmpty : List String -> List String
removeFinalEmpty segments =
    case segments of
        [] ->
            []

        "" :: [] ->
            []

        seg :: rest ->
            seg :: removeFinalEmpty rest



-- PREPARE QUERY


prepareQuery : Maybe String -> Dict String (List String)
prepareQuery maybeQuery =
    case maybeQuery of
        Nothing ->
            Dict.empty

        Just qry ->
            List.foldr addParam Dict.empty (String.split "&" qry)


addParam : String -> Dict String (List String) -> Dict String (List String)
addParam seg dict =
    case String.split "=" seg of
        [ rawKey, rawValue ] ->
            case Url.percentDecode rawKey of
                Nothing ->
                    dict

                Just key ->
                    case Url.percentDecode rawValue of
                        Nothing ->
                            dict

                        Just value ->
                            Dict.update key (addToParametersHelp value) dict

        _ ->
            dict


addToParametersHelp : a -> Maybe (List a) -> Maybe (List a)
addToParametersHelp value maybeList =
    case maybeList of
        Nothing ->
            Just [ value ]

        Just list ->
            Just (value :: list)
