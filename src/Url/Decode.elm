module Url.Decode exposing (Decoder, Url, const, decode, fragment, int, map, oneOf, option, query, string, succeed)

import Dict exposing (Dict)
import Internal
import Url
import Url.Query.Decode as Query


{-| -}
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


{-| -}
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


{-| -}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder a) =
    Decoder <| \state ->
        case a state of
            Just ( state_, value ) ->
                Just ( state_, f value )

            Nothing ->
                Nothing


{-| -}
oneOf : List (Decoder a) -> Decoder (a -> b) -> Decoder b
oneOf decoders decoder =
    withNext decoder <| \state next ->
        case List.filterMap (\(Decoder d) -> d state) decoders of
            ( state_, value ) :: _ ->
                Just (state_, next value)

            _ ->
                Nothing


{-| -}
succeed : a -> Decoder a
succeed a =
    Decoder (\state -> Just ( state, a ))


{-| -}
const : String -> Decoder a -> Decoder a
const wanted decoder =
    withNext decoder <| \state next ->
        segment state <| \str ->
            if str == wanted then Just next else Nothing


{-| -}
int : Decoder (Int -> a) -> Decoder a
int decoder =
    withNext decoder <| \state next ->
        segment state <| \str ->
            Maybe.map next (String.toInt str)


{-| -}
string : Decoder (String -> a) -> Decoder a
string decoder =
    withNext decoder <| \state next ->
        segment state <| \str ->
            Just (next str)


{-| -}
option : List ( String, b ) -> Decoder (b -> a) -> Decoder a
option options decoder =
    withNext decoder <| \state next ->
        segment state <| \str ->
            Maybe.map next (amongst options str)


{-| -}
query : String -> Query.Decoder b -> Decoder (b -> a) -> Decoder a
query field (Internal.Decoder queryDecoder) decoder =
    withNext decoder <| \state next ->
        Dict.get field state.params
            |> Maybe.andThen queryDecoder
            |> Maybe.map next
            |> Maybe.map (Tuple.pair state)


{-| -}
fragment : Decoder (String -> a) -> Decoder a
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
