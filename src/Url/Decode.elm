module Url.Decode exposing (Decoder, const, decode, fragment, int, map, oneOf, option, query, string, succeed)

import Dict exposing (Dict)
import Internal
import Query.Decode as Query
import Url exposing (Url)


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

        toResult (Decoder d) =
            case d init of
                Just ( state, value ) ->
                    if List.isEmpty state.path then
                        Just value

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    case List.filterMap toResult decoders of
        [ value ] ->
            Just value

        _ ->
            Nothing


{-| -}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder a) =
    Decoder <|
        \state ->
            Maybe.map (\( state_, value ) -> ( state_, f value )) (a state)


{-| -}
oneOf : List (Decoder a) -> Decoder (a -> b) -> Decoder b
oneOf decoders (Decoder func) =
    Decoder <|
        \state ->
            case List.filterMap (\(Decoder d) -> d state) decoders of
                ( state_, value ) :: _ ->
                    apply (func state_) (Just value)

                _ ->
                    Nothing


{-| -}
succeed : a -> Decoder a
succeed a =
    Decoder (\state -> Just ( state, a ))


{-| -}
const : String -> Decoder a -> Decoder a
const wanted (Decoder func) =
    Decoder <|
        \state ->
            segment state <|
                \state_ str ->
                    if str == wanted then
                        func state_

                    else
                        Nothing


{-| -}
int : Decoder (Int -> a) -> Decoder a
int (Decoder f) =
    Decoder <|
        \state ->
            segment state <|
                \state_ str ->
                    apply (f state_) (String.toInt str)


{-| -}
string : Decoder (String -> a) -> Decoder a
string (Decoder f) =
    Decoder <|
        \state ->
            segment state <|
                \state_ str ->
                    apply (f state_) (Just str)


{-| -}
option : List ( String, b ) -> Decoder (b -> a) -> Decoder a
option options (Decoder f) =
    Decoder <|
        \state ->
            segment state <|
                \state_ str ->
                    apply (f state_) (amongst options str)


{-| -}
query : String -> Query.Decoder b -> Decoder (b -> a) -> Decoder a
query field (Internal.Decoder queryD) (Decoder f) =
    Decoder <|
        \state ->
            apply (f state) <| Maybe.andThen queryD (Dict.get field state.params)


{-| -}
fragment : Decoder (String -> a) -> Decoder a
fragment (Decoder func) =
    Decoder <|
        \state ->
            apply (func state) state.fragment



-- INTERNAL


segment : State -> (State -> String -> Maybe a) -> Maybe a
segment state func =
    case state.path of
        str :: rest ->
            func (State rest state.params state.fragment) str

        _ ->
            Nothing


amongst : List ( String, b ) -> String -> Maybe b
amongst options wanted =
    case List.filter (Tuple.first >> (==) wanted) options of
        [ ( _, opt ) ] ->
            Just opt

        _ ->
            Nothing


apply : Maybe ( State, a -> b ) -> Maybe a -> Maybe ( State, b )
apply f a =
    Maybe.map2 (\( state, next ) value -> ( state, next value )) f a



-- PREPARE PATH


preparePath : String -> List String
preparePath path =
    List.reverse <|
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
