module Url.Query.Decode exposing (Decoder, maybe, int, list, map, option, string)

import Internal exposing (Decoder(..))


{-| -}
type alias Decoder a =
    Internal.Decoder a


{-| -}
int : Decoder Int
int =
    Decoder <|
        \params ->
            case params of
                [ str ] ->
                    String.toInt str

                _ ->
                    Nothing


{-| -}
string : Decoder String
string =
    Decoder <|
        \params ->
            case params of
                [ value ] ->
                    Just value

                _ ->
                    Nothing


{-| -}
option : List ( String, a ) -> Decoder a
option options =
    Decoder <|
        \params ->
            case params of
                [ value ] ->
                    amongst options value

                _ ->
                    Nothing


{-| -}
list : Decoder a -> Decoder (List a)
list (Decoder v) =
    Decoder <|
        \params ->
            let
                values =
                    List.filterMap (\s -> v [ s ]) params
            in
            if List.length params == List.length values then
                Just values

            else
                Nothing


{-| -}
maybe : Decoder a -> Decoder (Maybe a)
maybe (Decoder a) =
    Decoder <|
        \params ->
            case params of
                [] ->
                    Just Nothing

                some ->
                    Just (a params)


{-| -}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder a) =
    Decoder <|
        \params ->
            Maybe.map f (a params)



-- INTERNAL


amongst : List ( String, b ) -> String -> Maybe b
amongst options wanted =
    case List.filter (Tuple.first >> (==) wanted) options of
        [ ( _, opt ) ] ->
            Just opt

        _ ->
            Nothing
