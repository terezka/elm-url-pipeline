module Url.Query.Decode exposing (Decoder, int, list, option, string)

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



-- INTERNAL


amongst : List ( String, b ) -> String -> Maybe b
amongst options wanted =
    case List.filter (Tuple.first >> (==) wanted) options of
        [ ( _, opt ) ] ->
            Just opt

        _ ->
            Nothing
