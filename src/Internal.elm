module Internal exposing (Decoder(..))

{-| -}


type Decoder a
    = Decoder (List String -> Maybe a)
