module Asset exposing (Image, emptyHost, image, src, toString)

{-| Assets, such as images
-}

import Html exposing (Attribute, Html)
import Html.Attributes
import Svg exposing (circle, svg)
import Svg.Attributes exposing (class, cx, cy, height, r, width)


type Image
    = Image String



-- HOST ICONS


{-| Renders an svg circle based on the given pixel size.
-}
emptyHost : Int -> Html msg
emptyHost size =
    svg
        [ class "empty-host-drag"
        , height <| String.fromInt size
        , width <| String.fromInt size
        ]
        [ circle
            [ cx <| String.fromInt (size // 2)
            , cy <| String.fromInt (size // 2)
            , r <| String.fromInt (size // 2)
            ]
            []
        ]

-- USING IMAGES


image : String -> Image
image url =
    Image url


src : Image -> Attribute msg
src (Image url) =
    Html.Attributes.src url


toString : Image -> String
toString (Image url) =
    url
