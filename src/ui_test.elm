module Main exposing (Msg(..), main, update, view)

import Browser
import Element exposing (column, el, fill, fillPortion, height, layout, padding, paddingXY, rgb255, row, shrink, text, width)
import Element.Background as Background
import Element.Font as Font
import Hexagons.Hex exposing (..)
import Hexagons.Layout exposing (..)
import Hexagons.Map exposing (..)
import Html exposing (Html, button, div, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (rect, svg)
import Svg.Attributes as Svga


main =
    Browser.sandbox { init = 0, update = update, view = view }


grid_layout : Hexagons.layout 
grid_layout = {
    orientation = Hexagons.orientationLayoutPointy
    , size = Hexagons.Point 100 100
    , origin = Hexagons.Point 0 0
}

-- colordefs


white =
    rgb255 255 255 255


black =
    rgb255 0 0 0


red =
    rgb255 230 57 70


mint =
    rgb255 241 250 238


lightblue =
    rgb255 168 218 220


medblue =
    rgb255 69 123 157


darkblue =
    rgb255 29 53 87


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view model =
    layout [] <|
        column [ width fill, height fill ]
            -- Headerbar
            [ row
                [ Background.color darkblue
                , width fill
                , paddingXY 10 20
                , height shrink
                , Font.color mint
                ]
                [ el [] (text "HexHeim")
                ]

            -- Main
            , row
                [ Background.color mint
                , width fill
                , height (fillPortion 10)
                ]
                [ Element.html <|
                    svg
                        [ Svga.height "600"
                        , Svga.viewBox "0 0 1000 600"
                        ]
                        [ rect [ Svga.x "10", Svga.y "10", Svga.width "100", Svga.height "100" ] [] ]
                ]

            -- Footer
            , row
                [ Background.color black
                , Font.color white
                , Font.size 13
                , width fill
                , height shrink
                , padding 5
                ]
                [ el [] (text "Made by @Swendude.")
                ]
            ]
