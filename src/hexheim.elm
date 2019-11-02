module MapExample exposing (main)

import Browser exposing (Document)
import Debug
import Dict exposing (Dict)
import Draggable
import Element exposing (Element, column, el, padding, row)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Hexagons.Hex exposing (..)
import Hexagons.Layout exposing (..)
import Hexagons.Map exposing (..)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import List.Zipper exposing (Zipper, fromCons)
import Maybe exposing (withDefault)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Svg.Lazy exposing (lazy, lazy2, lazy3)
import Task
import VirtualDom exposing (attribute)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Draggable.subscriptions DragMsg model.drag



-- MODEL


orientationLayoutFlat : Orientation
orientationLayoutFlat =
    { forward_matrix =
        { f0 = 3.0 / 2.0
        , f1 = 0.0
        , f2 = sqrt 3.0 / 2.0
        , f3 = sqrt 3.0
        }
    , inverse_matrix =
        { f0 = 2.0 / 3.0
        , f1 = 0.0
        , f2 = -1.0 / 3.0
        , f3 = sqrt 3.0 / 3.0
        }
    , start_angle = 0.0
    }


layout =
    { orientation = orientationLayoutFlat
    , size = ( 20.0, 20.0 )
    , origin = ( 0.0, 0.0 )
    }


mapWidth : Int
mapWidth =
    50


mapHeight : Int
mapHeight =
    30



-- colordefs


white =
    Element.rgb255 255 255 255


black =
    Element.rgb255 0 0 0


lightgrey =
    Element.rgb255 200 200 200


red =
    Element.rgb255 230 57 70


mint =
    Element.rgb255 241 250 238


lightblue =
    Element.rgb255 168 218 220


medblue =
    Element.rgb255 69 123 157


darkblue =
    Element.rgb255 29 53 87


watergray =
    "#86a39c"


landgray =
    "#b7af92"


bordergray =
    "#5a6046"


minZoom =
    0.1


maxZoom =
    5


stepZoom =
    0.1


type Zoom
    = Factor Float
    | Normal


type CellType
    = Grass
    | Void


typeView : CellType -> (Hash -> String -> List (Svg Msg))
typeView ctype =
    case ctype of
        Grass ->
            grassView

        Void ->
            toPolygon


type alias TypeMap =
    { typeView : CellType -> (Hash -> String -> List (Svg Msg))
    , hexType : Dict Hash CellType
    , void : CellType
    }


type alias Model =
    { map : Map
    , gridLayout : Layout
    , typeMap : TypeMap
    , viewBoxX : Float
    , viewBoxY : Float
    , drag : Draggable.State ()
    , zoom : Zoom
    , zoomChoice : Float
    }


emptyModel : Model
emptyModel =
    { map = rectangularFlatTopMap mapHeight mapWidth
    , gridLayout = layout
    , typeMap =
        { typeView = typeView
        , hexType = Dict.empty
        , void = Void
        }
    , viewBoxX = 0
    , viewBoxY = 0
    , drag = Draggable.init
    , zoom = Factor 0.9
    , zoomChoice = 0.9
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Cmd.none )



-- UPDATE
---- Drag config


dragConfig : Draggable.Config () Msg
dragConfig =
    Draggable.basicConfig OnDragBy


type Msg
    = NoOp
    | SetHex Hash
    | PanUp
    | PanDown
    | PanLeft
    | PanRight
    | ResetView
    | ZoomSliderChange Float
    | ZoomScroll Float
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg ())


addHexType : Hash -> CellType -> TypeMap -> TypeMap
addHexType hexh ctype tmap =
    { tmap | hexType = Dict.insert hexh ctype tmap.hexType }


handleZoom : (Float -> msg) -> Svg.Attribute msg
handleZoom onZoom =
    let
        alwaysPreventDefaultAndStopPropagation msg =
            { message = msg, stopPropagation = True, preventDefault = True }

        zoomDecoder : Decoder msg
        zoomDecoder =
            Decode.float
                |> Decode.field "deltaY"
                |> Decode.map onZoom
    in
    Html.Events.custom
        "wheel"
    <|
        Decode.map alwaysPreventDefaultAndStopPropagation zoomDecoder


normalZoom : Zoom -> Bool
normalZoom z =
    case z of
        Normal ->
            True

        _ ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetHex cell ->
            ( { model | typeMap = addHexType cell Grass model.typeMap }, Cmd.none )

        PanUp ->
            ( { model | viewBoxY = model.viewBoxY + 10 }, Cmd.none )

        PanDown ->
            ( { model | viewBoxY = model.viewBoxY - 10 }, Cmd.none )

        PanLeft ->
            ( { model | viewBoxX = model.viewBoxX + 10 }, Cmd.none )

        PanRight ->
            ( { model | viewBoxX = model.viewBoxX - 10 }, Cmd.none )

        ZoomSliderChange f ->
            let
                fcapped =
                    clamp minZoom maxZoom f
            in
            ( { model | zoom = Factor fcapped, zoomChoice = fcapped }, Cmd.none )

        ZoomScroll dy ->
            let
                posneg =
                    case dy < 0 of
                        False ->
                            stepZoom

                        True ->
                            negate stepZoom

                fcapped =
                    clamp minZoom maxZoom (model.zoomChoice + posneg)
            in
            ( { model | zoom = Factor fcapped, zoomChoice = fcapped }, Cmd.none )

        ResetView ->
            ( { model | zoom = Factor 0.9, zoomChoice = 0.9, viewBoxY = 0, viewBoxX = 0 }, Cmd.none )

        OnDragBy ( dx, dy ) ->
            ( { model | viewBoxX = model.viewBoxX - dx, viewBoxY = model.viewBoxY - dy }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model



-- VIEW


svgWidth =
    800


svgHeight =
    600


viewBoxStringCoords : Model -> String
viewBoxStringCoords model =
    let
        sizeWidth =
            Tuple.first layout.size

        sizeHeight =
            Tuple.second layout.size

        cellWidth =
            2 * sizeWidth

        cellHeight =
            sqrt 3 * sizeHeight

        rows =
            toFloat mapHeight

        cols =
            toFloat mapWidth

        width =
            cols * (0.75 * cellWidth) + (0.25 * cellWidth)

        height =
            (cellHeight * rows) + (0.5 * cellHeight)
    in
    case model.zoom of
        Normal ->
            String.fromFloat model.viewBoxX
                ++ " "
                ++ String.fromFloat model.viewBoxY
                ++ " "
                ++ String.fromFloat width
                ++ " "
                ++ String.fromFloat height

        Factor n ->
            let
                newWidth =
                    width / n

                newHeight =
                    height / n

                newX =
                    (width / 2) - (newWidth / 2) + model.viewBoxX

                newY =
                    (height / 2) - (newHeight / 2) + model.viewBoxY
            in
            String.fromFloat newX
                ++ " "
                ++ String.fromFloat newY
                ++ " "
                ++ String.fromFloat newWidth
                ++ " "
                ++ String.fromFloat newHeight


view : Model -> Document Msg
view model =
    { title = "HexHeim"
    , body =
        List.singleton <|
            Element.layout [] <|
                column [ Element.width Element.fill, Element.height Element.fill ]
                    -- Headerbar
                    [ row
                        [ Background.color darkblue
                        , Element.width Element.fill
                        , Element.paddingXY 10 20
                        , Element.height Element.shrink
                        , Font.color mint
                        ]
                        [ el [] (Element.text "HexHeim")
                        ]

                    -- Main
                    , row
                        [ Background.color mint
                        , Element.width Element.fill
                        , Element.height (Element.fillPortion 10)
                        ]
                        [ Element.el
                            [ Border.color black
                            , Border.widthEach { bottom = 0, left = 0, right = 2, top = 0 }
                            , Background.color lightgrey
                            , Element.width <| Element.px svgWidth
                            , Element.alignLeft
                            , Element.height Element.fill
                            ]
                          <|
                            Element.html <|
                                svg
                                    [ version "1.2"
                                    , baseProfile "tiny"
                                    , Svg.Attributes.width (String.fromInt svgWidth)
                                    , Svg.Attributes.height (String.fromInt svgHeight)
                                    , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
                                    , viewBox (viewBoxStringCoords model)
                                    , Draggable.mouseTrigger () DragMsg
                                    , handleZoom ZoomScroll
                                    ]
                                    [ lazy hexGrid model
                                    ]

                        -- Control Element
                        , Element.column [ Element.width Element.fill, Element.height Element.fill ]
                            [ Element.row [ Element.spacing 10, Element.padding 10, Element.width Element.fill, Element.height <| Element.px 50 ]
                                [ Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanUp, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "v" }
                                , Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanDown, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "^" }
                                , Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanRight, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "<" }
                                , Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanLeft, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text ">" }
                                ]
                            , Element.row [ Element.spacing 10, Element.padding 10, Element.width Element.fill, Element.height <| Element.px 50 ]
                                [ Input.slider
                                    [ Background.color lightgrey
                                    , Element.padding 10
                                    ]
                                    { onChange = ZoomSliderChange
                                    , label = Input.labelAbove [] <| Element.text ("Zoom (x " ++ (String.left 3 <| String.fromFloat model.zoomChoice) ++ ")")
                                    , min = minZoom
                                    , max = maxZoom
                                    , value = model.zoomChoice
                                    , thumb = Input.defaultThumb
                                    , step = Just stepZoom
                                    }
                                ]
                            , Element.row [ Element.spacing 10, Element.padding 10, Element.width Element.fill, Element.height <| Element.px 50 ]
                                [ Input.button [ Background.color lightgrey, Element.padding 10 ] { onPress = Just ResetView, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "Reset View" }
                                ]
                            ]
                        ]

                    -- Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanReset, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "Center View" }
                    -- , Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanLeft, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "Normal Zoom" }
                    -- Footer
                    , row
                        [ Background.color black
                        , Font.color white
                        , Font.size 13
                        , Element.width Element.fill
                        , Element.height Element.shrink
                        , padding 5
                        ]
                        [ el [] (Element.text "Made by @Swendude.")
                        ]
                    ]
    }


hexGrid : Model -> Html Msg
hexGrid model =
    let
        hexView : Hash -> (Hash -> String -> List (Svg Msg))
        hexView hexLocation =
            case Dict.get hexLocation model.typeMap.hexType of
                Just ctype ->
                    model.typeMap.typeView ctype

                Nothing ->
                    model.typeMap.typeView model.typeMap.void

        toSvg : Hash -> String -> Svg Msg
        toSvg hexLocation cornersCoords =
            g
                []
                (hexView hexLocation hexLocation cornersCoords)
    in
    g
        [ Svg.Attributes.transform
            ("translate("
                ++ String.fromFloat ((Tuple.first model.gridLayout.size * 2) / 2)
                ++ " "
                ++ String.fromFloat ((Tuple.second model.gridLayout.size * sqrt 3) / 2)
                ++ ")"
            )
        ]
    <|
        List.map2 toSvg
            (List.map getCellKey (Dict.toList model.map))
            (List.map (pointsToString << mapPolygonCorners << getCell) (Dict.toList model.map))


{-| Helper to convert points to SVG string coordinates
-}
pointsToString : List Point -> String
pointsToString points =
    String.join " " (List.map pointToStringCoords points)


{-| Helper to convert points to SVG string coordinates
-}
pointToStringCoords : Point -> String
pointToStringCoords ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


getCell : ( Hash, Hex ) -> Hex
getCell ( key, hex ) =
    hex


getCellKey : ( Hash, Hex ) -> Hash
getCellKey ( key, hex ) =
    key


mapPolygonCorners : Hex -> List Point
mapPolygonCorners =
    polygonCorners layout


rectangularFlatTopMap : Int -> Int -> Map
rectangularFlatTopMap height width =
    let
        qlist =
            List.range 0 (width - 1)

        createHex : Int -> Int -> Hex
        createHex qv rv =
            Hexagons.Hex.intFactory ( qv, rv )

        widthRowLine : Int -> List Hex
        widthRowLine qv =
            let
                offset =
                    qv // 2
            in
            List.map (createHex qv) <|
                List.range -offset (height - 1 - offset)

        allLines : List Hex
        allLines =
            List.concat <|
                List.map widthRowLine qlist

        makeDictRecord : Hex -> ( Hash, Hex )
        makeDictRecord hex =
            ( hashHex hex, hex )
    in
    Dict.fromList <| List.map makeDictRecord <| allLines



-- CellViews


{-| Default cellView
-}
toPolygon : Hash -> String -> List (Svg Msg)
toPolygon hexLocation cornersCoords =
    [ polygon
        [ Svg.Attributes.style "cursor: pointer"

        -- , attribute "vector-effect" "non-scaling-size"
        , stroke "#000000"
        , strokeWidth "1px"
        , fill watergray
        , strokeOpacity "0.1"
        , points cornersCoords
        , Svg.Events.onMouseDown <|
            SetHex hexLocation
        ]
        []
    ]


grassView : Hash -> String -> List (Svg Msg)
grassView hexLocation cornersCoords =
    [ polygon
        [ Svg.Attributes.style "cursor: pointer"
        , attribute "shape-rendering" "optimizeSpeed"

        -- , attribute "vector-effect" "non-scaling-size"
        , stroke "#003400"
        , strokeWidth "5px"
        , fill "None"
        , points cornersCoords

        -- , strokeOpacity "0.1"
        , Svg.Events.onMouseDown <|
            SetHex hexLocation
        ]
        []
    ]
