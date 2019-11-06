module Hexheim exposing (main)

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
import List.Extra exposing (cartesianProduct)
import List.Zipper exposing (Zipper, fromCons)
import Maybe exposing (withDefault)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Svg.Keyed exposing (node)
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


mapWidth : Int
mapWidth =
    100


mapHeight : Int
mapHeight =
    60



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


watergrayshade =
    "#43524e"


landgray =
    "#b7af92"


landgrayshade =
    "#5c5849"


bordergray =
    "#5a6046"


bgcolor =
    "#444f4c"


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


typeView : CellType -> (Hash -> String -> Svg Msg)
typeView ctype =
    case ctype of
        Grass ->
            grassView

        Void ->
            toPolygon


type alias TypeMap =
    { typeView : CellType -> (Hash -> String -> Svg Msg)
    , void : CellType
    }


type alias HexInfo =
    { hex : Hex
    , hexType : CellType
    }


type alias HexHeimMap =
    Dict Hash HexInfo


hexHeimToNormalMap : HexHeimMap -> Map
hexHeimToNormalMap =
    Dict.map mapHexFromInfo


mapHexFromInfo : Hash -> HexInfo -> Hex
mapHexFromInfo _ hi =
    hi.hex


type alias Model =
    { map : HexHeimMap
    , gridLayout : Layout
    , typeMap : TypeMap
    , viewBoxX : Float
    , viewBoxY : Float
    , drag : Draggable.State ()
    , zoom : Zoom
    , zoomChoice : Float
    , brushChoice : Int
    , viewPortLimits : ( Point, Point )
    }


emptyModel : Model
emptyModel =
    { map = rectangularFlatTopMap Void mapHeight mapWidth
    , gridLayout =
        { orientation = orientationLayoutFlat
        , size = ( 20.0, 20.0 )
        , origin = ( 0.0, 0.0 )
        }
    , typeMap =
        { typeView = typeView
        , void = Void
        }
    , viewBoxX = 0
    , viewBoxY = 0
    , drag = Draggable.init
    , zoom = Factor 0.9
    , zoomChoice = 0.9
    , brushChoice = 1
    , viewPortLimits = ( ( 0.0, 0.0 ), ( 0.0, 0.0 ) )
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { emptyModel | viewPortLimits = initViewPortLimits emptyModel }, Cmd.none )


initViewPortLimits : Model -> ( Point, Point )
initViewPortLimits model =
    let
        vbc =
            viewBoxCoords model
    in
    ( ( vbc.x - (0.2 * vbc.width), vbc.y - (0.2 * vbc.height) ), ( 0.2 * vbc.width, 0.2 * vbc.height ) )



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
    | BrushSliderChange Int
    | ZoomScroll Float
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg ())


updateHexesType : CellType -> List Hash -> HexHeimMap -> HexHeimMap
updateHexesType ct hexes hhmap =
    List.foldl (hexFold ct) hhmap hexes


updateHexType : CellType -> Maybe HexInfo -> Maybe HexInfo
updateHexType ct hexInfo =
    case hexInfo of
        Just hexInfo_ ->
            Just { hexInfo_ | hexType = ct }

        Nothing ->
            Nothing


hexFold : CellType -> Hash -> HexHeimMap -> HexHeimMap
hexFold ct hex hhmap =
    Dict.update hex (updateHexType ct) hhmap



-- setHexType : Hash -> CellType -> HexHeimMap -> HexHeimMap
-- setHexType hexh ctype hhmap =
--     Dict.insert hexh


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
            let
                defaultdirections =
                    List.map Hexagons.Hex.direction [ NE, E, SE, SW, W, NW ]

                directions =
                    hexCircle (hashToHex cell) model.brushChoice

                hexcell =
                    Maybe.withDefault (Hexagons.Hex.intFactory ( 0, 0 )) (Dict.get cell (hexHeimToNormalMap model.map))

                neighbors : List Hash
                neighbors =
                    [ cell ] ++ (List.map hashHex <| List.map (\dir -> Hexagons.Hex.add hexcell dir) directions)
            in
            ( { model | map = updateHexesType Grass neighbors model.map }, Cmd.none )

        PanUp ->
            let
                minY =
                    Tuple.second <| Tuple.first model.viewPortLimits

                maxY =
                    Tuple.second <| Tuple.second model.viewPortLimits

                newY =
                    clamp minY maxY (model.viewBoxY + 10)
            in
            ( { model | viewBoxY = newY }, Cmd.none )

        PanDown ->
            let
                minY =
                    Tuple.second <| Tuple.first model.viewPortLimits

                maxY =
                    Tuple.second <| Tuple.second model.viewPortLimits

                newY =
                    clamp minY maxY (model.viewBoxY - 10)
            in
            ( { model | viewBoxY = newY }, Cmd.none )

        PanLeft ->
            let
                minX =
                    Tuple.first <| Tuple.first model.viewPortLimits

                maxX =
                    Tuple.first <| Tuple.second model.viewPortLimits

                newX =
                    clamp minX maxX (model.viewBoxX + 10)
            in
            ( { model | viewBoxX = newX }, Cmd.none )

        PanRight ->
            let
                minX =
                    Tuple.first <| Tuple.first model.viewPortLimits

                maxX =
                    Tuple.first <| Tuple.second model.viewPortLimits

                newX =
                    clamp minX maxX (model.viewBoxX - 10)
            in
            ( { model | viewBoxX = newX }, Cmd.none )

        ZoomSliderChange f ->
            let
                fcapped =
                    clamp minZoom maxZoom f
            in
            ( { model | zoom = Factor fcapped, zoomChoice = fcapped }, Cmd.none )

        BrushSliderChange f ->
            ( { model | brushChoice = f }, Cmd.none )

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
            let
                smoothdx =
                    dx * 1.5

                smoothdy =
                    dy * 1.5

                minX =
                    Tuple.first <| Tuple.first model.viewPortLimits

                minY =
                    Tuple.second <| Tuple.first model.viewPortLimits

                maxX =
                    Tuple.first <| Tuple.second model.viewPortLimits

                maxY =
                    Tuple.second <| Tuple.second model.viewPortLimits

                newX =
                    clamp minX maxX (model.viewBoxX - smoothdx)

                newY =
                    clamp minY maxY (model.viewBoxY - smoothdy)
            in
            ( { model | viewBoxX = newX, viewBoxY = newY }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model



-- VIEW


svgWidth =
    800


svgHeight =
    610


type alias ViewBoxCoords =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


viewBoxCoords : Model -> ViewBoxCoords
viewBoxCoords model =
    let
        sizeWidth =
            Tuple.first model.gridLayout.size

        sizeHeight =
            Tuple.second model.gridLayout.size

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
            { x = model.viewBoxX, y = model.viewBoxY, width = width, height = height }

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
            { x = newX
            , y = newY
            , width = newWidth
            , height = newHeight
            }


stringFromViewBoxCoords : ViewBoxCoords -> String
stringFromViewBoxCoords vbc =
    String.fromFloat vbc.x ++ " " ++ String.fromFloat vbc.y ++ " " ++ String.fromFloat vbc.width ++ " " ++ String.fromFloat vbc.height


view : Model -> Document Msg
view model =
    let
        vbc =
            viewBoxCoords model
    in
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
                            , Background.color lightgrey
                            , Element.width <| Element.px svgWidth
                            , Element.alignLeft
                            , Element.height Element.fill
                            ]
                          <|
                            Element.html <|
                                svg
                                    [ version "1.1"
                                    , Svg.Attributes.width (String.fromInt svgWidth)
                                    , Svg.Attributes.height (String.fromInt svgHeight)
                                    , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
                                    , viewBox <| stringFromViewBoxCoords vbc
                                    , Draggable.mouseTrigger () DragMsg
                                    , handleZoom ZoomScroll
                                    ]
                                    [ Svg.rect
                                        [ Svg.Attributes.x (String.fromFloat (vbc.x - vbc.width))
                                        , Svg.Attributes.y (String.fromFloat (vbc.y - vbc.height))
                                        , Svg.Attributes.height (String.fromFloat (vbc.height * 2.5))
                                        , Svg.Attributes.width (String.fromFloat (vbc.width * 2.5))
                                        , Svg.Attributes.fill bgcolor
                                        ]
                                        []
                                    , lazy2 hexGrid Grass model
                                    , lazy2 hexGrid Void model
                                    ]

                        -- Control Element
                        , Element.column [ Element.width Element.fill, Element.height Element.fill ]
                            [ Element.row [ Element.spacing 10, Element.padding 10, Element.width Element.fill, Element.height <| Element.px 50 ]
                                [ Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanUp, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "v" }
                                , Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanDown, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "^" }
                                , Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanRight, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "<" }
                                , Input.button [ Background.color lightgrey, Element.width <| Element.px 45 ] { onPress = Just PanLeft, label = Element.el [ Element.centerX, Element.centerY ] <| Element.text ">" }
                                ]
                            , Element.row [ Element.spacing 10, Element.padding 10, Element.width <| Element.fillPortion 5, Element.height <| Element.px 50 ]
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
                            , Element.row [ Element.spacing 10, Element.padding 10, Element.width <| Element.fillPortion 5, Element.height <| Element.px 50 ]
                                [ Input.slider
                                    [ Background.color lightgrey
                                    , Element.padding 10
                                    ]
                                    { onChange = round >> BrushSliderChange
                                    , label = Input.labelAbove [] <| Element.text ("Brush size (" ++ String.fromInt model.brushChoice ++ ")")
                                    , min = 1.0
                                    , max = 5.0
                                    , value = toFloat model.brushChoice
                                    , thumb = Input.defaultThumb
                                    , step = Just 1
                                    }
                                ]
                            ]
                        ]

                    -- Footer
                    , row
                        [ Background.color black
                        , Font.color white
                        , Font.size 13
                        , Element.width Element.fill
                        , Element.height Element.shrink
                        , padding 5
                        , Element.spacing 10
                        ]
                        [ el [ Element.alignLeft ] (Element.text "Made by @Swendude.")
                        , el [ Element.alignRight ]
                            (Element.newTabLink [ Font.color (Element.rgb255 100 100 255), Font.underline ]
                                { url = "https://github.com/Swendude/Hexheim"
                                , label = Element.text "Check source"
                                }
                            )
                        ]
                    ]
    }


hexGrid : CellType -> Model -> Html Msg
hexGrid cellType model =
    let
        hexView : Hash -> (Hash -> String -> Svg Msg)
        hexView hexLocation =
            case Dict.get hexLocation model.map of
                Just hexInfo ->
                    model.typeMap.typeView hexInfo.hexType

                Nothing ->
                    model.typeMap.typeView model.typeMap.void

        toSvg : Hash -> String -> ( String, Svg Msg )
        toSvg hexLocation cornersCoords =
            ( hashToString hexLocation, lazy2 (hexView hexLocation) hexLocation cornersCoords )

        cellList : List ( Hash, Hex )
        cellList =
            List.map (\( hash, hexInfo ) -> ( hash, hexInfo.hex )) <| List.filter (\( hash, hexInfo ) -> hexInfo.hexType == cellType) (Dict.toList model.map)
    in
    Svg.Keyed.node "g"
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
            (List.map getCellKey cellList)
            (List.map (pointsToString << polygonCorners model.gridLayout << getCell) cellList)


{-| Helper to convert points to SVG string coordinates
-}
pointsToString : List Point -> String
pointsToString points =
    String.join " " (List.map pointToStringCoords points)


{-| Helper to print Hashes
-}
hashToString : Hash -> String
hashToString ( q, r, s ) =
    "( " ++ String.fromInt q ++ ", " ++ String.fromInt r ++ ", " ++ String.fromInt s ++ " )"


{-| Helper to convert points to SVG string coordinates
-}
pointToStringCoords : Point -> String
pointToStringCoords ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


getCell : ( Hash, Hex ) -> Hex
getCell ( key, hex ) =
    hex


hashToHex : Hash -> Hex
hashToHex =
    IntCubeHex


getCellKey : ( Hash, Hex ) -> Hash
getCellKey ( key, hex ) =
    key


listToHex : List Int -> Maybe Hex
listToHex list =
    case list of
        [ a, b, c ] ->
            Just <| IntCubeHex ( a, b, c )

        _ ->
            Nothing


hexCircle : Hex -> Int -> List Hex
hexCircle center radius =
    let
        ranges =
            [ List.range (negate radius) radius
            , List.range (negate radius) radius
            , List.range (negate radius) radius
            ]
    in
    List.filterMap listToHex <| List.filter (\h -> (==) 0 (List.foldl (+) 0 h)) <| cartesianProduct ranges


rectangularFlatTopMap : CellType -> Int -> Int -> HexHeimMap
rectangularFlatTopMap defaultCellType height width =
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

        makeDictRecord : Hex -> ( Hash, HexInfo )
        makeDictRecord hex =
            ( hashHex hex, { hex = hex, hexType = defaultCellType } )
    in
    Dict.fromList <| List.map makeDictRecord <| allLines



-- CellViews


{-| Default cellView
-}
toPolygon : Hash -> String -> Svg Msg
toPolygon hexLocation cornersCoords =
    polygon
        [ Svg.Attributes.style "cursor: pointer"

        -- , attribute "vector-effect" "non-scaling-size"
        , stroke watergrayshade
        , strokeWidth "1px"
        , fill watergray
        , points cornersCoords
        , Svg.Events.onMouseDown <|
            SetHex hexLocation
        ]
        []


grassView : Hash -> String -> Svg Msg
grassView hexLocation cornersCoords =
    polygon
        [ Svg.Attributes.style "cursor: pointer"
        , attribute "shape-rendering" "optimizeSpeed"
        , stroke landgrayshade
        , strokeWidth "1px"
        , fill landgray
        , points cornersCoords

        -- , strokeOpacity "0.1"
        , Svg.Events.onMouseDown <|
            SetHex hexLocation
        ]
        []
