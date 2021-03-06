module Dragmap exposing (MapSettings, Model, Msg(..), init, subscriptions, update, view)

{-| This Dragmap module provides the basic functionality for implementing a
dragmap. It just needs a hostfile (json) and map image (svg).
-}

import Asset exposing (Image)
import Bootstrap.Button as Button
import Bootstrap.Popover as Popover
import Draggable
import Draggable.Events exposing (onDragBy, onDragEnd, onDragStart)
import File.Download as Download
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, id, src, style)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Json.Encode as Encode
import Maybe exposing (withDefault)



-- MODEL


{-| Contains the settings used to render the map.
These are read from the JSON file or defaults are used.
The sizes are all in pixels.
-}
type alias MapSettings =
    { height : Int
    , width : Int
    , activeIconSize : Int
    , emptyIconSize : Int
    }


{-| Holds the mapSettings (from the json decode) and the list of hosts.
The mapSettings are only hold here to ease the decoding.
The movingHosts holds the host that is currently being dragged (if there is one).
-}
type alias HostModel =
    { mapSettings : MapSettings
    , hostList : List Host
    , movingHost : Maybe Host
    }


{-| Defines one host. Position is used as ( left, top ) in pixels.
-}
type alias Host =
    { id : String
    , position : ( Int, Int )
    , popState : Popover.State
    }


type alias Model =
    { mapImage : Asset.Image
    , mapSettings : MapSettings
    , hostModel : Maybe HostModel
    , drag : Draggable.State String
    }



-- INIT


{-| Since the hostfile decode might fail and it might not have a mapsetting, a
default value is provided, but it's not actually used since the icons can't be
rendered and the map won't be displayed.
-}
init : String -> Image -> ( Model, Cmd Msg )
init hostfile image =
    let
        hostmodel =
            getHosts hostfile

        mapset =
            case hostmodel of
                Nothing ->
                    MapSettings 1325 1026 60 25

                Just hmodel ->
                    hmodel.mapSettings
    in
    ( { mapImage = image
      , mapSettings = mapset
      , hostModel = hostmodel
      , drag = Draggable.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = PopoverMsg String Popover.State
    | OnDragBy Draggable.Delta
    | StartDragging String
    | StopDragging
    | DragMsg (Draggable.Msg String)
    | DownloadJson


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownloadJson ->
            let
                hostlist =
                    case model.hostModel of
                        Nothing ->
                            []

                        Just hmodel ->
                            allHosts hmodel
            in
            ( model, downloadJson hostlist model.mapSettings )

        PopoverMsg host state ->
            let
                updatePopState session =
                    if String.contains host session.id then
                        { session | popState = state }

                    else
                        session
            in
            let
                newhostModel =
                    case model.hostModel of
                        Nothing ->
                            Nothing

                        Just hmodel ->
                            Just { hmodel | hostList = List.map updatePopState hmodel.hostList }
            in
            ( { model | hostModel = newhostModel }, Cmd.none )

        OnDragBy delta ->
            let
                newhostmodel =
                    case model.hostModel of
                        Nothing ->
                            Nothing

                        Just hmodel ->
                            Just <| dragActiveBy delta hmodel
            in
            ( { model | hostModel = newhostmodel }, Cmd.none )

        StartDragging id ->
            let
                newhostmodel =
                    case model.hostModel of
                        Nothing ->
                            Nothing

                        Just hmodel ->
                            Just <| startDragging id hmodel
            in
            ( { model | hostModel = newhostmodel }, Cmd.none )

        StopDragging ->
            let
                newhostmodel =
                    case model.hostModel of
                        Nothing ->
                            Nothing

                        Just hmodel ->
                            Just <| stopDragging hmodel
            in
            ( { model | hostModel = newhostmodel }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model



-- VIEW


{-| Renders the map based on the uploaded map image and hostfile, if the
hostfile couldn't be decoded an error is displayed.
-}
view : Model -> Html Msg
view model =
    case model.hostModel of
        Nothing ->
            p [] [ text "Error: Invalid Hostfile." ]

        Just hostmodel ->
            viewMap model hostmodel


{-| Renders the map and all the icons.
-}
viewMap : Model -> HostModel -> Html Msg
viewMap model hostmodel =
    Keyed.node "div"
        [ style "position" "relative" ]
        (( Asset.toString model.mapImage
         , img
            [ src (Asset.toString model.mapImage)
            , style "position" "relative"
            , style "height" <| String.fromInt model.mapSettings.height ++ "px"
            , style "width" <| String.fromInt model.mapSettings.width ++ "px"
            ]
            []
         )
            :: viewIcons model hostmodel
        )


{-| Renders the list of icons.
-}
viewIcons : Model -> HostModel -> List ( String, Html Msg )
viewIcons model hostmodel =
    List.map (viewKeyedIcon model) (allHosts hostmodel)


{-| Keys the icon for faster dom rendering.
-}
viewKeyedIcon : Model -> Host -> ( String, Html Msg )
viewKeyedIcon model host =
    ( host.id, viewIcon model host )


{-| Renders the icon and gives it correct position values.
Also makes the icon dragable and provides a popover with the (shortened) hostname
-}
viewIcon : Model -> Host -> Html Msg
viewIcon model host =
    let
        offset =
            model.mapSettings.emptyIconSize // 2
    in
    div
        [ Draggable.mouseTrigger host.id DragMsg
        , class "imac-location"
        , style "left" <|
            String.fromInt
                ( Tuple.first host.position - offset
                )
                ++ "px"
        , style "top" <|
            String.fromInt
                ( Tuple.second host.position - offset
                )
                ++ "px"
        ]
        [ Popover.config
            (Button.button
                [ Button.attrs <|
                    id (hostToId host.id)
                        :: Popover.onHover host.popState (PopoverMsg host.id)
                ]
                [ Asset.emptyHost model.mapSettings.emptyIconSize ]
            )
            |> Popover.top
            |> Popover.content [] [ text (hostToId host.id) ]
            |> Popover.view host.popState
        ]



-- HOST HELPERS


hostToId : String -> String
hostToId host =
    Maybe.withDefault host (List.head (String.split "." host))



-- DRAG HELPERS


allHosts : HostModel -> List Host
allHosts { hostList, movingHost } =
    movingHost
        |> Maybe.map (\a -> a :: hostList)
        |> Maybe.withDefault hostList


startDragging : String -> HostModel -> HostModel
startDragging id ({ hostList } as hostmodel) =
    let
        ( targetAsList, others ) =
            List.partition (.id >> (==) id) hostList
    in
    { hostmodel
        | hostList = others
        , movingHost = targetAsList |> List.head
    }


stopDragging : HostModel -> HostModel
stopDragging hostmodel =
    { hostmodel
        | hostList = allHosts hostmodel
        , movingHost = Nothing
    }


dragHostBy : ( Float, Float ) -> Host -> Host
dragHostBy ( dx, dy ) host =
    let
        ( x, y ) =
            host.position
    in
    { host | position = ( round (toFloat x + dx), round (toFloat y + dy) ) }


dragActiveBy : ( Float, Float ) -> HostModel -> HostModel
dragActiveBy delta group =
    { group | movingHost = group.movingHost |> Maybe.map (dragHostBy delta) }


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.customConfig
        [ onDragBy OnDragBy
        , onDragStart StartDragging
        , onDragEnd StopDragging
        ]



-- HOST DECODERS


getHosts : String -> Maybe HostModel
getHosts hostfile =
    let
        result =
            Decode.decodeString hostModelDecoder hostfile
    in
    case result of
        Ok hostmodel ->
            Just hostmodel

        Err _ ->
            let
                result2 =
                    Decode.decodeString hostListDecoder hostfile
            in
            case result2 of
                Ok hostlist ->
                    let
                        hostmodel =
                            { mapSettings = MapSettings 1325 1026 60 25
                            , hostList = hostlist
                            , movingHost = Nothing
                            }
                    in
                    Just hostmodel

                Err _ ->
                    Nothing


hostModelDecoder : Decoder HostModel
hostModelDecoder =
    Field.attempt "mapsettings" mapSettingsDecoder <|
        \maybemapsettings ->
            Field.require "hosts" hostListDecoder <|
                \hostlist ->
                    let
                        mapsettings =
                            case maybemapsettings of
                                Nothing ->
                                    MapSettings 1325 1026 60 25

                                Just settings ->
                                    settings
                    in
                    Decode.succeed
                        { mapSettings = mapsettings
                        , hostList = hostlist
                        , movingHost = Nothing
                        }


mapSettingsDecoder : Decoder MapSettings
mapSettingsDecoder =
    Field.require "heigth" Decode.int <|
        \height ->
            Field.require "width" Decode.int <|
                \width ->
                    Field.require "active-size" Decode.int <|
                        \activeIconSize ->
                            Field.require "empty-size" Decode.int <|
                                \emptyIconSize ->
                                    Decode.succeed
                                        { height = height
                                        , width = width
                                        , activeIconSize = activeIconSize
                                        , emptyIconSize = emptyIconSize
                                        }


hostListDecoder : Decoder (List Host)
hostListDecoder =
    Decode.list hostDecoder


hostDecoder : Decoder Host
hostDecoder =
    Field.require "hostname" Decode.string <|
        \hostname ->
            Field.attempt "left" Decode.int <|
                \maybeleft ->
                    Field.attempt "top" Decode.int <|
                        \maybetop ->
                            case ( maybeleft, maybetop ) of
                                ( Just left, Just top ) ->
                                    Decode.succeed
                                        { id = hostname
                                        , position = ( left, top )
                                        , popState = Popover.initialState
                                        }

                                _ ->
                                    Decode.succeed
                                        { id = hostname
                                        , position = ( 50, 50 )
                                        , popState = Popover.initialState
                                        }


downloadJson : List Host -> MapSettings -> Cmd msg
downloadJson hostlist mapsettings =
    Download.string "new-hostlist.json" "application/json" (hostmodelToJson hostlist mapsettings)


hostmodelToJson : List Host -> MapSettings -> String
hostmodelToJson hostlist mapsettings =
    Encode.encode 2 <|
        Encode.object
            [ ( "mapsettings", mapsettingsToJson mapsettings )
            , ( "hosts", hostlistToJson hostlist )
            ]


mapsettingsToJson : MapSettings -> Encode.Value
mapsettingsToJson mapsettings =
    Encode.object
        [ ( "heigth", Encode.int mapsettings.height )
        , ( "width", Encode.int mapsettings.width )
        , ( "active-size", Encode.int mapsettings.activeIconSize )
        , ( "empty-size", Encode.int mapsettings.emptyIconSize )
        ]


hostlistToJson : List Host -> Encode.Value
hostlistToJson hostlist =
    Encode.list hostConverter hostlist


hostConverter : Host -> Encode.Value
hostConverter host =
    Encode.object
        [ ( "hostname", Encode.string host.id )
        , ( "left", Encode.int (Tuple.first host.position) )
        , ( "top", Encode.int (Tuple.second host.position) )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag
