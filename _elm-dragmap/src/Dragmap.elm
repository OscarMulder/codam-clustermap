module Dragmap exposing (MapSettings, Model, Msg(..), init, update, view, subscriptions)

{-| This Dragmap module ...
-}

import Asset exposing (Image)
import Bootstrap.Button as Button
import Bootstrap.Popover as Popover
import Draggable
import Draggable.Events exposing (onClick, onDragBy, onDragEnd, onDragStart)
import File.Download as Download
import Html exposing (Html, a, button, div, img, p, text)
import Html.Attributes exposing (class, href, id, src, style, target)
import Html.Keyed as Keyed
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Json.Encode as Encode
import Maybe exposing (withDefault)
import Platform.Cmd
import Time



-- MODEL


{-| Contains the settings used to render the map.
These are passed to the init function.
The sizes are all in pixels.
-}
type alias MapSettings =
    { height : Int
    , width : Int
    , activeIconSize : Int
    , emptyIconSize : Int
    }


{-| Holds the mapSettings loaded from the host json and the list of hosts. The
mapSettings width and height are needed to recalculate the icon positions if the
map size changes. The IconSize values are not used. The movingHosts holds the
host that is currently being dragged (if there is one).
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


init : String -> Image -> MapSettings -> ( Model, Cmd Msg )
init hostfile image mapsettings =
    ( { mapImage = image
      , mapSettings = mapsettings
      , hostModel = getHosts hostfile
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
            ( model, downloadJson hostlist )

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


{-| Renders the map based on the uploaded map image and hostfile.
-}
view : Model -> Html Msg
view model =
    case model.hostModel of
        Nothing ->
            p [] [ text "Can't render map" ]
        
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


{-| Renders the list of icons with their correct positions which will be
displayed on the map.
-}
viewIcons : Model -> HostModel -> List ( String, Html Msg )
viewIcons model hostmodel =
    List.map (viewKeyedIcon model  hostmodel.mapSettings) (allHosts hostmodel)


{-| Keys the icon for faster dom rendering.
-}
viewKeyedIcon : Model -> MapSettings -> Host -> ( String, Html Msg )
viewKeyedIcon model  hostmapsettings host =
    ( host.id, viewIcon model  hostmapsettings host )


{-| Renders the icon and gives it correct position values. Can be either an
empty host icon or an active session icon.
-}
viewIcon : Model -> MapSettings -> Host -> Html Msg
viewIcon model hostmapsettings host =
    let
        offset =
            model.mapSettings.emptyIconSize // 2
    in
    div
        [ Draggable.mouseTrigger host.id DragMsg
        , class "imac-location"
        , style "left"
            <| String.fromInt ((calculateLeft model hostmapsettings
            <| Tuple.first host.position) - offset) ++ "px"
        , style "top"
            <| String.fromInt ((calculateTop model hostmapsettings
            <| Tuple.second host.position) - offset) ++ "px"
        ]
        [ Popover.config
            (Button.button
                [ Button.attrs
                    <| id (hostToId host.id)
                    :: Popover.onHover host.popState (PopoverMsg host.id)
                ]
                [ Asset.emptyHost model.mapSettings.emptyIconSize ]
            )
            |> Popover.top
            |> Popover.content [] [ text (hostToId host.id) ]
            |> Popover.view host.popState
        ]


-- HOST HELPERS


{-| Recalculates the left position value using the base map size (from the
hostModel) and the actual mapsize.
-}
calculateLeft : Model -> MapSettings -> Int -> Int
calculateLeft model hostmapsettings left =
    round
        <| (toFloat left / toFloat hostmapsettings.width) * toFloat model.mapSettings.width


{-| Recalculates the top position value using the base map size (from the
hostModel) and the actual mapsize.
-}
calculateTop : Model -> MapSettings -> Int -> Int
calculateTop model hostmapsettings top =
    round
        <| (toFloat top / toFloat hostmapsettings.height) * toFloat model.mapSettings.height


hostToId : String -> String
hostToId host =
    Maybe.withDefault host (List.head (String.split "." host))



-- DRAG HELPERS


allHosts : HostModel -> List Host
allHosts { mapSettings, hostList, movingHost } =
    movingHost
        |> Maybe.map (\a -> a :: hostList)
        |> Maybe.withDefault hostList


startDragging : String -> HostModel -> HostModel
startDragging id ({ mapSettings, hostList, movingHost } as hostmodel) =
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
            Nothing


hostModelDecoder : Decoder HostModel
hostModelDecoder =
    Field.require "mapsettings" mapSettingsDecoder <|
        \mapsettings ->
            Field.require "hosts" hostListDecoder <|
                \hostlist ->
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
            Field.require "left" Decode.int <|
                \left ->
                    Field.require "top" Decode.int <|
                        \top ->
                            Decode.succeed
                                { id = hostname
                                , position = ( left, top )
                                , popState = Popover.initialState
                                }


downloadJson : List Host -> Cmd msg
downloadJson hostlist =
    Download.string "new-hostlist.json" "application/json" (hostlistToJson hostlist)


hostlistToJson : List Host -> String
hostlistToJson hostlist =
    Encode.encode 2 (Encode.list hostConverter hostlist)


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

