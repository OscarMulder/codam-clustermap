module Dragmap exposing (MapSettings, Model, Msg(..), init, update, view, viewUsedImacs)

{-| This Dragmap module ...
-}

import Asset exposing (Image)
import Bootstrap.Button as Button
import Bootstrap.Popover as Popover
import Html exposing (Html, a, button, div, img, p, text)
import Html.Attributes exposing (class, href, id, src, style, target)
import Html.Keyed as Keyed
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
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
    { hostfile : String
    , mapImage : Asset.Image
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                group =
                    dragActiveBy delta hostGroup
            in
            ( { model | hostModel = group }, Cmd.none )

        StartDragging id ->
            let
                group =
                    startDragging id hostGroup
            in
            ( { model | hostModel = group }, Cmd.none )

        StopDragging ->
            let
                group =
                    stopDragging hostGroup
            in
            ( { model | hostModel = group }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model



-- VIEW


{-| The view is rendered based on if the http request for hosts and sessions
succeed. If they don't the view can sometimes be rendered using "old" data. In
that case a warning is displayed. The case where the hostrequest fails but we
still have a hostmodel shouldn't happen because the hostrequest is done only
once. It is still handled just to be sure.
-}
view : Model -> Html Msg
view model =
    viewMap model sessionlist hostmodel


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
viewIcons model  hostmodel =
    List.map (viewKeyedIcon model  hostmodel.mapSettings) hostmodel.hostList


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
allHosts { mapSettings, movingHost, hostList } =
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
            hostmodel

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

