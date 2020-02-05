module Clustermap exposing (MapSettings, Model, Msg(..), init, update, view, viewUsedImacs)

{-| This Clustermap module makes it possible to display multiple clustermaps
without a lot of duplicate code. It is setup in such a way that it will work
with any map and any amount of computers.
-}

import Asset exposing (Image)
import Bootstrap.Button as Button
import Bootstrap.Popover as Popover
import Endpoint exposing (Endpoint)
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

{-| Tracks the state of a session request.
-}
type SessionRequest
    = Failure Http.Error
    | Loading
    | Success (List Session)


{-| Tracks the state of a host request.
-}
type HostRequest
    = HostFailure Http.Error
    | HostLoading
    | HostSuccess HostModel


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
map size changes. The IconSize values are not used.
-}
type alias HostModel =
    { mapSettings : MapSettings
    , hostList : List Host
    }


{-| Defines one host. Position is used as ( left, top ) in pixels.
-}
type alias Host =
    { id : String
    , position : ( Int, Int )
    , popState : Popover.State
    }


{-| Defines an active session. The host field should match one of the host id's
in the hostList. If it doesn't the session is not displayed. The username is
used in the display and to find the profile picture.
-}
type alias Session =
    { username : String
    , host : String
    }


type alias Model =
    { hostEndpoint : Endpoint
    , sessionEndpoint : Endpoint
    , mapImage : Asset.Image
    , mapSettings : MapSettings
    , activeList : List Session
    , hostModel : Maybe HostModel
    , reqS : SessionRequest
    , reqH : HostRequest
    }



-- INIT


init : Endpoint -> Endpoint -> Image -> MapSettings -> ( Model, Cmd Msg )
init hostEndpoint sessionEndpoint image mapsettings =
    ( { hostEndpoint = hostEndpoint
      , sessionEndpoint = sessionEndpoint
      , mapImage = image
      , mapSettings = mapsettings
      , activeList = []
      , hostModel = Nothing
      , reqS = Loading
      , reqH = HostLoading
      }
    , Platform.Cmd.batch [ getActiveSessions sessionEndpoint, getHosts hostEndpoint ]
    )



-- UPDATE


type Msg
    = GotSessions (Result Error (List Session))
    | GotHosts (Result Error HostModel)
    | FetchSessions Time.Posix
    | PopoverMsg String Popover.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSessions result ->
            case result of
                Ok sessionlist ->
                    ( { model | activeList = sessionlist, reqS = Success sessionlist }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | reqS = Failure err }
                    , Cmd.none
                    )

        GotHosts result ->
            case result of
                Ok hostmodel ->
                    ( { model | hostModel = Just hostmodel, reqH = HostSuccess hostmodel }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | reqH = HostFailure err }
                    , Cmd.none
                    )

        FetchSessions _ ->
            ( model
            , getActiveSessions model.sessionEndpoint
            )

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
            case newhostModel of
                Nothing ->
                    ( { model | hostModel = newhostModel }, Cmd.none )

                Just newhmodel ->
                    ( { model | hostModel = Just newhmodel, reqH = HostSuccess newhmodel }, Cmd.none )



-- VIEW


{-| The view is rendered based on if the http request for hosts and sessions
succeed. If they don't the view can sometimes be rendered using "old" data. In
that case a warning is displayed. The case where the hostrequest fails but we
still have a hostmodel shouldn't happen because the hostrequest is done only
once. It is still handled just to be sure.
-}
view : Model -> Html Msg
view model =
    case model.reqH of
        HostFailure err ->
            case model.hostModel of
                Nothing ->
                    div []
                        [ p [] [ text <| "Error retrieving hosts, can't show map. Error: " ++ httpErrorString err ]
                        ]

                Just hostmodel ->
                    case model.reqS of
                        Failure _ ->
                            div []
                                [ p [] [ text "Error retrieving sessions, map might be outdated." ]
                                , viewMap model model.activeList hostmodel
                                ]

                        Loading ->
                            text "Loading Sessions..."

                        Success sessionlist ->
                            viewMap model sessionlist hostmodel

        HostLoading ->
            text "Loading Hosts..."

        HostSuccess hostmodel ->
            case model.reqS of
                Failure _ ->
                    div []
                        [ p [] [ text "Error retrieving sessions, map might be outdated." ]
                        , viewMap model model.activeList hostmodel
                        ]

                Loading ->
                    text "Loading Sessions..."

                Success sessionlist ->
                    viewMap model sessionlist hostmodel


{-| Renders the map and all the icons.
-}
viewMap : Model -> List Session -> HostModel -> Html Msg
viewMap model sessionlist hostmodel =
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
            :: viewIcons model sessionlist hostmodel
        )


{-| Renders the list of icons with their correct positions which will be
displayed on the map.
-}
viewIcons : Model -> List Session -> HostModel -> List ( String, Html Msg )
viewIcons model sessionlist hostmodel =
    List.map (viewKeyedIcon model sessionlist hostmodel.mapSettings) hostmodel.hostList


{-| Keys the icon for faster dom rendering.
-}
viewKeyedIcon : Model -> List Session -> MapSettings -> Host -> ( String, Html Msg )
viewKeyedIcon model sessionlist hostmapsettings host =
    ( host.id, viewIcon model sessionlist hostmapsettings host )


{-| Renders the icon and gives it correct position values. Can be either an
empty host icon or an active session icon.
-}
viewIcon : Model -> List Session -> MapSettings -> Host -> Html Msg
viewIcon model sessionlist hostmapsettings host =
    let
        maybeSession =
            List.head (List.filter (hostFilter host.id) sessionlist)
    in
    let
        offset =
            case maybeSession of
                Nothing ->
                    model.mapSettings.emptyIconSize // 2

                Just _ ->
                    model.mapSettings.activeIconSize // 2
    in
    div
        [ class "imac-location"
        , style "left"
            <| String.fromInt ((calculateLeft model hostmapsettings
            <| Tuple.first host.position) - offset) ++ "px"
        , style "top"
            <| String.fromInt ((calculateTop model hostmapsettings
            <| Tuple.second host.position) - offset) ++ "px"
        ]
        (case maybeSession of
            Nothing ->
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

            Just session ->
                [ Popover.config
                    (Button.button
                        [ Button.attrs
                            <| id (hostToId host.id)
                            :: Popover.onHover host.popState (PopoverMsg host.id)
                        ]
                        [ a [ href ("https://profile.intra.42.fr/users/" ++ session.username), target "_blank" ]
                            [ img
                                [ src ("https://cdn.intra.42.fr/users/small_" ++ session.username ++ ".jpg")
                                , class "round-img"
                                , style "width"
                                    <| String.fromInt model.mapSettings.activeIconSize
                                , style "height"
                                    <| String.fromInt model.mapSettings.activeIconSize
                                ]
                                []
                            ]
                        ]
                    )
                    |> Popover.top
                    |> Popover.title [] [ text session.username ]
                    |> Popover.content [] [ text (hostToId host.id) ]
                    |> Popover.view host.popState
                ]
        )


{-| Renders a text element containing the amount of used imacs and total imacs
in "42/120" format.
-}
viewUsedImacs : Model -> Html Msg
viewUsedImacs model =
    let
        hlist =
            case model.hostModel of
                Nothing ->
                    []

                Just hmodel ->
                    hmodel.hostList
    in
    let
        totalImacs =
            String.fromInt
                <| List.length hlist

        usedImacs =
            String.fromInt
                <| List.length
                <| List.filter (hostListFilter hlist) model.activeList
    in
    text (usedImacs ++ "/" ++ totalImacs)



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


hostListFilter : List Host -> Session -> Bool
hostListFilter hostlist session =
    case List.head <| List.filter (sessionFilter session.host) hostlist of
        Nothing ->
            False

        Just _ ->
            True


sessionFilter : String -> Host -> Bool
sessionFilter sessionhost host =
    String.contains sessionhost host.id

hostFilter : String -> Session -> Bool
hostFilter host session =
    String.contains host session.host

hostToId : String -> String
hostToId host =
    Maybe.withDefault host (List.head (String.split "." host))



-- HTTP


getHosts : Endpoint -> Cmd Msg
getHosts endpoint =
    Http.get
        { url = Endpoint.toString endpoint
        , expect = Http.expectJson GotHosts hostModelDecoder
        }


getActiveSessions : Endpoint -> Cmd Msg
getActiveSessions endpoint =
    Http.get
        { url = Endpoint.toString endpoint
        , expect = Http.expectJson GotSessions sessionListDecoder
        }


httpErrorString : Error -> String
httpErrorString error =
    case error of
        Http.BadUrl text ->
            "Bad Url: " ++ text

        Http.Timeout ->
            "Http Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus response ->
            "Bad Http Status: " ++ String.fromInt response

        Http.BadBody message ->
            "Bad Http Payload: "
                ++ message



-- HOST DECODERS


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



-- SESSION DECODERS


sessionListDecoder : Decoder (List Session)
sessionListDecoder =
    Decode.list sessionDecoder


sessionDecoder : Decoder Session
sessionDecoder =
    Field.require "username" Decode.string <|
        \username ->
            Field.require "host" Decode.string <|
                \host ->
                    Decode.succeed
                        { username = username
                        , host = host
                        }
