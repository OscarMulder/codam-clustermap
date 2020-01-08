module Clustermap exposing (MapSettings, Model, Msg(..), init, update, view, viewUsedImacs)

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


type SessionRequest
    = Failure Http.Error
    | Loading
    | Success (List Session)


type HostRequest
    = HostFailure Http.Error
    | HostLoading
    | HostSuccess HostModel


type alias MapSettings =
    { height : Int
    , width : Int
    , activeSize : Int
    , emptySize : Int
    }


type alias HostModel =
    { mapSettings : MapSettings
    , hostList : List Host
    }


type alias Host =
    { id : String
    , position : ( Int, Int )
    , popState : Popover.State
    }


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
            :: viewSessions model sessionlist hostmodel
        )


viewSessions : Model -> List Session -> HostModel -> List ( String, Html Msg )
viewSessions model sessionlist hostmodel =
    List.map (viewKeyedSession model sessionlist hostmodel.mapSettings) hostmodel.hostList


viewKeyedSession : Model -> List Session -> MapSettings -> Host -> ( String, Html Msg )
viewKeyedSession model sessionlist hostmapsettings host =
    ( host.id, viewSession model sessionlist hostmapsettings host )


viewSession : Model -> List Session -> MapSettings -> Host -> Html Msg
viewSession model sessionlist hostmapsettings host =
    let
        maybeSession =
            List.head (List.filter (hostFilter host.id) sessionlist)
    in
    let
        offset =
            case maybeSession of
                Nothing ->
                    model.mapSettings.emptySize // 2

                Just _ ->
                    model.mapSettings.activeSize // 2
    in
    div
        [ class "imac-location"
        , style "left" <| String.fromInt ((calculateLeft model hostmapsettings <| Tuple.first host.position) - offset) ++ "px"
        , style "top" <| String.fromInt ((calculateTop model hostmapsettings <| Tuple.second host.position) - offset) ++ "px"
        ]
        (case maybeSession of
            Nothing ->
                [ Popover.config
                    (Button.button
                        [ Button.attrs <| id (hostToId host.id) :: Popover.onHover host.popState (PopoverMsg host.id) ]
                        [ Asset.emptyHost model.mapSettings.emptySize ]
                    )
                    |> Popover.top
                    |> Popover.content [] [ text (hostToId host.id) ]
                    |> Popover.view host.popState
                ]

            Just session ->
                [ Popover.config
                    (Button.button
                        [ Button.attrs <| id (hostToId host.id) :: Popover.onHover host.popState (PopoverMsg host.id) ]
                        [ a [ href ("https://profile.intra.42.fr/users/" ++ session.username), target "_blank" ]
                            [ img
                                [ src ("https://cdn.intra.42.fr/users/small_" ++ session.username ++ ".jpg")
                                , class "round-img"
                                , style "width" <| String.fromInt model.mapSettings.activeSize
                                , style "height" <| String.fromInt model.mapSettings.activeSize
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
            String.fromInt <| List.length hlist

        usedImacs =
            String.fromInt <| List.length <| List.filter (hostListFilter hlist) model.activeList
    in
    text (usedImacs ++ "/" ++ totalImacs)



-- HOST HELPERS


calculateLeft : Model -> MapSettings -> Int -> Int
calculateLeft model hostmapsettings left =
    round <| (toFloat left / toFloat hostmapsettings.width) * toFloat model.mapSettings.width


calculateTop : Model -> MapSettings -> Int -> Int
calculateTop model hostmapsettings top =
    round <| (toFloat top / toFloat hostmapsettings.height) * toFloat model.mapSettings.height


hostListFilter : List Host -> Session -> Bool
hostListFilter hostlist session =
    case List.head <| List.filter (sessionFilter session.host) hostlist of
        Nothing ->
            False

        Just _ ->
            True


sessionFilter : String -> Host -> Bool
sessionFilter sessionhost host =
    if String.contains sessionhost host.id then
        True

    else
        False


hostFilter : String -> Session -> Bool
hostFilter host session =
    if String.contains host session.host then
        True

    else
        False


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
            Field.require "hosts" hostListFromFileDecoder <|
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
                        \activesize ->
                            Field.require "empty-size" Decode.int <|
                                \emptysize ->
                                    Decode.succeed
                                        { height = height
                                        , width = width
                                        , activeSize = activesize
                                        , emptySize = emptysize
                                        }


hostListFromFileDecoder : Decoder (List Host)
hostListFromFileDecoder =
    Decode.list hostFromFileDecoder


hostFromFileDecoder : Decoder Host
hostFromFileDecoder =
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
