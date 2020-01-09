module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.Popover as Popover
import Browser
import Draggable
import Draggable.Events exposing (onClick, onDragBy, onDragEnd, onDragStart)
import Dragmap as DM
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseUp, preventDefaultOn)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Json.Encode as Encode
import Maybe exposing (withDefault)
import Platform.Cmd
import Task
import Time exposing (Posix, millisToPosix)



-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dragmapModel : DM.Model
    , uploadHover : Bool
    , uploadFile : Maybe File
    , uploadFileList : List File
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model (HostGroup Nothing [] Nothing) HostLoading Draggable.init False Nothing []
    in
    ( model, Platform.Cmd.batch [ getHosts ] )



-- UPDATE


httpErrorString : Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "Bad Url: " ++ text

        Timeout ->
            "Http Timeout"

        NetworkError ->
            "Network Error"

        BadStatus response ->
            "Bad Http Status: " ++ String.fromInt response

        BadBody message ->
            "Bad Http Payload: "
                ++ message


type Msg
    = MorePlease
    | GotHosts (Result Error (List Host))
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | PopoverMsg String Popover.State
    | StartDragging String
    | StopDragging
    | DownloadJson
    | FilePick
    | UploadDragEnter
    | UploadDragLeave
    | GotFiles File (List File)
    | ReadFile String


allHosts : HostGroup -> List Host
allHosts { movingHost, hostList } =
    movingHost
        |> Maybe.map (\a -> a :: hostList)
        |> Maybe.withDefault hostList


startDragging : String -> HostGroup -> HostGroup
startDragging id ({ mapSettings, hostList, movingHost } as group) =
    let
        ( targetAsList, others ) =
            List.partition (.id >> (==) id) hostList
    in
    { group
        | hostList = others
        , movingHost = targetAsList |> List.head
    }


stopDragging : HostGroup -> HostGroup
stopDragging group =
    { group
        | hostList = allHosts group
        , movingHost = Nothing
    }


dragHostBy : ( Float, Float ) -> Host -> Host
dragHostBy ( dx, dy ) host =
    let
        ( x, y ) =
            host.position
    in
    { host | position = ( round (toFloat x + dx), round (toFloat y + dy) ) }


dragActiveBy : ( Float, Float ) -> HostGroup -> HostGroup
dragActiveBy delta group =
    { group | movingHost = group.movingHost |> Maybe.map (dragHostBy delta) }


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.customConfig
        [ onDragBy OnDragBy
        , onDragStart StartDragging
        , onDragEnd StopDragging
        ]


readFile : File -> Cmd Msg
readFile file =
    Task.perform ReadFile (File.toString file)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ hostGroup } as model) =
    case msg of
        MorePlease ->
            ( { model | reqh = HostLoading }
            , getHosts
            )

        GotHosts result ->
            case result of
                Ok hostlist ->
                    let
                        hostgroup =
                            HostGroup Nothing hostlist Nothing
                    in
                    ( { model | reqh = HostSuccess hostlist, hostGroup = hostgroup }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | reqh = HostFailure err }
                    , Cmd.none
                    )

        OnDragBy delta ->
            let
                group =
                    dragActiveBy delta hostGroup
            in
            ( { model | hostGroup = group, reqh = HostSuccess (allHosts group) }, Cmd.none )

        StartDragging id ->
            let
                group =
                    startDragging id hostGroup
            in
            ( { model | hostGroup = group, reqh = HostSuccess (allHosts group) }, Cmd.none )

        StopDragging ->
            let
                group =
                    stopDragging hostGroup
            in
            ( { model | hostGroup = group, reqh = HostSuccess (allHosts group) }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        PopoverMsg host state ->
            let
                updatePopState item =
                    if String.contains host item.id then
                        { item | popState = state }
                    else
                        item
            in
            let
                newmoving =
                    case hostGroup.movingHost of
                        Nothing ->
                            Nothing
                        
                        Just mhost ->
                            Just (updatePopState mhost)
            in
            let
                group =
                    { hostGroup | hostList = List.map updatePopState (allHosts { hostGroup | movingHost = Nothing }), movingHost = newmoving }
            in
                ( { model | hostGroup = group, reqh = HostSuccess (allHosts hostGroup) }, Cmd.none )

        DownloadJson ->
            ( model, downloadJson (allHosts hostGroup) )

        FilePick ->
            ( model
            , Select.files [ "application/json" ] GotFiles
            )

        UploadDragEnter ->
            ( { model | uploadHover = True }
            , Cmd.none
            )

        UploadDragLeave ->
            ( { model | uploadHover = False }
            , Cmd.none
            )

        GotFiles file files ->
            let
                uploadfile =
                    Just file
            in
            ( { model
                | uploadFileList = file :: files
                , uploadFile = uploadfile
                , uploadHover = False
              }
            , readFile file
            )

        ReadFile filecontent ->
            ( { model | hostGroup = getHostList filecontent }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "position" "relative" ]
        [ h2 [] [ text "Dragmap" ]
        , Button.button [ Button.onClick DownloadJson ] [ text "Download Json" ]
        , viewUploader model
        , viewMap model
        ]


viewMap : Model -> Html Msg
viewMap model =
    case model.uploadFile of
        Nothing ->
            case model.reqh of
                HostFailure err ->
                    div []
                        [ text (httpErrorString err)
                        , button [ Html.Events.onClick MorePlease ] [ text "Try Again!" ]
                        ]

                HostLoading ->
                    text "Loading..."

                HostSuccess hostlist ->
                    viewClusterMap model hostlist

        Just hostfile ->
            viewClusterMap model (allHosts model.hostGroup)


viewClusterMap : Model -> List Host -> Html Msg
viewClusterMap model hostlist =
    div [ style "position" "relative" ] (img [ src "https://codamhero.dev/v2/img/f0.svg", style "position" "relative", style "heigth" "1325", style "width" "1026" ] [] :: viewItems model hostlist)


viewItems : Model -> List Host -> List (Html Msg)
viewItems model hostlist =
    List.map (viewItem model) (List.filter isUpstairs hostlist)


viewItem : Model -> Host -> Html Msg
viewItem model host =
    div [ Draggable.mouseTrigger host.id DragMsg, class "imac", style "display" "block", style "position" "absolute", style "background-color" "rgba(0, 0, 0, 0.0)", style "left" ((String.fromInt ((Tuple.first host.position) - 10)) ++ "px") , style "top" ((String.fromInt ((Tuple.second host.position) - 10)) ++ "px") ]
        [ Popover.config
            (Button.button
            [ Button.attrs <| List.append (Popover.onHover host.popState (PopoverMsg host.id)) (id (hostToId host.id) :: [ style "margin" "0", style "padding" "0", style "background" "rgba(0, 0, 0, 0)", style "border" "0px solid black" ]) ]
            [ i [ class "fas fa-circle empty" ][] ]
            )
            |> Popover.top
            |> Popover.content [][ text (hostToId host.id) ]
            |> Popover.view host.popState
        ]


viewUploader : Model -> Html Msg
viewUploader model =
    div
        [ style "border"
            (if model.uploadHover then
                "6px dashed purple"

             else
                "6px dashed #ccc"
            )
        , style "border-radius" "20px"
        , style "width" "250px"
        , style "height" "75px"
        , style "padding" "20px"
        , style "display" "inline-block"
        , style "align-items" "center"
        , hijackOn "dragenter" (Decode.succeed UploadDragEnter)
        , hijackOn "dragover" (Decode.succeed UploadDragEnter)
        , hijackOn "dragleave" (Decode.succeed UploadDragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ button [ Html.Events.onClick FilePick ] [ text "Upload Clustermap" ]
        ]



-- HELPERS

leftDataCorrection : Int -> Int
leftDataCorrection left =
    left - 25

dropDecoder : Decoder Msg
dropDecoder =
    Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore GotFiles File.decoder)


hijackOn : String -> Decode.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


downloadJson : List Host -> Cmd msg
downloadJson hostlist =
    Download.string "clusterf1.json" "application/json" (hostlistToJson hostlist)


hostConverter : Host -> Encode.Value
hostConverter host =
    Encode.object
        [ ( "hostname", Encode.string host.id )
        , ( "left", Encode.int (Tuple.first host.position) )
        , ( "top", Encode.int (Tuple.second host.position) )
        ]


hostlistToJson : List Host -> String
hostlistToJson hostlist =
    Encode.encode 2 (Encode.list hostConverter hostlist)


hostToId : String -> String
hostToId host =
    Maybe.withDefault host (List.head (String.split "." host))


isUpstairs : Host -> Bool
isUpstairs host =
    if String.contains "f0" host.id then
        True

    else
        False



-- JSON FILE DECODER


getHostList : String -> HostGroup
getHostList hostfile =
    let
        result =
            Decode.decodeString hostGroupDecoder hostfile
    in
    case result of
        Ok hostgroup ->
            hostgroup

        Err _ ->
            HostGroup Nothing [] Nothing


hostGroupDecoder : Decoder HostGroup
hostGroupDecoder =
    Field.attempt "mapsettings" mapSettingsDecoder <|
        \mapsettings ->
            Field.require "hosts" hostListFromFileDecoder <|
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
                                , position = ( leftDataCorrection left, top )
                                , popState = Popover.initialState
                                }



-- HTTP


getHosts : Cmd Msg
getHosts =
    Http.get
        { url = "https://codamhero.dev/v2/api/get_hosts.php"
        , expect = Http.expectJson GotHosts hostListDecoder
        }


hostListDecoder : Decoder (List Host)
hostListDecoder =
    Decode.list hostDecoder


hostDecoder : Decoder Host
hostDecoder =
    Field.require "host" Decode.string <|
        \host ->
            Decode.succeed
                { id = host
                , position = ( 0, 0 )
                , popState = Popover.initialState
                }
