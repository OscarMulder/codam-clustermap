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
    , mapFile : Maybe File
    , hostlistFile : Maybe File
    , uploadFileList : List File
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model (HostGroup Nothing Nothing [] Nothing) HostLoading Draggable.init False Nothing []
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
    = DownloadJson
    | FilePick
    | UploadDragEnter
    | UploadDragLeave
    | GotFiles File (List File)
    | ReadJsonFile String
    | ReadMapFile String


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
            , Select.files [ "application/json", "image/svg+xml" ] GotFiles
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
    Download.string "new-hostlist.json" "application/json" (hostlistToJson hostlist)
