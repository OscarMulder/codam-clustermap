module Main exposing (main)

import Asset exposing (Image, image)
import Bootstrap.Button as Button
import Bootstrap.Popover as Popover
import Browser
import Dragmap as DM exposing (Msg(..))
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseUp, preventDefaultOn)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Platform.Cmd
import Task




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
    { dragmapModel : Maybe DM.Model
    , uploadHover : Bool
    , mapFile : Maybe Image
    , hostlistFile : Maybe String
    , uploadFileList : List File
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model Nothing False Nothing Nothing []
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = FilePick
    | UploadDragEnter
    | UploadDragLeave
    | GotFiles File (List File)
    | ReadJsonFile String
    | ReadMapFile String
    | MapMsg DM.Msg



readFile : File -> Cmd Msg
readFile file =
    if File.mime file == "application/json" then
        Task.perform ReadJsonFile (File.toString file)
    else if File.mime file == "image/svg+xml" then
        Task.perform ReadMapFile (File.toUrl file)
    else
        Cmd.none

setDragModel : Maybe String -> Maybe Image -> Maybe DM.Model
setDragModel hostlist mapimage =
    case mapimage of
        Just mapfile ->
            case hostlist of
                Just hostlistfile ->
                    let
                        ( mapmodel, mapmsg ) =
                            DM.init hostlistfile mapfile    
                    in
                        Just mapmodel
                Nothing ->
                    Nothing
        Nothing ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ( { model
                | uploadFileList = file :: files
                , uploadHover = False
              }
            , readFile file
            )

        ReadJsonFile hoststring ->
            ( { model
                | hostlistFile = Just hoststring
                , dragmapModel = setDragModel (Just hoststring) model.mapFile
              }
            , Cmd.none
            )

        ReadMapFile mapstring ->
            ( { model
                | mapFile = Just (Asset.image mapstring)
                , dragmapModel = setDragModel model.hostlistFile (Just (Asset.image mapstring))
              }
            , Cmd.none
            )

        MapMsg mapmsg ->
            case model.dragmapModel of
                Just dragmodel ->
                    let
                        ( newmodel, newcmd ) =
                            DM.update mapmsg dragmodel
                    in
                    ( { model | dragmapModel = Just newmodel }, Platform.Cmd.map MapMsg newcmd )
                Nothing ->
                    ( model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { dragmapModel } =
    case dragmapModel of
        Nothing ->
            Sub.none
        Just dmodel ->
            Sub.map MapMsg <|
                DM.subscriptions dmodel



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "position" "relative" ]
    [ div [ style "position" "relative" ]
        [ h2 [] [ text "Dragmap" ]
        , viewUploader model
        , Button.button [ Button.attrs [ class "download-button" ], Button.onClick (MapMsg DownloadJson) ] [ text "Download Json" ]
        ]
    , div [ style "position" "relative" ]
        [        
        case model.dragmapModel of
            Just dmModel ->
                Html.map MapMsg <| DM.view dmModel
            Nothing ->
                p [][ text "Please upload a host list in json format and a cluster map image in svg format." ]
        ]
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
        , style "width" "400px"
        , style "height" "100px"
        , style "padding" "20px"
        , style "display" "inline-block"
        , style "align-items" "center"
        , hijackOn "dragenter" (Decode.succeed UploadDragEnter)
        , hijackOn "dragover" (Decode.succeed UploadDragEnter)
        , hijackOn "dragleave" (Decode.succeed UploadDragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ button [ Html.Events.onClick FilePick ] [ text "Upload Hostlist Json and Map svg" ]
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
