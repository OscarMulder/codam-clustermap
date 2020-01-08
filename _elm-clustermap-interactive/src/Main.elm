module Main exposing (main)

import Asset
import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Clustermap
import Element exposing (Device, DeviceClass(..), Orientation(..))
import Endpoint
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, style)
import Platform.Cmd
import Task
import Time



-- MAIN


main : Program { height : Int, width : Int } Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Window =
    { width : Int
    , height : Int
    }


type alias Model =
    { clusterF0 : Clustermap.Model
    , clusterF1 : Clustermap.Model
    , windowInfo : Window
    , deviceInfo : Maybe Device
    }


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init { width, height } =
    let
        device =
            classifyDevice { height = round (toFloat height * 0.9), width = round (toFloat width * 0.9) }

        mapsettings =
            calcMapSettings { height = round (toFloat height * 0.9), width = round (toFloat width * 0.9) } device

        ( f0model, f0cmd ) =
            Clustermap.init Endpoint.clusterf0 Endpoint.activeSessions Asset.clusterf0 mapsettings

        ( f1model, f1cmd ) =
            Clustermap.init Endpoint.clusterf1 Endpoint.activeSessions Asset.clusterf1 mapsettings
        
        -- Calling BeResponsive before the whole view is loaded
        resizeTask =
            if device.class == Phone then
                Task.perform BeResponsive BD.getViewport
            else
                Cmd.none
    in
    ( Model f0model f1model (Window (round (toFloat width * 0.9)) (round (toFloat height * 0.9))) <| Just device
    , Platform.Cmd.batch [ Platform.Cmd.map Mapf0Msg f0cmd, Platform.Cmd.map Mapf1Msg f1cmd, resizeTask ]
    )



-- UPDATE


type Msg
    = Mapf0Msg Clustermap.Msg
    | Mapf1Msg Clustermap.Msg
    | BeResponsive BD.Viewport
    | OnResize Int Int


calcMapSettings : { window | height : Int, width : Int } -> Device -> Clustermap.MapSettings
calcMapSettings window { class, orientation } =
    let
        preheight =
            round (toFloat window.height * 0.93)

        prewidth =
            round (toFloat preheight * 0.77)

        width =
            if prewidth >= window.width then
                round (toFloat window.width * 0.95)

            else
                prewidth

        height =
            if prewidth >= window.width then
                round (toFloat width * 1.299)

            else
                preheight
    in
    case class of
        Phone ->
            case orientation of
                Portrait ->
                    Clustermap.MapSettings height width 50 16

                Landscape ->
                    Clustermap.MapSettings height width 32 12

        Tablet ->
            case orientation of
                Portrait ->
                    Clustermap.MapSettings height width 50 16

                Landscape ->
                    Clustermap.MapSettings height width 50 16

        Desktop ->
            case orientation of
                Portrait ->
                    Clustermap.MapSettings height width 55 18

                Landscape ->
                    Clustermap.MapSettings height width 55 18

        BigDesktop ->
            case orientation of
                Portrait ->
                    Clustermap.MapSettings height width 70 24

                Landscape ->
                    Clustermap.MapSettings height width 70 24


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ clusterF0, clusterF1 } as model) =
    case msg of
        Mapf0Msg mapmsg ->
            let
                ( newmodel, newcmd ) =
                    Clustermap.update mapmsg model.clusterF0
            in
            ( { model | clusterF0 = newmodel }, Platform.Cmd.map Mapf0Msg newcmd )

        Mapf1Msg mapmsg ->
            let
                ( newmodel, newcmd ) =
                    Clustermap.update mapmsg model.clusterF1
            in
            ( { model | clusterF1 = newmodel }, Platform.Cmd.map Mapf1Msg newcmd )

        BeResponsive viewp ->
            let
                predevice =
                    classifyDevice { width = round viewp.viewport.width, height = round viewp.viewport.height }

                mapsettings =
                    calcMapSettings { height = round viewp.viewport.height, width = round viewp.viewport.width } predevice
            in
            let
                device =
                    case predevice.orientation of
                        Portrait ->
                            predevice
                        Landscape ->
                            if mapsettings.width > round (viewp.viewport.width / 2) then
                                { predevice | orientation = Portrait }
                            else
                                predevice
            in
            ( { model
                | clusterF0 = { clusterF0 | mapSettings = mapsettings }
                , deviceInfo = Just device
                -- I apply a small correction to the F1 map because the maps do not have the same margins, and this helps them allign better
                , clusterF1 = { clusterF1 | mapSettings = { mapsettings | height = mapsettings.height - 30, width = mapsettings.width - round (30.0 * 0.77) } }
              }
            , Cmd.none
            )

        OnResize width height ->
            ( { model
                | deviceInfo = Just <| classifyDevice { width = width, height = height }
              }
            , Task.perform BeResponsive BD.getViewport -- The viewport width and height are more accurate for mobile responsiveness.
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "", style "position" "relative" ]
        [ div [ class <| getMapcontainerClass model.deviceInfo ]
            [ div [ class "mapf0" ]
                [ h1
                    [ style "text-align" "center"
                    , style "font-size" "3rem"
                    , style "width" <| String.fromInt model.clusterF0.mapSettings.width ++ "px"
                    , style "margin-top" "1vh"
                    ]
                    [ text "F0 ->  ", Html.map Mapf0Msg <| Clustermap.viewUsedImacs model.clusterF0 ]
                , Html.map Mapf0Msg (Clustermap.view model.clusterF0)
                ]
            , div [ class "mapf1" ]
                [ h1
                    [ style "text-align" "center"
                    , style "font-size" "3rem"
                    , style "width" <| String.fromInt model.clusterF1.mapSettings.width ++ "px"
                    , style "margin-top" "1vh"
                    ]
                    [ text "F1 ->  ", Html.map Mapf1Msg <| Clustermap.viewUsedImacs model.clusterF1 ]
                , Html.map Mapf1Msg (Clustermap.view model.clusterF1)
                ]
            ]
        ]



-- VIEW HELPERS


getMapcontainerClass : Maybe Device -> String
getMapcontainerClass deviceinfo =
    case deviceinfo of
        Nothing ->
            "mapcontainer-row"

        Just d ->
            case d.orientation of
                Portrait ->
                    "mapcontainer-row"

                Landscape ->
                    "mapcontainer-col"



-- HELPERS


classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice window =
    { class =
        let
            longSide =
                max window.width window.height

            shortSide =
                min window.width window.height
        in
        if shortSide < 600 then
            Phone

        else if longSide <= 1200 then
            Tablet

        else if longSide > 1200 && longSide <= 1800 then
            Desktop

        else
            BigDesktop
    , orientation =
        if window.width < window.height then
            Portrait

        else
            Landscape
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Sub.map Mapf0Msg <| Time.every (30 * 1000) Clustermap.FetchSessions, Sub.map Mapf1Msg <| Time.every (30 * 1000) Clustermap.FetchSessions, BE.onResize OnResize ]
