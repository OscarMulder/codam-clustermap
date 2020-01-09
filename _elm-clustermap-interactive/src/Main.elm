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

{-| The record { height : Int, width : Int, isFirefox : Bool } contains values
passed in by javascript (flags). This is done to get an accurate initial
viewport height and width as a base for the map sizes. They will be used in the
init function. isFirefox is needed for responsiveness (see init function).
-}
main : Program { height : Int, width : Int, isFirefox : Bool }  Model Msg
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
    , deviceInfo : Device
    }


{-| 
On initial loading we use the values passed from the flags to determine a map
size. I found that taking 90% of the width and hight values gives better
results. 

Firefox loads nicely with the width and hight from the flags, but chrome
doesn't. Also, performing getViewport in Firefox before the map is fully loaded
gives strange results. It only seems to work fine when triggered from the
onResize event. So the getViewport task isn't performed when the browser is
Firefox, but is for all other browsers (I only tested Chrome and Firefox).
-}
init : { width : Int, height : Int, isFirefox : Bool } -> ( Model, Cmd Msg )
init { width, height, isFirefox } =
    let
        width90 =
            round (toFloat width * 0.9)
        
        height90 =
            round (toFloat height * 0.9)

        device =
            classifyDevice { height = height90, width = width90}

        mapsettings =
            calcMapSettings { height = height90, width = width90 } device

        ( f0model, f0cmd ) =
            Clustermap.init Endpoint.clusterf0 Endpoint.activeSessions Asset.clusterf0 mapsettings

        ( f1model, f1cmd ) =
            Clustermap.init Endpoint.clusterf1 Endpoint.activeSessions Asset.clusterf1 mapsettings
        
        resizeTask =
            if isFirefox then
                Cmd.none
            else
                Task.perform BeResponsive BD.getViewport
    in
    ( Model f0model f1model (Window width90 height90) device
    , Platform.Cmd.batch 
        [ Platform.Cmd.map Mapf0Msg f0cmd
        , Platform.Cmd.map Mapf1Msg f1cmd
        , resizeTask
        ]
    )



-- UPDATE


type Msg
    = Mapf0Msg Clustermap.Msg
    | Mapf1Msg Clustermap.Msg
    | BeResponsive BD.Viewport
    | OnResize Int Int


{-| Calculates the optimal map size based on the size of the window (viewport)
and the orientation of the device. The multiply values (0.93, 0.77, etc.) are
based on the dimensions of the svgs that are used.

By default the height of the screen (times 0.93 to leave margin) is taken as
base value to calculate the map width. If this results in a map width larger
than screen width, the width is taken as base.

After this the icon sizes are determined based on device type and orientation.
-}
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
                            -- If 2 maps don't fit side by side on the screen, display in portrait mode.
                            if mapsettings.width > round (viewp.viewport.width / 2) then
                                { predevice | orientation = Portrait }
                            else
                                predevice
            in
            -- I apply a small correction to the F1 map because the maps do not have the
            -- same margins, and this helps them allign better.
            ( { model
                | clusterF0 = { clusterF0 | mapSettings = mapsettings }
                , deviceInfo = device
                , clusterF1 = { clusterF1 | mapSettings = { mapsettings | height = mapsettings.height - 30, width = mapsettings.width - round (30.0 * 0.77) } }
              }
            , Cmd.none
            )

        -- The onResize event gives a width and height, but on mobile these values are
        -- inconsistent. I use them to set the deviceInfo mostly for show, because these
        -- values are immediately replaced in the BeResponsive update call which is done to
        -- get more accurate width and height on mobile.
        OnResize width height ->
            ( { model
                | deviceInfo = classifyDevice { width = width, height = height }
              }
            , Task.perform BeResponsive BD.getViewport
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


{-| Give the class corresponding to the correct grid layout based on device
orientation.
-}
getMapcontainerClass : Device -> String
getMapcontainerClass deviceinfo =
    case deviceinfo.orientation of
        Portrait ->
            "mapcontainer-row"

        Landscape ->
            "mapcontainer-col"



-- HELPERS


{-| Set the device type and orientation based on a width and height.
-}
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

{-| Every 30 seconds both clusters are updated, also there is a listener for
onResize events. 
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch 
        [ Sub.map Mapf0Msg 
            <| Time.every (30 * 1000) Clustermap.FetchSessions
        , Sub.map Mapf1Msg 
            <| Time.every (30 * 1000) Clustermap.FetchSessions
        , BE.onResize OnResize
        ]
 