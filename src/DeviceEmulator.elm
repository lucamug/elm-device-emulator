module DeviceEmulator exposing (main)

-- import Route

import Color exposing (..)
import ConduitSpa as App02
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html
import Html.Attributes
import Json.Decode
import Navigation
import SimpleSpa as App01
import Task
import Window


type AppType
    = Application01
    | Application02


type alias Model =
    { modelApp01 : App01.Model
    , modelApp02 : App02.Model
    , appSelected : AppType
    , modelDeviceEmulator : ModelDeviceEmulator
    }


type Msg
    = MsgApp01 App01.Msg
    | MsgApp02 App02.Msg
    | ToggleFullscreen
    | ChangeDevice DeviceType
    | ChangeApp AppType
    | WindowSize Window.Size
    | UrlChange Navigation.Location


type DeviceType
    = IPhone5
    | IPhone7
    | IPhoneX
    | IPad


type alias ModelDeviceEmulator =
    { deviceType : DeviceType
    , windowSize : Window.Size
    , fullscreen : Bool
    }


init :
    Json.Decode.Value
    -> Navigation.Location
    -> ( Model, Cmd Msg )
init flag location =
    let
        ( initModelApp01, initCmdApp01 ) =
            App01.init flag location

        ( initModelApp02, initCmdApp02 ) =
            App02.init flag location
    in
    ( { modelApp01 = initModelApp01
      , modelApp02 = initModelApp02
      , appSelected = Application01
      , modelDeviceEmulator =
            { deviceType = IPhone7
            , windowSize =
                { width = 0
                , height = 0
                }
            , fullscreen = False
            }
      }
    , Cmd.batch
        [ Cmd.map MsgApp01 initCmdApp01
        , Cmd.map MsgApp02 initCmdApp02
        , Task.perform WindowSize Window.size
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        UrlChange url ->
            ( model, Cmd.none )

        MsgApp01 msg ->
            let
                ( newApp01, newCmd ) =
                    App01.update msg model.modelApp01
            in
            ( { model | modelApp01 = newApp01 }, Cmd.map MsgApp01 newCmd )

        MsgApp02 msg ->
            let
                ( newApp02, newCmd ) =
                    App02.update msg model.modelApp02
            in
            ( { model | modelApp02 = newApp02 }, Cmd.map MsgApp02 newCmd )

        ToggleFullscreen ->
            let
                oldModel =
                    model.modelDeviceEmulator

                modelDeviceEmulator =
                    { oldModel | fullscreen = not oldModel.fullscreen }
            in
            ( { model | modelDeviceEmulator = modelDeviceEmulator }, Cmd.none )

        ChangeDevice deviceType ->
            let
                oldModel =
                    model.modelDeviceEmulator

                modelDeviceEmulator =
                    { oldModel | deviceType = deviceType }
            in
            ( { model | modelDeviceEmulator = modelDeviceEmulator }, Cmd.none )

        ChangeApp appType ->
            ( { model | appSelected = appType }, Cmd.none )

        WindowSize windowSize ->
            let
                oldModel =
                    model.modelDeviceEmulator

                newModel =
                    { oldModel | windowSize = windowSize }
            in
            ( { model | modelDeviceEmulator = newModel }, Cmd.none )


type alias Flag =
    String


fromLocationToMsgApp01 : Navigation.Location -> Msg
fromLocationToMsgApp01 location =
    MsgApp01 (App01.fromLocationToMsg location)


fromLocationToMsgApp02 : Navigation.Location -> Msg
fromLocationToMsgApp02 location =
    MsgApp02 (App02.fromLocationToMsg location)


main : Program Json.Decode.Value Model Msg
main =
    Navigation.programWithFlags fromLocationToMsgApp01
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


windowSize : Model -> ( Int, Int )
windowSize model =
    if model.modelDeviceEmulator.fullscreen then
        ( model.modelDeviceEmulator.windowSize.width
        , model.modelDeviceEmulator.windowSize.height
        )
    else
        deviceSize model


view : Model -> Html.Html Msg
view model =
    let
        ( windowWidth, windowHeight ) =
            windowSize model
    in
    layout
        [ if model.modelDeviceEmulator.fullscreen then
            Background.color white
          else
            Background.color grey9
        , viewMenuStickyRight model
        ]
    <|
        row
            [ alignTop
            , height fill
            ]
            [ if model.modelDeviceEmulator.windowSize.width > 0 then
                if model.modelDeviceEmulator.fullscreen then
                    html (Html.map MsgApp01 (App01.view model.modelApp01))
                else
                    viewDevice model
              else
                el [ centerY ] <| text "windowSize is not set yet"
            ]


deviceSize : Model -> ( number, number1 )
deviceSize model =
    case model.modelDeviceEmulator.deviceType of
        IPhone5 ->
            ( 320, 568 )

        IPhone7 ->
            ( 375, 667 )

        IPhoneX ->
            ( 375, 812 )

        IPad ->
            ( 768, 1024 )


viewDevice : Model -> Element Msg
viewDevice model =
    let
        content =
            case model.appSelected of
                Application01 ->
                    html
                        (Html.map MsgApp01
                            (Html.div
                                [ Html.Attributes.style
                                    [ ( "white-space", "normal" )
                                    , ( "whith", "100%" )
                                    ]
                                ]
                                [ App01.view model.modelApp01 ]
                            )
                        )

                Application02 ->
                    html
                        (Html.map MsgApp02
                            (Html.div
                                [ Html.Attributes.style
                                    [ ( "white-space", "normal" )
                                    , ( "whith", "100%" )
                                    ]
                                ]
                                [ App02.view model.modelApp02 ]
                            )
                        )

        -- text "ciao"
        deviceBorderTop =
            80

        deviceBorderBottom =
            110

        deviceBorderSide =
            10

        ( deviceWidth, deviceHeight ) =
            deviceSize model
    in
    column
        [ height shrink
        , alignTop
        ]
        [ column
            [ above True <|
                el
                    [ Font.shadow { offset = ( -2, -2 ), blur = 0, color = black }
                    , Font.shadow { offset = ( 2, 2 ), blur = 0, color = grey3 }
                    , Font.color <| gray2
                    , Font.size 48
                    , moveDown 80
                    ]
                    (text "● ▬▬▬")
            , below True <|
                el
                    [ Font.shadow { offset = ( -2, -2 ), blur = 0, color = black }
                    , Font.shadow { offset = ( 2, 2 ), blur = 0, color = grey3 }
                    , Font.color <| gray2
                    , Font.size 150
                    , moveUp 180
                    ]
                    (text "●")
            ]
            [ column
                [ padding 16
                ]
                [ el
                    [ height <| px (2 + deviceHeight + deviceBorderTop + deviceBorderBottom)
                    , width <| px (2 + deviceWidth + deviceBorderSide * 2)
                    , Border.widthEach
                        { top = deviceBorderTop
                        , right = deviceBorderSide
                        , bottom = deviceBorderBottom
                        , left = deviceBorderSide
                        }
                    , Border.rounded 50
                    , Border.color <| grey1
                    , style ( "box-shadow", "rgba(10, 10, 10, 0.19) 5px 5px 5px 5px" )
                    ]
                  <|
                    el
                        [ width fill
                        , height fill
                        , scrollbars
                        , Border.width 1
                        , Border.color black
                        , Background.color white
                        , style ( "justify-content", "normal" )
                        , style ( "max-height", toString deviceHeight ++ "px" )
                        ]
                    <|
                        content

                -- (Element.map MsgForm (Pages.Form.viewElement deviceWidth model.modelForm))
                ]
            ]
        ]


viewMenuStickyRight : Model -> Attribute Msg
viewMenuStickyRight model =
    let
        ( windowWidth, windowHeight ) =
            windowSize model

        menuItems =
            viewDeviceMenu
                ++ [ text <|
                        toString windowWidth
                            ++ "px ✕ "
                            ++ toString windowHeight
                            ++ "px"
                   ]
                ++ [ if model.appSelected == Application01 then
                        el [ Events.onClick <| ChangeApp Application01 ] <| text "App01"
                     else
                        el [ Events.onClick <| ChangeApp Application02 ] <| text "App02"
                   ]
                ++ [ if model.modelDeviceEmulator.fullscreen then
                        el [ Events.onClick ToggleFullscreen ] <|
                            -- Logo.logo Logo.ExitFullscreen 18
                            text "not full screen"
                     else
                        el [ Events.onClick ToggleFullscreen ] <|
                            -- Logo.logo Logo.Fullscreen 18
                            text "fullscreen"
                   ]
    in
    above True <|
        row
            [ style ( "opacity", "0.7" )
            , style ( "position", "fixed" )
            , style ( "left", "0" )
            , style ( "bottom", "auto" )
            , Background.color <| white
            , Font.size 14
            , Font.family
                [ Font.sansSerif
                ]
            , pointer
            , padding 6
            , spacing 12
            ]
        <|
            menuItems



{-
   viewSubMenuRight : Model -> List (Element Msg)
   viewSubMenuRight model =
       [ el []
           (if model.route == Form then
               myLink []
                   { label = Logo.logo Logo.ExitFullscreen 18
                   , url = pathStarting
                   }
            else
               myLink []
                   { label = Logo.logo Logo.Fullscreen 18
                   , url = pathStarting ++ "form"
                   }
           )
       ]
-}


viewDeviceMenu : List (Element Msg)
viewDeviceMenu =
    [ el [ pointer, Events.onClick <| ChangeDevice IPhone5 ] <| text "iPhone 5"
    , el [ pointer, Events.onClick <| ChangeDevice IPhone7 ] <| text "iPhone 7"
    , el [ pointer, Events.onClick <| ChangeDevice IPhoneX ] <| text "iPhone X"
    , el [ pointer, Events.onClick <| ChangeDevice IPad ] <| text "iPad"
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowSize


grey9 : Color
grey9 =
    Color.rgb 0x99 0x99 0x99


grey5 : Color
grey5 =
    Color.rgb 0x55 0x55 0x55


grey3 : Color
grey3 =
    Color.rgb 0x33 0x33 0x33


gray2 : Color
gray2 =
    Color.rgb 0x22 0x22 0x22


grey1 : Color
grey1 =
    Color.rgb 0x11 0x11 0x11


style : ( String, String ) -> Element.Attribute Msg
style style =
    Element.attribute (Html.Attributes.style [ style ])
