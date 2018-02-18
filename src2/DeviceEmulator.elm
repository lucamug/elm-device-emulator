module DeviceEmulator exposing (main)

--import Color exposing (..)
--import Element.Area as Area
--import Element.Background as Background
--import Element.Border as Border
--import Element.Events as Events
--import Element.Font as Font
--import Element.Input as Input
-- import DeviceEmulator
-- import Element.Area as Area
--import Element.Input as Input

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html
import Html.Attributes
import Main
import Task
import Window


--import Html.Attributes
--import Window


type alias Model =
    { modelMain : Main.Model
    , modelDeviceEmulator : ModelDeviceEmulator
    }


type Msg
    = MsgMain Main.Msg
    | ToggleFullscreen
    | ChangeDevice DeviceType
    | WindowSize Window.Size


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


initModel : Model
initModel =
    { modelMain = Main.initModel
    , modelDeviceEmulator =
        { deviceType = IPhone7
        , windowSize =
            { width = 0
            , height = 0
            }
        , fullscreen = False
        }
    }


initCmd : Cmd Msg
initCmd =
    Task.perform WindowSize Window.size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgMain msg ->
            let
                ( newMain, newCmd ) =
                    Main.update msg model.modelMain
            in
            ( { model | modelMain = newMain }, Cmd.map MsgMain newCmd )

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

        WindowSize windowSize ->
            let
                oldModel =
                    model.modelDeviceEmulator

                newModel =
                    { oldModel | windowSize = windowSize }
            in
            ( { model | modelDeviceEmulator = newModel }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, initCmd )
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
                    Element.map MsgMain (Main.viewElement model.modelMain)
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
            Element.map MsgMain (Main.viewElement model.modelMain)

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
