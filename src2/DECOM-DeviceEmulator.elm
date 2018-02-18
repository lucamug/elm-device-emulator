module DeviceEmulator exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Area as Area
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Window


type DeviceType
    = IPhone5
    | IPhone7
    | IPhoneX
    | IPad


type alias Model =
    { deviceType : DeviceType
    }


initModel : Model
initModel =
    { deviceType = IPhone7 }


type Msg
    = ToggleFullscreen
    | ChangeDevice DeviceType
    | WindowSize Window.Size


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowSize


greyDark : Color
greyDark =
    black


dark : Color
dark =
    black


blackTer : Color
blackTer =
    black


style : ( String, String ) -> Element.Attribute Msg
style style =
    Element.attribute (Html.Attributes.style [ style ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Element Msg -> Element Msg
view model content =
    let
        deviceBorderTop =
            80

        deviceBorderBottom =
            110

        deviceBorderSide =
            10

        ( deviceWidth, deviceHeight ) =
            case model.deviceType of
                IPhone5 ->
                    ( 320, 568 )

                IPhone7 ->
                    ( 375, 667 )

                IPhoneX ->
                    ( 375, 812 )

                IPad ->
                    ( 768, 1024 )
    in
    column
        [ height shrink
        , alignTop
        , viewMenuStickyRight viewDeviceMenu model
        ]
        [ column
            [ above True <|
                el
                    [ Font.shadow { offset = ( -2, -2 ), blur = 0, color = black }
                    , Font.shadow { offset = ( 2, 2 ), blur = 0, color = greyDark }
                    , Font.color <| dark
                    , moveDown 80
                    , Font.size 48
                    ]
                    (text "● ▬▬▬")
            , below True <|
                el
                    [ Font.shadow { offset = ( -2, -2 ), blur = 0, color = black }
                    , Font.shadow { offset = ( 2, 2 ), blur = 0, color = greyDark }
                    , Font.color <| dark
                    , moveUp 180
                    , Font.size 150
                    ]
                    (text "●")
            ]
            [ row [] viewDeviceMenu
            , text "ciao"
            , column
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
                    , Border.color <| blackTer

                    -- , Border.shadow { offset = ( 5, 5 ), blur = 5, size = 5, color = black }
                    , style ( "box-shadow", "rgba(10, 10, 10, 0.19) 5px 5px 5px 5px" )
                    ]
                  <|
                    el
                        [ width fill
                        , scrollbars
                        , Border.width 1
                        , Border.color <| black
                        , style ( "justify-content", "normal" )
                        , style ( "max-height", toString deviceHeight ++ "px" )
                        ]
                    <|
                        content

                -- (Element.map MsgForm (Pages.Form.viewElement deviceWidth model.modelForm))
                ]
            ]
        ]


viewMenuStickyRight : List (Element Msg) -> Model -> Attribute Msg
viewMenuStickyRight menuItems model =
    above True <|
        row
            [ style ( "opacity", "0.7" )
            , style ( "position", "fixed" )
            , style ( "right", "0" )
            , style ( "bottom", "auto" )
            , Background.color <| white
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

    --, el [ pointer, Events.onClick <| ChangeDevice IPad ] <| text "iPad"
    ]
