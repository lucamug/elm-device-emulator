port module Main exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Svg
import Svg.Attributes as SA


-- TYPES


type Msg
    = EditorChangeToggle Bool
    | EditorChangeText String
    | EditorChangeLogo Logo
    | EditorChangeJson String


type Logo
    = Elm
    | Strawberry
    | Watermelon


type alias Data =
    { text : String
    , logo : Logo
    , toggle : Bool
    }


type alias Model =
    { data : Data
    , err : Maybe String
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            model.data
    in
    case msg of
        EditorChangeToggle value ->
            let
                newData =
                    { data | toggle = value }
            in
            handleNewJson model newData

        EditorChangeLogo value ->
            let
                newData =
                    { data | logo = value }
            in
            handleNewJson model newData

        EditorChangeText value ->
            let
                newData =
                    { data | text = value }
            in
            handleNewJson model newData

        EditorChangeJson newDataString ->
            let
                ( err, newData ) =
                    stringToData newDataString model.data
            in
            ( { model | data = newData, err = err }, Cmd.none )


handleNewJson : Model -> Data -> ( Model, Cmd msg )
handleNewJson model newData =
    ( { model | data = newData, err = Nothing }, Cmd.none )



-- INIT


initModel : Model
initModel =
    { data =
        { text = "Hello!"
        , logo = Elm
        , toggle = False
        }
    , err = Nothing
    }


initCmd : Cmd Msg
initCmd =
    Cmd.none


init : ( Model, Cmd Msg )
init =
    ( initModel
    , initCmd
    )



-- DECODERS/ENCODERS


dataToString : Data -> String
dataToString data =
    Json.Encode.encode 4 <| jsonEncoder data


stringToResult : String -> Result String Data
stringToResult string =
    Json.Decode.decodeString jsonDecoder string


stringToData : String -> Data -> ( Maybe String, Data )
stringToData string oldData =
    case stringToResult string of
        Ok newData ->
            ( Nothing, newData )

        Err err ->
            ( Just err, oldData )


jsonEncoder : Data -> Json.Encode.Value
jsonEncoder data =
    Json.Encode.object
        [ ( "text", Json.Encode.string <| data.text )
        , ( "logo", Json.Encode.string <| toString data.logo )
        , ( "toggle", Json.Encode.bool <| data.toggle )
        ]


jsonDecoder : Json.Decode.Decoder Data
jsonDecoder =
    Json.Decode.Pipeline.decode Data
        |> Json.Decode.Pipeline.required "text" Json.Decode.string
        |> Json.Decode.Pipeline.required "logo" (Json.Decode.string |> Json.Decode.andThen logoDecoder)
        |> Json.Decode.Pipeline.required "toggle" Json.Decode.bool


logoDecoder : String -> Json.Decode.Decoder Logo
logoDecoder logoString =
    case logoString of
        "Elm" ->
            Json.Decode.succeed Elm

        "Strawberry" ->
            Json.Decode.succeed Strawberry

        "Watermelon" ->
            Json.Decode.succeed Watermelon

        _ ->
            Json.Decode.fail <| "I don't know a logo named \"" ++ logoString ++ "\""



-- VIEWS


view : Model -> Html.Html Msg
view model =
    layout
        [ Font.family
            [ Font.typeface "Source Sans Pro"
            , Font.sansSerif
            ]
        , Font.size 16
        , Font.color <| Color.rgb 0x33 0x33 0x33
        , Background.color <| Color.rgb 0xFF 0xFF 0xFF
        ]
    <|
        viewElement model


viewElement : Model -> Element Msg
viewElement model =
    row
        [ width <| px 320
        , alignTop
        , paddingXY 0 20
        ]
        [ viewEditor model ]


viewHeading : String -> Element msg
viewHeading t =
    el
        [ Font.size 20
        , alignLeft
        , paddingXY 0 10
        ]
    <|
        text t


viewEditor : Model -> Element Msg
viewEditor model =
    column [ spacing 10 ]
        [ el [ Font.size 28 ] <| text "Elm - Unbreakable Json"
        , column []
            [ viewHeading "Json"
            , Input.spellcheckedMultiline
                [ height <| px 130 ]
                { onChange = Just EditorChangeJson
                , text = dataToString model.data
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text ""
                , notice =
                    case model.err of
                        Nothing ->
                            Nothing

                        Just err ->
                            Just <| Input.errorBelow [ Font.color Color.red ] <| paragraph [] [ text err ]
                }
            ]
        , Input.text []
            { label = Input.labelAbove [] <| viewHeading "Text"
            , onChange = Just EditorChangeText
            , notice = Nothing
            , placeholder = Nothing
            , text = model.data.text
            }
        , Input.radio []
            { label = Input.labelAbove [] <| viewHeading "Logo"
            , onChange = Just EditorChangeLogo
            , notice = Nothing
            , selected = Just model.data.logo
            , options =
                [ Input.option Elm
                    (row [ padding 10, spacing 10 ]
                        [ el [ alignLeft ] (html <| logoElm 22)
                        , el [ alignLeft ] (text "Elm")
                        ]
                    )
                , Input.option Watermelon
                    (row [ padding 10, spacing 10 ]
                        [ el [ alignLeft ] (html <| logoWatermelon 22)
                        , el [ alignLeft ] (text "Watermelon")
                        ]
                    )
                , Input.option Strawberry
                    (row [ padding 10, spacing 10 ]
                        [ el [ alignLeft ] (html <| logoStrawberry 22)
                        , el [ alignLeft ] (text "Strawberry")
                        ]
                    )
                ]
            }
        , paragraph []
            [ Input.checkbox []
                { label = Input.labelAbove [] <| viewHeading "Toggle"
                , onChange = Just EditorChangeToggle
                , notice = Nothing
                , checked = model.data.toggle
                , icon = Nothing
                }
            ]
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- LOGOS


logoWatermelon : Int -> Html.Html msg
logoWatermelon height =
    Svg.svg
        [ SA.viewBox "0 0 50.5 50.5"
        , SA.height <| toString height
        , SA.width <| toString <| floor <| toFloat height * 1
        ]
        [ Svg.path [ SA.fill "#88c057", SA.d "M18.4 24l1.4 4.5c.2 1.1-.6 2.2-1.7 2.6l-1.1.4-.3 1a4 4 0 0 1-2.7 2.7c-1.2.3-3.7-2.3-4.7-2.1L0 42.4A30 30 0 0 0 42.4 0l-24 24z" ] []
        , Svg.path [ SA.fill "#e22f37", SA.d "M37 5.3L18.5 24l1.4 4.5c.2 1.1-.6 2.2-1.7 2.6l-1.1.4-.3 1a4 4 0 0 1-2.7 2.7c-1.2.3-3.7-2.3-4.7-2.1l-4 4c8.8 9.5 22.4 8.7 31.5-.3S46.6 14 37 5.3z" ] []
        , Svg.circle [ SA.cx "4.5", SA.cy "17", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "16.5", SA.cy "39", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "26", SA.cy "25.6", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "30.9", SA.cy "20.7", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "28.1", SA.cy "37.6", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "33", SA.cy "32.7", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "38", SA.cy "27.7", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "35.9", SA.cy "15.7", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "22.4", SA.cy "36.2", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "27.5", SA.cy "31", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "32.5", SA.cy "26", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "7.5", SA.cy "27", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "13.5", SA.cy "19", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "37.3", SA.cy "21.4", SA.r "1.5", SA.fill "#231f20" ] []
        ]


logoStrawberry : Int -> Html.Html msg
logoStrawberry height =
    Svg.svg
        [ SA.viewBox "0 0 57 57"
        , SA.height <| toString height
        , SA.width <| toString <| floor <| toFloat height * 1
        ]
        [ Svg.path [ SA.fill "#659c35", SA.d "M29.8 9.4l-2.9.6L24 1.5 29 0z" ] []
        , Svg.path [ SA.fill "#88c057", SA.d "M36.1 8.5a8 8 0 0 0 2.4-3.6c-5.5-2-7.2.6-7.2.6 0-1-.9-1.5-2-1.6l.5 5.5-2.9.6-1.8-5.2c-.5.2-.8.5-.8.7 0 0-1.7-2.6-7.2-.6a7.9 7.9 0 0 0 2.4 3.7c-4.4.6-8.4 2-10.5 5.8 10.3 3 13.4-1.9 13.4-1.9.6 6.8 5.8 8.7 6.6 9 .8-.3 6-2.2 6.6-9 0 0 1.2 5 11.4 1.9-2.2-3.7-6.5-5.3-10.9-6z" ] []
        , Svg.path [ SA.fill "#e22f37", SA.d "M45.3 15v-.1c-8.7 2-9.7-2.4-9.7-2.4-.6 6.8-5.8 8.7-6.6 9-.8-.3-6-2.2-6.6-9 0 0-2.6 4-10.8 2.5C9.3 17.6 8 21.4 8 27c0 13 12.8 30 20.5 30C36.2 57 49 39.9 49 27c0-5.8-1.3-9.6-3.8-12h.1z" ] []
        , Svg.path [ SA.fill "#994530", SA.d "M17.3 20.7c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 1.9 1.9c.5 0 1-.2 1.4-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm11.8 4c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 2 1.9c.4 0 1-.2 1.3-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm11.2-4c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 1.9 1.9c.5 0 1-.2 1.4-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm-18.2 13c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 2 2c.4 0 1-.3 1.3-.7.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm7 9c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 2 1.9c.4 0 1-.2 1.3-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm7-9c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 2 1.9c.4 0 1-.2 1.3-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8z" ] []
        ]


logoElm : Int -> Html.Html msg
logoElm height =
    let
        f =
            SA.fill

        d =
            SA.d

        p =
            Svg.path

        c =
            { c1 = "#F0AD00"
            , c2 = "#7FD13B"
            , c3 = "#60B5CC"
            , c4 = "#5A6378"
            }
    in
    Svg.svg
        [ SA.version "1"
        , SA.viewBox "0 0 323 323"
        , SA.height <| toString height
        , SA.width <| toString <| floor <| toFloat height * 1
        ]
        [ p [ f c.c1, d "M162 153l70-70H92zm94 94l67 67V179z" ] []
        , p [ f c.c2, d "M9 0l70 70h153L162 0zm238 85l77 76-77 77-76-77z" ] []
        , p [ f c.c3, d "M323 144V0H180zm-161 27L9 323h305z" ] []
        , p [ f c.c4, d "M153 162L0 9v305z" ] []
        ]
