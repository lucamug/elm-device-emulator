module SimpleSpa
    exposing
        ( Model
        , Msg(SetRoute)
        , fromLocationToMsg
        , init
        , main
        , update
        , view
        )

import Html
import Json.Encode
import Navigation exposing (Location)


fromLocationToMsg : Location -> Msg
fromLocationToMsg =
    (\_ -> Just Home) >> SetRoute


type alias Model =
    {}


type Msg
    = SetRoute (Maybe Route)


type Route
    = Home


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute a ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.text "Hello World"


type alias Value =
    Json.Encode.Value


init : Value -> Navigation.Location -> ( Model, Cmd Msg )
init val location =
    ( {}, Cmd.none )


main : Program Value Model Msg
main =
    Navigation.programWithFlags fromLocationToMsg
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
