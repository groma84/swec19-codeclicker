module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import List.Extra
import Task
import Time



---- MODEL ----


type alias Icons =
    Dict String String


type IconType
    = Moneybag
    | Page
    | Mailbox
    | Blondeface
    | Blackhairedface
    | Redhairedface
    | Keyboard
    | Envelope


type alias Model =
    { lastTick : Int
    , icons : Icons
    }



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time.Posix
    | InitialTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialTime time ->
            ( { model | lastTick = Time.posixToMillis time }, Cmd.none )

        Tick time ->
            let
                millisTime =
                    Time.posixToMillis time

                modelAfterTick =
                    model
            in
            ( { modelAfterTick | lastTick = millisTime }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


header : Html Msg
header =
    section [ class "header" ]
        []


clickingArea : Icons -> Html Msg
clickingArea icons =
    div [ class "clicking-area" ]
        [ text "writeCodeButton"
        , text "makeMoneyButton"
        ]


statsArea : Model -> Html Msg
statsArea model =
    div [ class "stats-area" ]
        [ ol []
            [ li [] [ text "current money and stuff" ] ]
        ]


buyingArea : Model -> Html Msg
buyingArea model =
    div [ class "buying-area" ]
        [ ol []
            [ li
                []
                [ text "buy things area" ]
            ]
        ]


iconImg : Icons -> IconType -> Html Msg
iconImg icons iconType =
    let
        iconUrl =
            getIconUrl icons iconType
    in
    img [ src iconUrl, class "icon-button__icon" ] []


iconButton : Icons -> String -> IconType -> Msg -> Bool -> Html Msg
iconButton icons txt iconType msg isEnabled =
    button [ type_ "button", class "icon-button", onClick msg, disabled (not isEnabled) ]
        [ span []
            [ iconImg icons iconType
            , span [ class "icon-button__text" ] [ text txt ]
            ]
        ]



{-
   buyButton : Icons -> Coder -> Msg -> Bool -> Html Msg
   buyButton icons coder msg isEnabled =
       button [ type_ "button", class "buy-button", onClick msg, disabled (not isEnabled) ]
           [ div [ class "buy-button__info-wrapper" ]
               [ div []
                   [ iconImg icons coder.iconType
                   , span [ class "buy-button__name-text" ] [ text coder.name ]
                   ]
               , div []
                   [ iconImg icons Moneybag
                   , span [ class "buy-button__cost-text" ] [ text <| String.fromInt <| calculatePrice coder.basePrice coder.priceMultiplier coder.count ]
                   ]
               ]
           ]
-}


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ header
        , div [ class "game" ]
            [ clickingArea model.icons
            , statsArea model
            , buyingArea model
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


decodeIconTuple : JD.Decoder ( String, String )
decodeIconTuple =
    JD.map2 Tuple.pair (JD.index 0 JD.string) (JD.index 1 JD.string)


decodeIconList : JD.Decoder (List ( String, String ))
decodeIconList =
    JD.list decodeIconTuple


decodeFlags : JD.Decoder (List ( String, String ))
decodeFlags =
    JD.field "icons" decodeIconList


getIconUrl : Icons -> IconType -> String
getIconUrl icons iconType =
    let
        iconKey =
            case iconType of
                Moneybag ->
                    "moneybag"

                Page ->
                    "page"

                Mailbox ->
                    "mailbox"

                Blondeface ->
                    "blondeface"

                Blackhairedface ->
                    "blackhairedface"

                Redhairedface ->
                    "redhairedface"

                Keyboard ->
                    "keyboard"

                Envelope ->
                    "envelope"
    in
    case Dict.get iconKey icons of
        Just iconUrl ->
            iconUrl

        Nothing ->
            ""



---- PROGRAM ----


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    let
        parsedFlags =
            JD.decodeValue decodeFlags flags

        asDict =
            case parsedFlags of
                Err err ->
                    Debug.log (err |> Debug.toString) Dict.empty

                Ok pf ->
                    Dict.fromList pf
    in
    ( { lastTick = 0
      , icons = asDict
      }
    , Task.perform InitialTime Time.now
    )


main : Program JD.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
