module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Task
import Time



---- MODEL ----


type alias Model =
    { lastTick : Int
    , currentLoc : Int
    , currentMoney : Int
    , manuallyGeneratedLoc : Int
    , manuallyGeneratedMoney : Int
    , icons : Icons
    }



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time.Posix
    | InitialTime Time.Posix
    | WriteSomeCode
    | ConvertCodeToMoney


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialTime time ->
            ( { model | lastTick = Time.posixToMillis time }, Cmd.none )

        Tick time ->
            ( { model | lastTick = Time.posixToMillis time }, Cmd.none )

        WriteSomeCode ->
            ( { model | manuallyGeneratedLoc = model.manuallyGeneratedLoc + 1, currentLoc = model.currentLoc + 1 }, Cmd.none )

        ConvertCodeToMoney ->
            let
                ( newLoc, newMoney, newManuallyGeneratedMoney ) =
                    if model.currentLoc >= 10 then
                        ( model.currentLoc - 10, model.currentMoney + 1, model.manuallyGeneratedMoney + 1 )

                    else
                        ( model.currentLoc, model.currentMoney, model.manuallyGeneratedMoney )
            in
            ( { model | currentLoc = newLoc, currentMoney = newMoney, manuallyGeneratedMoney = newManuallyGeneratedMoney }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


header : Html Msg
header =
    section [ class "header" ]
        [ h1 [] [ text "Codeclicker" ]
        , h2 [] [ text "#swec19" ]
        ]


clickingArea : Icons -> Int -> Int -> Html Msg
clickingArea icons manuallyGeneratedLoc currentLoc =
    let
        makeMoneyButton =
            if manuallyGeneratedLoc >= 10 then
                iconButton icons "Send a newsletter to make money!" Mailbox ConvertCodeToMoney (currentLoc >= 10)

            else
                text ""
    in
    div [ class "clicking-area" ]
        [ iconButton icons "Write some code!" Keyboard WriteSomeCode True
        , makeMoneyButton
        ]


statsArea : Model -> Html Msg
statsArea model =
    div [ class "stats-area" ]
        [ ol []
            [ li [] [ text (String.fromInt model.currentLoc ++ " LoC") ]
            , li [] [ text (String.fromInt model.currentMoney ++ " $$") ]
            ]
        ]


buyingArea : Model -> Html Msg
buyingArea model =
    div [ class "buying-area" ]
        [ ol []
            [ li [] [ text (String.fromInt model.currentLoc ++ " LoC") ]
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


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ header
        , div [ class "game" ]
            [ clickingArea model.icons model.manuallyGeneratedLoc model.currentLoc
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
      , currentLoc = 0
      , currentMoney = 0
      , manuallyGeneratedLoc = 0
      , manuallyGeneratedMoney = 0
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
