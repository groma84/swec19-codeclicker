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


type CoderType
    = Praktikant


type alias Coder =
    { name : String
    , coderType : CoderType
    , locPerTickGain : Int
    , alreadyUnlocked : Bool
    , basePrice : Int
    , priceMultiplier : Float
    , iconType : IconType
    , count : Int
    }


praktikant : Coder
praktikant =
    { name = "Praktikant"
    , coderType = Praktikant
    , locPerTickGain = 1
    , alreadyUnlocked = False
    , basePrice = 3
    , priceMultiplier = 1.03
    , iconType = Blondeface
    , count = 0
    }


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
    , currentLoc : Int
    , currentMoney : Int
    , manuallyGeneratedLoc : Int
    , manuallyGeneratedMoney : Int
    , icons : Icons
    , coders : List Coder
    }


isUnlocked : CoderType -> Model -> Bool
isUnlocked coderType model =
    case coderType of
        Praktikant ->
            model.manuallyGeneratedMoney >= 2


calculatePrice : Int -> Float -> Int -> Int
calculatePrice basePrice multiplier count =
    toFloat basePrice
        * (multiplier ^ toFloat count)
        |> round


isUsable : Coder -> Model -> Bool
isUsable coder model =
    let
        currentPrice =
            calculatePrice coder.basePrice coder.priceMultiplier coder.count
    in
    case coder.coderType of
        Praktikant ->
            model.currentMoney >= currentPrice



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time.Posix
    | InitialTime Time.Posix
    | WriteSomeCode
    | ConvertCodeToMoney
    | BuyCoder Coder


tick : Model -> Int -> Model
tick model ticks =
    if ticks == 0 then
        model

    else
        let
            new
        in
        
        tick model (ticks - 1)


gameloop : Model -> Int -> Model
gameloop model timediff =
    let
        ticks =
            round (toFloat timediff / 1000)
    in
    tick model ticks


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
                    gameloop model (millisTime - model.lastTick)
            in
            ( { modelAfterTick | lastTick = millisTime }, Cmd.none )

        WriteSomeCode ->
            ( { model | manuallyGeneratedLoc = model.manuallyGeneratedLoc + 1, currentLoc = model.currentLoc + 1 }, Cmd.none )

        ConvertCodeToMoney ->
            let
                ( newLoc, newMoney, newManuallyGeneratedMoney ) =
                    if model.currentLoc >= 10 then
                        ( model.currentLoc - 10, model.currentMoney + 1, model.manuallyGeneratedMoney + 1 )

                    else
                        ( model.currentLoc, model.currentMoney, model.manuallyGeneratedMoney )

                checkAndUnlockCoder : Coder -> Coder
                checkAndUnlockCoder coder =
                    let
                        unlocked =
                            coder.alreadyUnlocked || isUnlocked coder.coderType model
                    in
                    { coder | alreadyUnlocked = unlocked }

                allUnlockedCoders =
                    List.map checkAndUnlockCoder model.coders
            in
            ( { model | coders = allUnlockedCoders, currentLoc = newLoc, currentMoney = newMoney, manuallyGeneratedMoney = newManuallyGeneratedMoney }, Cmd.none )

        BuyCoder coder ->
            let
                currentPrice =
                    calculatePrice coder.basePrice coder.priceMultiplier coder.count

                ( newMoney, newCoder ) =
                    if isUsable coder model then
                        ( model.currentMoney - currentPrice, { coder | count = coder.count + 1 } )

                    else
                        ( model.currentMoney, coder )

                updatedCoders =
                    -- updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
                    List.Extra.updateIf (\c -> c.coderType == coder.coderType) (\_ -> newCoder) model.coders
            in
            ( model, Cmd.none )

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
    let
        visibleCoders =
            List.filter .alreadyUnlocked model.coders

        oneCoder : Coder -> Html Msg
        oneCoder coder =
            li []
                [ buyButton model.icons coder (BuyCoder coder) (isUsable coder model)
                ]
    in
    div [ class "buying-area" ]
        [ ol []
            (List.map oneCoder visibleCoders)
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
      , coders = [ praktikant ]
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
