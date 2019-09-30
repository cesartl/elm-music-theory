module Main exposing (Accidental(..), Model, Msg(..), Note(..), init, main, next, toString, toStringWith, transpose, update, view)

import Browser
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import State exposing (State, advance, andThen, run, state, traverse)


main =
    Browser.sandbox { init = init, update = update, view = view }


{-| This note is also known as pitch class, because it describes every
occurrence of that note in any octave. Every accidental is described using
the flat notation for consistency.
-}
type Note
    = A
    | Bb
    | B
    | C
    | Db
    | D
    | Eb
    | E
    | F
    | Gb
    | G
    | Ab


allNotes =
    [ A, Bb, B, C, Db, D, Eb, E, F, Gb, G, Ab ]


parseNote : String -> Maybe Note
parseNote str =
    case str of
        "A" ->
            Just A
        "B" ->
            Just B
        "Bb" ->
            Just Bb

        "C" ->
            Just C

        "Db" ->
            Just Db

        "D" ->
            Just D

        "Eb" ->
            Just Eb

        "E" ->
            Just E

        "F" ->
            Just F

        "Gb" ->
            Just Gb

        "G" ->
            Just G

        "Ab" ->
            Just Ab

        _ ->
            Nothing


{-| This type describes accidentals. Sometimes accidentals can be enharmonic,
which means that they describe the same pitch: one example of this is A Sharp
B Flat.
-}
type Accidental
    = Flat
    | Sharp


next : Note -> Note
next note =
    case note of
        A ->
            Bb

        Bb ->
            B

        B ->
            C

        C ->
            Db

        Db ->
            D

        D ->
            Eb

        Eb ->
            E

        E ->
            F

        F ->
            Gb

        Gb ->
            G

        G ->
            Ab

        Ab ->
            A


transpose : Int -> Note -> Note
transpose count note =
    case count of
        0 ->
            note

        n ->
            transpose (n - 1) (next note)


{-| Converts a note to String.
-}
toString : Note -> String
toString note =
    toStringWith Sharp note


{-| Converts a note to String. You can decide how to display accidentals.
-}
toStringWith : Accidental -> Note -> String
toStringWith accidental note =
    case note of
        A ->
            "A"

        Bb ->
            case accidental of
                Flat ->
                    "Bb"

                Sharp ->
                    "A#"

        B ->
            "B"

        C ->
            "C"

        Db ->
            case accidental of
                Flat ->
                    "Db"

                Sharp ->
                    "C#"

        D ->
            "D"

        Eb ->
            case accidental of
                Flat ->
                    "Eb"

                Sharp ->
                    "D#"

        E ->
            "E"

        F ->
            "F"

        Gb ->
            case accidental of
                Flat ->
                    "Gb"

                Sharp ->
                    "F#"

        G ->
            "G"

        Ab ->
            case accidental of
                Flat ->
                    "Ab"

                Sharp ->
                    "G#"


type Interval
    = W
    | H


intervalToString : Interval -> String
intervalToString i =
    case i of
        W ->
            "W"

        H ->
            "H"


type alias ScaleBluePrint =
    List Interval


majorScale : ScaleBluePrint
majorScale =
    [ W, W, H, W, W, W, H ]


applyInterval : Interval -> Note -> Note
applyInterval interval =
    case interval of
        W ->
            transpose 2

        H ->
            transpose 1

type alias Scale = List Note

-- Creates a scale from a blue print and a root note
-- This uses the State (https://package.elm-lang.org/packages/folkertdev/elm-state/latest/State) package
-- The key idea is to use traverse : (a -> State s b) -> List a -> State s (List b)
-- where a=Interval, s=b=Note
-- function is Interval -> State Note Note and it uses State.advance which just creates a State by wrapping a function
-- the created state outputs the current note and sets the next state note by applying the interval
-- At the end we have a State Note Scale so we can use State.run to give it a root note and get the list of all the notes in the scale
makeScale : ScaleBluePrint -> Note -> Scale
makeScale blueprint root =
    let
        f : Interval -> State Note Note
        f interval =
            advance (\note -> ( note, applyInterval interval note ))

        notesState : State Note Scale
        notesState =
            traverse f blueprint
    in
    Tuple.first (run root notesState)


-- This rotates a blue print by the given offset
shiftBluePrint : Int -> ScaleBluePrint -> ScaleBluePrint
shiftBluePrint n blueprint =
    let
        l1 =
            List.drop n blueprint

        l2 =
            List.take n blueprint
    in
    List.append l1 l2


type Mode
    = Mode String Int


find : (a -> Bool) -> List a -> Maybe a
find f list =
    list
        |> List.filter f
        |> List.head


modes =
    [ Mode "Ionian (Major)" 0
    , Mode "Dorian" 1
    , Mode "Phrygian" 2
    , Mode "Lydian" 3
    , Mode "Mixolydian" 4
    , Mode "Aeolian (Minor)" 5
    , Mode "Locrian" 6
    ]

-- For a given scale, returns the list of notes which have the same note in their major scales (not necessarily in the same order)
findEquivalent : Scale -> List Note
findEquivalent notes =
    let
        f : Note -> Bool
        f root =
            let
                rootNotes =
                    makeScale majorScale root

                existInScale : Note -> Bool
                existInScale =
                    \n -> List.member n rootNotes
            in
            List.all existInScale notes
    in
    List.filter f allNotes


renderMode : Note -> Mode -> Html.Html msg
renderMode note mode =
    case mode of
        Mode name offset ->
            let
                blueprint =
                    shiftBluePrint offset majorScale

                notes =
                    makeScale blueprint note

                notesStr =
                    List.map toString notes
                        |> String.join ","

                intervalStr : String
                intervalStr =
                    blueprint
                        |> List.map intervalToString
                        |> String.join ","

                equivalentMajorScale =
                    findEquivalent notes
                        |> List.map toString
                        |> String.join ","
            in
            div []
                [ span [ attribute "style" "width: 120px; display: inline-block" ]
                    [ text name ]
                , span [ attribute "style" "width: 140px; display: inline-block" ]
                    [ text intervalStr
                    ]
                , span [ attribute "style" "width: 150px; display: inline-block" ]
                    [ text notesStr ]
                , span []
                    [ text equivalentMajorScale ]
                ]


type alias Model =
    { rootTxt : String
    , root : Maybe Note
    }


init =
    { rootTxt = "C"
    , root = Just C
    }


type Msg
    = ChangeKey String


update msg model =
    case msg of
        ChangeKey key ->
            { rootTxt = key, root = parseNote key }


view model =
    div []
        [ div [] [ text "Enter root note" ]
        , input [ placeholder "Root key", value model.rootTxt, onInput ChangeKey ] []
        , div [ attribute "style" "margin-top: 10px; font-weight: bold" ]
            [ span [ attribute "style" "width: 120px; display: inline-block" ]
                [ text "Mode" ]
            , span [ attribute "style" "width: 140px; display: inline-block" ]
                [ text "Intervals"
                ]
            , span [ attribute "style" "width: 150px; display: inline-block" ]
                [ text "Notes" ]
            , span []
                [ text "Same notes as" ]
            ]
        , div []
            (case model.root of
                Just note ->
                    List.map (renderMode note) modes

                Nothing ->
                    [ div [] [] ]
            )
        ]
