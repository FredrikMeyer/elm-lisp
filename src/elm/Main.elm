module Main exposing (Model, main)

import Browser
import Environment
import Eval
import Html exposing (Html, div, li, text, ul)
import Html.Attributes as Attributes
import Html.Events
import LispParser
import Ports exposing (cmdEnter)
import Printer
import Types exposing (Environment)


type alias Model =
    { inputText : String
    , inputs : List String
    , results : List String
    , environment : Environment
    }


initialModel : Model
initialModel =
    { inputText = ""
    , results = []
    , inputs = []
    , environment = Environment.initialEnvironment
    }


type Msg
    = FormSubmitted
    | InputText String
    | CmdEnter
    | ShortcutMsg Shortcut


type Shortcut
    = Lambda
    | Let
    | Def
    | Do
    | If


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    div []
        [ Html.h1
            [ Attributes.id "header"
            ]
            [ text "Give me some Lisp" ]
        , div [ Attributes.id "container" ]
            [ Html.form
                [ Html.Events.onSubmit FormSubmitted
                , Attributes.id "form"
                ]
                [ Html.textarea
                    [ Html.Events.onInput InputText
                    , Attributes.id "inputArea"
                    , Attributes.value model.inputText
                    ]
                    []
                , Html.button
                    [ Attributes.id "button"
                    ]
                    [ text "Submit"
                    ]
                ]
            , div
                [ Attributes.id "results"
                ]
                [ listOfValues model.inputs
                , listOfValues model.results
                ]
            ]
        , div [ Attributes.id "shortcuts" ]
            [ Html.button
                [ Html.Events.onClick <| ShortcutMsg Lambda
                ]
                [ text <| shortcutToString Lambda
                ]
            , Html.button
                [ Html.Events.onClick <| ShortcutMsg Let ]
                [ text <| shortcutToString Let
                ]
            , Html.button
                [ Html.Events.onClick <| ShortcutMsg Def ]
                [ text <| shortcutToString Def
                ]
            , Html.button
                [ Html.Events.onClick <| ShortcutMsg If ]
                [ text <| shortcutToString If ]
            , Html.button
                [ Html.Events.onClick <| ShortcutMsg Do ]
                [ text <| shortcutToString Do ]
            ]
        , div []
            [ text "See this page's "
            , Html.a
                [ Attributes.href "https://github.com/FredrikMeyer/elm-lisp" ]
                [ text " Github page." ]
            ]
        ]


listOfValues : List String -> Html Msg
listOfValues list =
    div
        [ Attributes.id "result"
        ]
        [ ul [] <|
            List.map
                (\r -> li [] [ text r ])
                list
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormSubmitted ->
            if String.length model.inputText == 0 then
                ( model, Cmd.none )

            else
                let
                    ( resultString, newEnvironment ) =
                        submitCommandToLisp model
                in
                ( { model
                    | results = resultString :: model.results
                    , environment = newEnvironment
                    , inputs = model.inputText :: model.inputs
                    , inputText = ""
                  }
                , Cmd.none
                )

        InputText s ->
            ( { model | inputText = s }, Cmd.none )

        CmdEnter ->
            let
                newModel =
                    let
                        ( resultString, newEnvironment ) =
                            submitCommandToLisp model
                    in
                    { model
                        | results = resultString :: model.results
                        , environment = newEnvironment
                        , inputs = model.inputText :: model.inputs
                        , inputText = ""
                    }
            in
            ( newModel, Cmd.none )

        ShortcutMsg shortcut ->
            let
                newInput =
                    model.inputText ++ " " ++ shortcutToString shortcut
            in
            ( { model | inputText = newInput }, Cmd.none )


shortcutToString : Shortcut -> String
shortcutToString shortcut =
    case shortcut of
        Lambda ->
            "(fn (x) x)"

        Let ->
            "(let (a 2) a)"

        Def ->
            "(def! a 2)"

        Do ->
            "(do (def! b 1) 3)"

        If ->
            "(if (> 2 3) 42 24)"


submitCommandToLisp : Model -> ( String, Environment )
submitCommandToLisp model =
    let
        env =
            model.environment

        parsedInput =
            LispParser.parseSexp model.inputText

        ( newEnv, result ) =
            Eval.eval env parsedInput
    in
    ( Printer.toString result, newEnv )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ cmdEnter (\_ -> CmdEnter)
        ]
