module Main exposing (Model, main)

import Browser
import Environment
import Eval
import Html exposing (Html, div, li, text, ul)
import Html.Attributes as Attributes
import Html.Events
import LispParser
import Printer
import Types exposing (Environment)


type alias Model =
    { inputText : String
    , inputs : List String
    , results : List String
    , environment : Environment
    }


type Msg
    = FormSubmitted
    | InputText String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputText = ""
      , results = []
      , inputs = []
      , environment = Environment.initialEnvironment
      }
    , Cmd.none
    )


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
                    [ text "Trykk"
                    ]
                ]
            , div
                [ Attributes.id "results"
                ]
                [ listOfValues model.inputs
                , listOfValues model.results
                ]
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
                    env =
                        model.environment

                    parsedInput =
                        LispParser.parseSexp model.inputText

                    ( newEnv, result ) =
                        Eval.eval env parsedInput

                    res =
                        Printer.toString result
                in
                ( { model
                    | results = res :: model.results
                    , environment = newEnv
                    , inputs = model.inputText :: model.inputs
                    , inputText = ""
                  }
                , Cmd.none
                )

        InputText s ->
            ( { model | inputText = s }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
