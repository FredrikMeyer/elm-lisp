module Main exposing (Model, main)

import Browser exposing (Document)
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


view : Model -> Document Msg
view model =
    { title = "Let's Lisp"
    , body =
        [ div []
            [ Html.h1 [] [ text "Give me some Lisp" ]
            , Html.form
                [ Html.Events.onSubmit FormSubmitted
                ]
                [ Html.textarea
                    [ Html.Events.onInput InputText
                    , Attributes.style "width" "70%"
                    , Attributes.value model.inputText
                    ]
                    []
                , Html.button
                    []
                    [ text "Trykk"
                    ]
                ]
            , div
                [ Attributes.style "display" "flex"
                ]
                [ listOfValues model.inputs
                , listOfValues model.results
                ]
            ]
        ]
    }


listOfValues : List String -> Html Msg
listOfValues list =
    div
        [ Attributes.style "border" "1px solid black"
        , Attributes.style "padding" "10px"
        , Attributes.style "width" "70%"
        ]
        [ ul [] <|
            List.map
                (\r -> li [] [ text r ])
                list
        ]


update : Msg -> Model -> ( Model, Cmd msg )
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


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
