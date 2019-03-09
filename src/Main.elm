module Main exposing (Model, main)

import Browser exposing (Document)
import Html exposing (Html, div, li, p, text, ul)
import Html.Attributes as Attributes
import Html.Events
import LispParser
import Printer


type alias Model =
    { inputText : String
    , results : List String
    , number : Int
    }


type Msg
    = FormSubmitted
    | InputText String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputText = ""
      , results = []
      , number = 0
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
                [ Html.input
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
                [ Attributes.style "border" "1px solid black"
                , Attributes.style "padding" "10px"
                , Attributes.style "width" "70%"
                ]
                [ ul [] <|
                    List.map
                        (\r -> li [] [ text r ])
                        model.results
                ]
            , Html.p [] [ text <| String.fromInt model.number ]
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        FormSubmitted ->
            if String.length model.inputText == 0 then
                ( model, Cmd.none )

            else
                let
                    parsedString =
                        evalString model.inputText
                in
                ( { model
                    | number = model.number + 1
                    , results = parsedString :: model.results
                    , inputText = ""
                  }
                , Cmd.none
                )

        InputText s ->
            ( { model | inputText = s }, Cmd.none )


evalString : String -> String
evalString inp =
    let
        parsed =
            LispParser.parseSexp inp
    in
    Printer.toString parsed


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
