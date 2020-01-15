module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, div, h1, header, input, label, li, section, text, ul)
import Html.Attributes exposing (autofocus, class, name, placeholder, type_)
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Json



---- MODEL ----


type alias Todo =
    { id : Int
    , content : String
    , completed : Bool
    }


type alias Model =
    { todos : List Todo
    , uid : Int
    , inputText : String
    }


init : ( Model, Cmd Msg )
init =
    ( { todos = []
      , uid = 0
      , inputText = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Add
    | UpdateInputText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            ( { model
                | todos =
                    model.todos
                        ++ [ { id = model.uid + 1, content = model.inputText, completed = False } ]
                , uid = model.uid + 1
              }
            , Cmd.none
            )

        UpdateInputText str ->
            ( { model | inputText = str }
            , Cmd.none
            )


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



---- VIEW ----


renderTodo : Todo -> Html Msg
renderTodo todo =
    li []
        [ input [ class "toggle", type_ "checkbox" ] []
        , label [] [ text todo.content ]
        ]


renderTodos : List Todo -> List (Html Msg)
renderTodos todos =
    List.map
        renderTodo
        todos


view : Model -> Html Msg
view model =
    div [ class "todoapp" ]
        [ header []
            [ h1 []
                [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , autofocus True
                , name "newTodo"
                , onInput UpdateInputText
                , onEnter Add
                ]
                []
            ]
        , section
            [ class "main" ]
            [ ul [ class "todo-list" ] (renderTodos model.todos) ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
