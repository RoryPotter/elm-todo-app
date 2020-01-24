module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (onEnter)
import Html.Extra as Html exposing (..)
import Json.Decode as Json
import List.Extra exposing (..)
import Task



---- MODEL ----


type alias Todo =
    { id : Int
    , content : String
    , completed : Bool
    , editing : Bool
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
    = NoOp
    | Add
    | UpdateInputText String
    | Check Int Bool
    | EditingTodo Int Bool
    | UpdateTodo Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add ->
            if model.inputText == "" then
                ( model, Cmd.none )

            else
                ( { model
                    | todos =
                        model.todos
                            ++ [ { id = model.uid + 1, content = String.trim model.inputText, completed = False, editing = False } ]
                    , uid = model.uid + 1
                    , inputText = ""
                  }
                , Cmd.none
                )

        UpdateInputText str ->
            ( { model | inputText = str }
            , Cmd.none
            )

        Check id checkboxValue ->
            ( { model
                | todos = updateIf (\todo -> todo.id == id) (\todo -> { todo | completed = checkboxValue }) model.todos
              }
            , Cmd.none
            )

        EditingTodo id isEditing ->
            ( { model
                | todos = updateIf (\todo -> todo.id == id) (\todo -> { todo | editing = isEditing }) model.todos
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus ("todo-" ++ String.fromInt id))
            )

        UpdateTodo id value ->
            ( { model
                | todos = updateIf (\todo -> todo.id == id) (\todo -> { todo | content = String.trim value }) model.todos
              }
            , Cmd.none
            )



---- VIEW ----


renderTodo : Todo -> Html Msg
renderTodo todo =
    li
        [ classList
            [ ( "completed", todo.completed )
            , ( "editing", todo.editing )
            ]
        ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", onCheck (Check todo.id) ] []
            , label [ onDoubleClick (EditingTodo todo.id True) ] [ text todo.content ]
            ]
        , input
            [ class "edit"
            , name "content"
            , value todo.content
            , id ("todo-" ++ String.fromInt todo.id)
            , onInput (UpdateTodo todo.id)
            , onBlur (EditingTodo todo.id False)
            , onEnter (EditingTodo todo.id False)
            ]
            []
        ]


renderTodos : List Todo -> List (Html Msg)
renderTodos todos =
    List.map
        renderTodo
        todos


view : Model -> Html Msg
view model =
    let
        isHidden =
            List.length model.todos == 0
    in
    div [ class "todoapp" ]
        [ header [ class "header" ]
            [ h1 []
                [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , autofocus True
                , value model.inputText
                , name "newTodo"
                , onInput UpdateInputText
                , onEnter Add
                ]
                []
            ]
        , section
            [ class "main", hidden isHidden ]
            [ ul [ class "todo-list" ] (renderTodos model.todos) ]
        , footer
            [ class "footer"
            , hidden isHidden
            ]
            [ text "" ]
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
