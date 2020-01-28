module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (onEnter)
import Html.Extra as Html exposing (..)
import Json.Decode as Json
import List.Extra exposing (find, updateIf)
import Task



---- MODEL ----


escKey : Int
escKey =
    27


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
    , editingText : String
    , visibility : Visibility
    }


type Visibility
    = All
    | Active
    | Completed


newTodo : Int -> String -> Todo
newTodo id text =
    { id = id
    , content = String.trim text
    , completed = False
    , editing = False
    }


emptyModel : Model
emptyModel =
    { todos = []
    , uid = 0
    , inputText = ""
    , editingText = ""
    , visibility = All
    }



---- UPDATE ----


type Msg
    = NoOp
    | Add
    | UpdateInputText String
    | Check Int Bool
    | StartEditingTodo Int
    | UpdateTodo Int String
    | StopEditingTodo Int
    | DiscardTodoUpdate Int
    | Delete Int
    | ToggleAll Bool
    | ClearCompleted
    | ChangeVisibility Visibility


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
                            ++ [ newTodo model.uid model.inputText ]
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

        StartEditingTodo id ->
            ( { model
                | todos = updateIf (\todo -> todo.id == id) (\todo -> { todo | editing = True }) model.todos
                , editingText =
                    case find (\todo -> todo.id == id) model.todos of
                        Just todo ->
                            todo.content

                        Nothing ->
                            ""
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus ("todo-" ++ String.fromInt id))
            )

        UpdateTodo id value ->
            ( { model
                | todos = updateIf (\todo -> todo.id == id) (\todo -> { todo | content = String.trim value }) model.todos
              }
            , Cmd.none
            )

        StopEditingTodo id ->
            let
                todoContent =
                    case find (\todo -> todo.id == id) model.todos of
                        Just todo ->
                            todo.content

                        Nothing ->
                            ""

                isTodoEmpty =
                    todoContent == ""
            in
            if isTodoEmpty then
                ( { model | todos = remove model.todos id }, Cmd.none )

            else
                ( { model
                    | todos = updateIf (\todo -> todo.id == id) (\todo -> { todo | editing = False }) model.todos
                  }
                , Cmd.none
                )

        DiscardTodoUpdate id ->
            ( { model
                | todos = updateIf (\todo -> todo.id == id) (\todo -> { todo | content = model.editingText, editing = False }) model.todos
              }
            , Cmd.none
            )

        Delete id ->
            ( { model | todos = remove model.todos id }
            , Cmd.none
            )

        ToggleAll allCompleted ->
            ( { model
                | todos = List.map (\todo -> { todo | completed = allCompleted }) model.todos
              }
            , Cmd.none
            )

        ClearCompleted ->
            ( { model
                | todos = List.filter (\todo -> not todo.completed) model.todos
              }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )


remove : List Todo -> Int -> List Todo
remove todos id =
    List.filter (\todo -> todo.id /= id) todos


onEsc : Msg -> Attribute Msg
onEsc msg =
    let
        isEsc code =
            if code == escKey then
                Json.succeed msg

            else
                Json.fail "not ESC"
    in
    on "keydown" (Json.andThen isEsc keyCode)



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
            [ input [ class "toggle", type_ "checkbox", checked todo.completed, onCheck (Check todo.id) ] []
            , label [ onDoubleClick (StartEditingTodo todo.id) ] [ text todo.content ]
            , button [ class "destroy", type_ "button", onClick (Delete todo.id) ] []
            ]
        , input
            [ class "edit"
            , name "content"
            , value todo.content
            , id ("todo-" ++ String.fromInt todo.id)
            , onInput (UpdateTodo todo.id)
            , onBlur (StopEditingTodo todo.id)
            , onEnter (StopEditingTodo todo.id)
            , onEsc (DiscardTodoUpdate todo.id)
            ]
            []
        ]


todoCountView : Int -> Html Msg
todoCountView numberOfTodos =
    let
        item_pluralize =
            if numberOfTodos == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt numberOfTodos) ]
        , text (item_pluralize ++ " left")
        ]


visibilityFilter : String -> String -> Visibility -> Html Msg
visibilityFilter url name visibility =
    li []
        [ a [ class "", href url, onClick (ChangeVisibility visibility) ] [ text name ]
        , text " "
        ]


view : Model -> Html Msg
view model =
    let
        isHidden =
            List.length model.todos == 0

        visableTodos =
            case model.visibility of
                All ->
                    model.todos

                Active ->
                    List.filter (\todo -> not todo.completed) model.todos

                Completed ->
                    List.filter (\todo -> todo.completed) model.todos
    in
    div [ class "todoapp" ]
        [ viewHeader model.inputText
        , viewTodos visableTodos isHidden
        , viewFooter model.todos isHidden
        ]


viewHeader : String -> Html Msg
viewHeader inputText =
    header [ class "header" ]
        [ h1 []
            [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value inputText
            , name "newTodo"
            , onInput UpdateInputText
            , onEnter Add
            ]
            []
        ]


viewTodos : List Todo -> Bool -> Html Msg
viewTodos todos isHidden =
    section
        [ class "main", hidden isHidden ]
        [ input [ class "toggle-all", id "toggle-all", type_ "checkbox", onCheck ToggleAll ] []
        , label [ for "toggle-all" ] [ text "Mark all as complete" ]
        , ul [ class "todo-list" ] (List.map renderTodo todos)
        ]


viewFooter : List Todo -> Bool -> Html Msg
viewFooter todos isHidden =
    let
        numberOfCompletedTodos =
            List.length (List.filter (\todo -> todo.completed) todos)

        numberOfTodosLeft =
            List.length todos - numberOfCompletedTodos

        clearCompletedTodos =
            not (numberOfCompletedTodos > 0)
    in
    footer
        [ class "footer"
        , hidden isHidden
        ]
        [ todoCountView numberOfTodosLeft
        , ul [ class "filters" ]
            [ visibilityFilter "#/" "All" All
            , visibilityFilter "#/active" "Active" Active
            , visibilityFilter "#/completed" "Completed" Completed
            ]
        , button
            [ class "clear-completed"
            , type_ "button"
            , hidden clearCompletedTodos
            , onClick ClearCompleted
            ]
            [ text "Clear completed" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( emptyModel
                , Cmd.none
                )
        , view = \model -> { title = "Elm • TodoMVC", body = [ view model ] }
        , update = update
        , subscriptions = always Sub.none
        }
