port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (onEnter)
import Html.Extra as Html exposing (..)
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List.Extra exposing (find, updateIf)
import Task



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • TodoMVC", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = always Sub.none
        }



---- Init and Decoding the Model ----


type alias Flags =
    Maybe String


init : Flags -> ( Model, Cmd Msg )
init maybeModelToDecode =
    case maybeModelToDecode of
        Just modelToDecode ->
            case Decode.decodeString modelDecorder modelToDecode of
                Ok model ->
                    ( model, Cmd.none )

                Err _ ->
                    ( emptyModel, Cmd.none )

        Nothing ->
            ( emptyModel, Cmd.none )


modelDecorder : Decoder Model
modelDecorder =
    Decode.succeed Model
        |> Pipeline.required "todos" (Decode.list todoDecoder)
        |> Pipeline.required "uid" int
        |> Pipeline.required "inputText" string
        |> Pipeline.required "editingText" string
        |> Pipeline.required "visibility" visibiltyDecoder


todoDecoder : Decoder Todo
todoDecoder =
    Decode.succeed Todo
        |> Pipeline.required "id" int
        |> Pipeline.required "content" string
        |> Pipeline.required "completed" bool
        |> Pipeline.required "editing" bool


visibiltyDecoder : Decoder Visibility
visibiltyDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "All" ->
                        Decode.succeed All

                    "Active" ->
                        Decode.succeed Active

                    "Completed" ->
                        Decode.succeed Completed

                    other ->
                        Decode.fail <| "Unknown visibility: " ++ other
            )



---- Saving to Local Storage using Ports and Encoding the Model----


port setStorage : Encode.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage (modelEncoder newModel), cmds ]
    )


modelEncoder : Model -> Encode.Value
modelEncoder model =
    Encode.object
        [ ( "todos", Encode.list todoEncoder model.todos )
        , ( "uid", Encode.int model.uid )
        , ( "inputText", Encode.string model.inputText )
        , ( "editingText", Encode.string model.editingText )
        , ( "visibility", visibilityEncoder model.visibility )
        ]


todoEncoder : Todo -> Encode.Value
todoEncoder todo =
    Encode.object
        [ ( "id", Encode.int todo.id )
        , ( "content", Encode.string todo.content )
        , ( "completed", Encode.bool todo.completed )
        , ( "editing", Encode.bool todo.editing )
        ]


visibilityEncoder : Visibility -> Encode.Value
visibilityEncoder visibility =
    case visibility of
        All ->
            Encode.string "All"

        Active ->
            Encode.string "Active"

        Completed ->
            Encode.string "Completed"



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
        escKey =
            27

        isEsc code =
            if code == escKey then
                Decode.succeed msg

            else
                Decode.fail "not ESC"
    in
    on "keydown" (Decode.andThen isEsc keyCode)



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


viewVisibilityFilter : String -> String -> Visibility -> Html Msg
viewVisibilityFilter url name visibility =
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
            [ viewVisibilityFilter "#/" "All" All
            , viewVisibilityFilter "#/active" "Active" Active
            , viewVisibilityFilter "#/completed" "Completed" Completed
            ]
        , button
            [ class "clear-completed"
            , type_ "button"
            , hidden clearCompletedTodos
            , onClick ClearCompleted
            ]
            [ text "Clear completed" ]
        ]
