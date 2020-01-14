module Main exposing (main)

import Browser
import Html exposing (Html, div, footer, h1, header, input, li, p, section, text, ul)
import Html.Attributes exposing (autofocus, class, id, placeholder)



---- MODEL ----


type alias Todo =
    { content : String
    , completed : Bool
    }


type alias Todos =
    List Todo


type alias Model =
    Maybe Todos


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


renderTodo : Todo -> Html Msg
renderTodo todo =
    li []
        [ p [] [ text todo.content ]
        ]


renderTodos : Maybe Todos -> List (Html Msg)
renderTodos maybeTodos =
    case maybeTodos of
        Just todos ->
            List.map
                renderTodo
                todos

        Nothing ->
            [ text "" ]


view : Model -> Html Msg
view model =
    div [ class "todoapp" ]
        [ header []
            [ h1 []
                [ text "todos" ]
            , input [ class "new-todo", placeholder "What needs to be done?", autofocus True ]
                []
            ]
        , section
            [ class "main" ]
            [ ul [] (renderTodos model) ]
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
