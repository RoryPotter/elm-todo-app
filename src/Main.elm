module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, h4, li, p, text, ul)
import Html.Attributes exposing (id)



---- MODEL ----


type alias Todo =
    { title : String
    , content : String
    }


type alias Todos =
    List Todo


type alias Model =
    Maybe Todos


init : ( Model, Cmd Msg )
init =
    ( Just [ { title = "Test 1", content = "Test content" } ], Cmd.none )



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
        [ h4 [] [ text todo.title ]
        , p [] [ text todo.content ]
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
    div []
        [ h1 []
            [ text "Todo App" ]
        , div
            [ id "main" ]
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
