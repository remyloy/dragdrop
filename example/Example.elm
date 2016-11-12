module Example exposing (..)

import DragDrop exposing (DraggedItem)
import Html exposing (Html, Attribute, div, h3, text)
import Html.Attributes exposing (style)
import Html.App
import Mouse exposing (Position)


type Msg
    = LeftStartDrag String DraggedItem
    | RightStartDrag String DraggedItem
    | LeftEndDrag String
    | RightEndDrag String
    | Moving Position
    | CancelDrag


type alias Model =
    { left : List String, right : List String, draggedItem : Maybe ( String, DraggedItem ) }


init : ( Model, Cmd Msg )
init =
    { left = [ "Tomatoes", "Potatos", "Carrots", "Milk" ], right = [], draggedItem = Nothing } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LeftStartDrag i p ->
            { model | left = model.left |> List.filter (\x -> x /= i), draggedItem = Just ( i, p ) } ! []

        RightStartDrag i p ->
            { model | right = model.right |> List.filter (\x -> x /= i), draggedItem = Just ( i, p ) } ! []

        LeftEndDrag i ->
            { model | left = i :: model.left, draggedItem = Nothing } ! []

        RightEndDrag i ->
            { model | right = i :: model.right, draggedItem = Nothing } ! []

        Moving pos ->
            { model | draggedItem = model.draggedItem |> Maybe.map (\( x, item ) -> ( x, { item | position = pos } )) } ! []

        CancelDrag ->
            { model | left = model.draggedItem |> Maybe.map (\( i, p ) -> i :: model.left) |> Maybe.withDefault model.left, draggedItem = Nothing } ! []


view : Model -> Html Msg
view model =
    let
        leftItems =
            model.left
                |> List.map (\i -> div [ DragDrop.drag LeftStartDrag i ] [ text i ])

        rightItems =
            model.right
                |> List.map (\i -> div [ DragDrop.drag RightStartDrag i ] [ text i ])

        containerStyle =
            style [ ( "border", "1px solid blue" ), ( "width", "200px" ), ( "height", "200px" ) ]

        draggedContent =
            (model.draggedItem |> Maybe.map fst)
    in
        div [ style [ ( "display", "flex" ) ] ]
            [ div []
                [ h3 [] [ text "To buy" ]
                , div [ DragDrop.accept LeftEndDrag draggedContent, containerStyle ] leftItems
                ]
            , div []
                [ h3 [] [ text "In stock" ]
                , div [ DragDrop.accept RightEndDrag draggedContent, containerStyle ] rightItems
                ]
            , DragDrop.view model.draggedItem (\i -> div [] [ text i ])
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    DragDrop.subscriptions model.draggedItem Moving CancelDrag


main : Program Never
main =
    Html.App.program { init = init, update = update, view = view, subscriptions = subscriptions }
