module DragDrop exposing (DraggedItem, drag, accept, subscriptions, view)

import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (on, onWithOptions)
import Json.Decode exposing ((:=))
import Mouse exposing (Position)


type alias DraggedItem =
    { offset : Position, position : Position }


drag : (x -> DraggedItem -> msg) -> x -> Attribute msg
drag tagger elem =
    let
        decoder =
            Json.Decode.object2 tagger
                (Json.Decode.succeed elem)
                draggedItem

        draggedItem =
            Json.Decode.object2 DraggedItem
                offset
                position

        offset =
            Json.Decode.object2 Position
                ("offsetX" := Json.Decode.int)
                ("offsetY" := Json.Decode.int)

        position =
            Json.Decode.object2 Position
                ("pageX" := Json.Decode.int)
                ("pageY" := Json.Decode.int)

        options =
            { stopPropagation = True, preventDefault = True }
    in
        onWithOptions "mousedown" options decoder


accept : (x -> msg) -> Maybe x -> Attribute msg
accept tagger elem =
    let
        decoder =
            elem
                |> Maybe.map tagger
                |> Maybe.map Json.Decode.succeed
                |> Maybe.withDefault (Json.Decode.fail "nothing is dragged")

        options =
            { stopPropagation = True, preventDefault = True }
    in
        onWithOptions "mouseup" options decoder


subscriptions : Maybe x -> (Position -> msg) -> msg -> Sub msg
subscriptions dragged moveTagger cancelTagger =
    case dragged of
        Just _ ->
            Sub.batch [ Mouse.moves moveTagger, Mouse.ups (\_ -> cancelTagger) ]

        Nothing ->
            Sub.none


view : Maybe ( x, DraggedItem ) -> (x -> Html msg) -> Html msg
view dragged f =
    case dragged of
        Just ( data, { offset, position } ) ->
            let
                px i =
                    (toString i) ++ "px"

                left =
                    px (position.x - offset.x)

                top =
                    px (position.y - offset.y)
            in
                div [ style [ ( "position", "fixed" ), ( "left", left ), ( "top", top ), ( "pointer-events", "none" ), ( "z-index", "2" ) ] ] [ (f data) ]

        Nothing ->
            text ""
