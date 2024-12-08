module Main exposing (Flags, Model, Msg, main)

import Array
import Browser
import Browser.Events
import Game
import Helpers
import Html
import Html.Attributes
import Json.Decode as Decode


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Game Game.Msg
    | OnVisibilityChange Browser.Events.Visibility


type alias Model =
    { gameModel : Game.Model }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Game (Game.subscriptions model.gameModel)
        , Browser.Events.onKeyDown (Decode.map (\s -> Game (Game.OnKeyDown s)) (Decode.field "key" Decode.string))
        , Browser.Events.onVisibilityChange OnVisibilityChange
        ]


type alias Flags =
    { hidden : Bool }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { isRunningInTerminal = False }
        |> Game.init
        |> (\game ->
                ( { gameModel =
                        if flags.hidden then
                            game.model
                                |> (\m -> { m | pause = True })

                        else
                            game.model
                  }
                , Cmd.map Game game.command
                )
           )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnVisibilityChange visibility ->
            case visibility of
                Browser.Events.Visible ->
                    ( model, Cmd.none )

                Browser.Events.Hidden ->
                    ( { model | gameModel = (\m -> { m | pause = True }) model.gameModel }, Cmd.none )

        Game msgGame ->
            Game.update msgGame model.gameModel
                |> (\game ->
                        ( { model | gameModel = game.model }
                        , Cmd.map Game game.command
                        )
                   )


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        ]
        ([ Html.node "style"
            []
            [ Html.text <|
                String.concat
                    [ "body    {background-color: black; color: white; font-size: 16px}"
                    , ".level  {color: #24b}" -- Dark Blue
                    , ".shield {color: #900}" -- Red
                    , ".dot    {color: #540}" -- Dark Yellow
                    , ".player {color: #ff4}" -- Yellow
                    , ".ghost0 {color: #0ee}" -- Cyan
                    , ".ghost1 {color: #fa0}" -- Orange
                    , ".ghost2 {color: #f00}" -- Red
                    , ".ghost3 {color: #faa}" -- Pink
                    , ".hunt   {color: #35c}" -- Blue
                    , ".modal  {color: white; background-color: #24b}" -- Pink
                    , ".modal-not-visible {color: #24b; background-color: #24b}" -- Pink
                    , ".invisible {color: rgba(0,0,0,0)}"
                    ]
            ]
         , Html.pre []
            (Array.toList
                (model.gameModel
                    |> Game.view
                        { charToTile = charToTile
                        , tilesToRow = tilesToRow
                        }
                )
            )
         ]
            ++ (if model.gameModel.debug then
                    [ Html.pre
                        [ Html.Attributes.style "color" "green"
                        , Html.Attributes.style "position" "fixed"
                        , Html.Attributes.style "left" "20px"
                        ]
                        [ Html.text <| Helpers.stringJoin "\n" (Game.debugText model.gameModel)
                        ]
                    ]

                else
                    []
               )
        )


tilesToRow : Int -> Array.Array (Html.Html msg) -> Html.Html msg
tilesToRow index tiles =
    if index == 0 then
        Html.div []
            [ Html.a
                [ Html.Attributes.href <| "https://" ++ Game.githubUrl
                , Html.Attributes.target "_blank"
                , Html.Attributes.style "text-decoration" "none"
                ]
                (Array.toList tiles)
            ]

    else
        Html.div [] (Array.toList tiles)


charToTile : Int -> Game.Tile -> String -> Game.Acc (Html.Html msg) -> Game.Acc (Html.Html msg)
charToTile index tileType char acc =
    let
        class : String
        class =
            tileTypeToClass index tileType
    in
    if acc.previousTileType == class then
        -- Char with same color
        { acc | textTemp = acc.textTemp ++ char }

    else if acc.previousTileType == "" then
        -- First char in the row
        { acc | textTemp = char, previousTileType = class }

    else
        -- Color variation
        { acc
            | textTemp = char
            , previousTileType = class
            , tiles =
                Array.push
                    (Html.span [ Html.Attributes.class acc.previousTileType ] [ Html.text acc.textTemp ])
                    acc.tiles
        }


tileTypeToClass : Int -> Game.Tile -> String
tileTypeToClass index charType =
    if index == 0 && charType /= Game.TileEndOfLine then
        "level"

    else
        case charType of
            Game.TileLevelWhilePlaying ->
                "level"

            Game.TileLevelWhileShield ->
                "shield"

            Game.TileLevelWhileIdle ->
                "dot"

            Game.TileDot ->
                "dot"

            Game.TileEndOfLine ->
                "invisible"

            Game.TilePlayer ->
                "player"

            Game.TileNotVisible ->
                "invisible"

            Game.TileModalNotVisible ->
                "modal-not-visible"

            Game.TileModalVisible ->
                "modal"

            Game.TileGhostEscaping id ->
                "ghost" ++ String.fromInt id

            Game.TileGhostHunting ->
                "hunt"

            Game.TileNoOp ->
                "dot"
