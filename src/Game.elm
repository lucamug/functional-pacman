module Game exposing
    ( Acc
    , Board
    , Character
    , Direction(..)
    , Flavor(..)
    , Game
    , GameStatus(..)
    , Level
    , Model
    , Msg(..)
    , PlayingMode(..)
    , Position
    , Tile(..)
    , debugText
    , init
    , subscriptions
    , update
    , view
    )

import Array
import Helpers
import Time


type alias Model =
    Game
        { fps : Int
        , keyDown : String
        , debug : Bool
        , shield : Bool
        , initGhostsQuantity : Int
        , initFps : Int
        , isRunningInTerminal : Bool
        }


type alias Game a =
    { a
        | player : Character
        , ghosts : Array.Array Character
        , level : Level
        , counter : Int
        , points : Int
        , pause : Bool
        , status : GameStatus
        , initialDots : Int
        , dots : Int
        , playingMode : PlayingMode
        , currentLevelId : Int
    }


type alias Level =
    { teleports : Array.Array { a : Position {}, b : Position {} }
    , positionSpawnGhosts : Position {}
    , positionAndDirectionSpawnPlayer : Position { direction : Direction }
    , board : Board
    , width : Int
    , height : Int
    , dots : Int
    }


type alias Board =
    Array.Array String


type alias Position a =
    { a | x : Int, y : Int }


type alias Character =
    Position
        { char : String
        , direction : Direction
        , flavor : Flavor
        }


type PlayingMode
    = HuntingGhosts { since : Int }
    | Escaping


type GameStatus
    = Idle
    | Playing
    | Quitting
    | Won
    | Lost
    | Menu


type Msg
    = Every Time.Posix
    | OnKeyDown String


type Flavor
    = Player
    | Ghost


type Direction
    = Up
    | Down
    | Left
    | Right


type Tile
    = TileLevelWhilePlaying
    | TileLevelWhileIdle
    | TileLevelWhileShield
    | TileDot
    | TileEndOfLine
    | TilePlayer
    | TileNotVisible
    | TileModalNotVisible
    | TileModalVisible
    | TileGhostEscaping Int
    | TileGhostHunting
    | TileNoOp


directions : Array.Array Direction
directions =
    Helpers.arrayFromList
        [ Up
        , Down
        , Left
        , Right
        ]


ghostsTypes : Int
ghostsTypes =
    Array.length charGhosts


initGhost : Position {} -> Int -> Character
initGhost positionSpawnGhosts index =
    { char =
        charGhosts
            |> Array.get (Helpers.modBy ghostsTypes index)
            |> Maybe.withDefault ""
    , x = positionSpawnGhosts.x
    , y = positionSpawnGhosts.y
    , flavor = Ghost
    , direction = Left
    }


initPlayer : Position { direction : Direction } -> Character
initPlayer positionAndDirectionSpawnPlayer =
    { char = playerShapes.right
    , x = positionAndDirectionSpawnPlayer.x
    , y = positionAndDirectionSpawnPlayer.y
    , flavor = Player
    , direction = positionAndDirectionSpawnPlayer.direction
    }


init : { isRunningInTerminal : Bool } -> { model : Model, command : Cmd msg }
init isRunningInTerminal =
    { model = initModel { levelId = 0 } { initFps = 30, initGhostsQuantity = 20 } isRunningInTerminal
    , command = Cmd.none
    }


initModel : { levelId : Int } -> { initFps : Int, initGhostsQuantity : Int } -> { isRunningInTerminal : Bool } -> Model
initModel { levelId } args { isRunningInTerminal } =
    let
        game : Game {}
        game =
            initGame_
                (Maybe.withDefault level0 <| Array.get levelId levels)
                { levelId = levelId
                , ghostsQuantity = args.initGhostsQuantity
                , status = Idle
                }
    in
    { player = game.player
    , ghosts = game.ghosts
    , level = game.level
    , counter = game.counter
    , points = game.points
    , pause = game.pause
    , status = game.status
    , initialDots = game.initialDots
    , dots = game.dots
    , playingMode = game.playingMode
    , currentLevelId = game.currentLevelId
    , fps = args.initFps
    , keyDown = ""
    , debug = False
    , shield = False
    , initFps = args.initFps
    , initGhostsQuantity = args.initGhostsQuantity
    , isRunningInTerminal = isRunningInTerminal
    }


initGame : { levelId : Int, ghostsQuantity : Int, status : GameStatus } -> Model -> Model
initGame args model =
    case Array.get args.levelId levels of
        Just level ->
            let
                game : Game {}
                game =
                    initGame_ level args
            in
            { model
                | player = game.player
                , ghosts = game.ghosts
                , level = game.level
                , counter = game.counter
                , points = game.points
                , pause = game.pause
                , status = game.status
                , initialDots = game.initialDots
                , dots = game.dots
                , playingMode = game.playingMode
                , currentLevelId = game.currentLevelId
            }

        Nothing ->
            model


initGame_ : Level -> { levelId : Int, ghostsQuantity : Int, status : GameStatus } -> Game {}
initGame_ level args =
    let
        ghosts : Array.Array Character
        ghosts =
            Helpers.arrayIndexedMap
                (\index _ -> initGhost level.positionSpawnGhosts index)
                (Array.repeat args.ghostsQuantity 0)
    in
    { player = initPlayer level.positionAndDirectionSpawnPlayer
    , ghosts = ghosts
    , level = level
    , counter = 0
    , points = 0
    , pause = False
    , status = args.status
    , initialDots = level.dots
    , dots = level.dots
    , playingMode = Escaping
    , currentLevelId = args.levelId
    }


removeMarkingTiles : Board -> Board
removeMarkingTiles board =
    Array.foldl
        (\char acc ->
            replace char tiles.canGoButNoDot acc
        )
        board
        (Helpers.arrayFromList
            [ tiles.spawnPlayerLeft
            , tiles.spawnPlayerUp
            , tiles.spawnPlayerRight
            , tiles.spawnPlayerDown
            , tiles.spawnGhosts
            , tiles.teleportUpDown
            , tiles.teleportLeftRight
            ]
        )


replace : String -> String -> Board -> Board
replace a b board =
    Array.map (String.replace a b) board


getPositionAndDirectionSpawnPlayer : Board -> Position { direction : Direction }
getPositionAndDirectionSpawnPlayer board =
    case Array.get 0 (findCharInBoard tiles.spawnPlayerLeft board) of
        Just p ->
            { x = p.x, y = p.y, direction = Left }

        Nothing ->
            case Array.get 0 (findCharInBoard tiles.spawnPlayerUp board) of
                Just p ->
                    { x = p.x, y = p.y, direction = Up }

                Nothing ->
                    case Array.get 0 (findCharInBoard tiles.spawnPlayerRight board) of
                        Just p ->
                            { x = p.x, y = p.y, direction = Right }

                        Nothing ->
                            case Array.get 0 (findCharInBoard tiles.spawnPlayerDown board) of
                                Just p ->
                                    { x = p.x, y = p.y, direction = Down }

                                Nothing ->
                                    { x = 3, y = 1, direction = Right }


getPositionSpawnGhosts : Board -> Position {}
getPositionSpawnGhosts board =
    case Array.get 0 (findCharInBoard tiles.spawnGhosts board) of
        Just p ->
            { x = p.x, y = p.y }

        Nothing ->
            { x = 1, y = 1 }


boardToLevel : Board -> Level
boardToLevel board =
    let
        boardWidth : Int
        boardWidth =
            board
                |> Array.get 0
                |> Maybe.map String.length
                |> Maybe.withDefault 10

        boardDots : Int
        boardDots =
            Helpers.countOccurrences
                (Helpers.arrayFromList [ tiles.dotLarge, tiles.dotSmall ])
                (Helpers.stringJoin "" board)

        teleports : Array.Array { a : Position {}, b : Position {} }
        teleports =
            let
                getTeleport : String -> Maybe { a : Position {}, b : Position {} }
                getTeleport char =
                    let
                        teleports_ : Array.Array (Position {})
                        teleports_ =
                            findCharInBoard char board
                    in
                    case Array.get 0 teleports_ of
                        Just a ->
                            case Array.get 1 teleports_ of
                                Just b ->
                                    Just { a = a, b = b }

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
            in
            Array.foldl
                (\char acc ->
                    case getTeleport char of
                        Just teleport ->
                            Helpers.arrayPushLast teleport acc

                        Nothing ->
                            acc
                )
                Array.empty
                (Helpers.arrayFromList [ tiles.teleportLeftRight, tiles.teleportUpDown ])
    in
    { teleports = teleports
    , positionSpawnGhosts = getPositionSpawnGhosts board
    , positionAndDirectionSpawnPlayer = getPositionAndDirectionSpawnPlayer board
    , board = removeMarkingTiles board
    , width = boardWidth
    , height = Array.length board
    , dots = boardDots
    }


level0 : Level
level0 =
    boardToLevel board0


levels : Array.Array Level
levels =
    Helpers.arrayFromList
        [ level0
        , boardToLevel board1
        , boardToLevel board2
        , boardToLevel board3
        ]


keyToMaybeDirection : String -> Maybe Direction
keyToMaybeDirection key =
    if key == "w" || key == "ArrowUp" then
        Just Up

    else if key == "a" || key == "ArrowLeft" then
        Just Left

    else if key == "s" || key == "ArrowDown" then
        Just Down

    else if key == "d" || key == "ArrowRight" then
        Just Right

    else
        Nothing


handleChangeDirectionOfPlayer : String -> Character -> Board -> Direction
handleChangeDirectionOfPlayer key player board =
    case keyToMaybeDirection key of
        Just direction ->
            if canGoPlayer (levelGet (move direction player) board) then
                direction

            else
                player.direction

        Nothing ->
            player.direction


update : Msg -> Model -> { model : Model, command : Cmd Msg }
update msg model =
    { model = update_ msg model
    , command = Cmd.none
    }


update_ : Msg -> Model -> Model
update_ msg model =
    case msg of
        OnKeyDown key ->
            model
                |> (\m -> { m | keyDown = key })
                |> (\m ->
                        if m.pause && key /= "Meta" then
                            { m | pause = False }

                        else if key == "b" then
                            { m | debug = not m.debug }

                        else if m.status == Quitting then
                            { m
                                | status =
                                    if key == "y" then
                                        Idle

                                    else if key == "n" then
                                        Playing

                                    else
                                        m.status
                            }

                        else if "1" <= key && key <= "9" then
                            let
                                levelId : Int
                                levelId =
                                    key
                                        |> String.toInt
                                        |> Maybe.withDefault 0
                                        |> (\lid -> lid - 1)
                            in
                            initGame { levelId = levelId, ghostsQuantity = Array.length m.ghosts, status = m.status } m

                        else
                            { m
                                | status =
                                    if m.status == Playing && key == "q" then
                                        Quitting

                                    else
                                        m.status
                                , pause =
                                    if key == " " then
                                        not m.pause

                                    else
                                        m.pause
                                , fps =
                                    if key == "y" && m.fps >= 2 then
                                        m.fps - 1

                                    else if key == "u" && m.fps <= 59 then
                                        m.fps + 1

                                    else
                                        m.fps
                                , ghosts =
                                    if key == "i" then
                                        Array.slice 0 (Array.length m.ghosts - 1) m.ghosts

                                    else if key == "o" then
                                        Helpers.arrayPushLast (initGhost m.level.positionSpawnGhosts (Array.length m.ghosts)) m.ghosts

                                    else
                                        m.ghosts
                                , shield =
                                    if key == "l" then
                                        not m.shield

                                    else
                                        m.shield
                            }
                   )
                |> (\m ->
                        if m.status == Idle || m.status == Won || m.status == Lost || m.status == Menu then
                            if key == "Enter" then
                                if model.fps == model.initFps then
                                    -- The game is still in the initial configuration (DEMO MODE)
                                    initGame { levelId = m.currentLevelId, ghostsQuantity = 4, status = Playing } { m | fps = 8, shield = False }

                                else
                                    initGame { levelId = m.currentLevelId, ghostsQuantity = Array.length m.ghosts, status = Playing } m

                            else if key == "f" then
                                initGame { levelId = m.currentLevelId, ghostsQuantity = Array.length m.ghosts, status = Playing } { m | fps = model.initFps, shield = True }

                            else if key == "m" then
                                if m.status == Menu then
                                    { m | status = Idle }

                                else
                                    { m | status = Menu }

                            else
                                m

                        else
                            m
                   )

        Every posix ->
            if model.status == Idle && Helpers.modBy 150 (model.counter + 1) == 0 then
                initGame
                    { levelId = Helpers.modBy (Array.length levels) (model.currentLevelId + 1)
                    , ghostsQuantity = Array.length model.ghosts
                    , status = model.status
                    }
                    model

            else
                let
                    newPlayer : Character
                    newPlayer =
                        (if model.status == Playing then
                            let
                                levelWithPlayerAndGhosts : Board
                                levelWithPlayerAndGhosts =
                                    addPlayerAndGhostsToLevel model.status model.ghosts model.player model.level.board

                                requestToChangeDirectionOfPlayer : Direction
                                requestToChangeDirectionOfPlayer =
                                    handleChangeDirectionOfPlayer model.keyDown model.player levelWithPlayerAndGhosts
                            in
                            moveIfPossiblePlayer
                                levelWithPlayerAndGhosts
                                ((\p -> { p | direction = requestToChangeDirectionOfPlayer }) model.player)

                         else
                            model.player
                        )
                            |> teleportPlayer model.level.teleports

                    newGhosts : { ghosts : Array.Array Character, isHit : Bool }
                    newGhosts =
                        calculateNewGhostsPositions
                            { ghosts = model.ghosts
                            , playingMode = model.playingMode
                            , shield = model.shield
                            , status = model.status
                            , level = model.level
                            }
                            newPlayer
                            posix

                    squareUnderPlayer : String
                    squareUnderPlayer =
                        levelGet newPlayer model.level.board

                    newDots : Int
                    newDots =
                        if isDot squareUnderPlayer then
                            model.dots - 1

                        else
                            model.dots

                    isWon : Bool
                    isWon =
                        model.status == Playing && newDots == 0
                in
                { model
                    | player = newPlayer
                    , points =
                        if newGhosts.isHit && model.status == Playing && model.playingMode /= Escaping then
                            model.points + 1

                        else
                            model.points
                    , playingMode =
                        if squareUnderPlayer == tiles.dotLarge then
                            HuntingGhosts { since = model.counter }

                        else
                            case model.playingMode of
                                Escaping ->
                                    Escaping

                                HuntingGhosts { since } ->
                                    if model.counter - since > 100 then
                                        Escaping

                                    else
                                        model.playingMode
                    , ghosts = newGhosts.ghosts
                    , counter = model.counter + 1
                    , dots = newDots
                    , level =
                        if isDot squareUnderPlayer then
                            (\l -> { l | board = boardSetDoubleWidth tiles.canGoButNoDot newPlayer l.board }) model.level

                        else
                            model.level
                    , status =
                        if isWon then
                            Won

                        else
                            let
                                isLost : Bool
                                isLost =
                                    newGhosts.isHit && model.status == Playing && model.playingMode == Escaping
                            in
                            if isLost then
                                Lost

                            else
                                model.status
                }


teleportPlayer : Array.Array { a : Position {}, b : Position {} } -> Character -> Character
teleportPlayer teleports character =
    Array.foldl
        (\teleport acc ->
            if acc.x == teleport.a.x && acc.y == teleport.a.y then
                { acc | x = teleport.b.x, y = teleport.b.y }

            else if acc.x == teleport.b.x && acc.y == teleport.b.y then
                { acc | x = teleport.a.x, y = teleport.a.y }

            else
                acc
        )
        character
        teleports


calculateNewGhostsPositions :
    { ghosts : Array.Array Character
    , playingMode : PlayingMode
    , shield : Bool
    , status : GameStatus
    , level : Level
    }
    -> Character
    -> Time.Posix
    -> { ghosts : Array.Array Character, isHit : Bool }
calculateNewGhostsPositions model newPlayer posix =
    Array.foldl
        (\index acc ->
            case Array.get index acc.ghosts of
                Just ghost ->
                    let
                        -- Using a temporary board with all characters on it so
                        -- that they cannot pass each other
                        tempLevelWithPlayerAndGhosts : Board
                        tempLevelWithPlayerAndGhosts =
                            addPlayerAndGhostsToLevel model.status acc.ghosts newPlayer model.level.board

                        ghostInNewPosition : Character
                        ghostInNewPosition =
                            ghostsMoveLogic model.shield posix tempLevelWithPlayerAndGhosts ghost

                        isHit : Bool
                        isHit =
                            -- If ghostInNewPosition is in the position of
                            -- player, we got an hit
                            levelGet ghostInNewPosition tempLevelWithPlayerAndGhosts
                                |> (\char -> Helpers.arrayMember char charsPlayer)
                    in
                    { ghosts =
                        Array.set index
                            (if model.playingMode /= Escaping && isHit then
                                { ghostInNewPosition
                                    | x = model.level.positionSpawnGhosts.x
                                    , y = model.level.positionSpawnGhosts.y
                                }

                             else
                                ghostInNewPosition
                            )
                            acc.ghosts
                    , isHit =
                        if isHit then
                            True

                        else
                            -- Once it is True, it remains True
                            acc.isHit
                    }

                Nothing ->
                    acc
        )
        { ghosts = model.ghosts, isHit = False }
        (Helpers.arrayRange 0 (Array.length model.ghosts))


isDot : String -> Bool
isDot char =
    Helpers.arrayMember char charsInDots


moveIfPossibleGhost : { isShield : Bool } -> Board -> Character -> Character
moveIfPossibleGhost isShield board character =
    let
        char : String
        char =
            levelGet (move character.direction character) board
    in
    if canGoGhost isShield char then
        moveCharacter character

    else
        character


moveIfPossiblePlayer : Board -> Character -> Character
moveIfPossiblePlayer board character =
    let
        char : String
        char =
            levelGet (move character.direction character) board
    in
    if canGoPlayer char then
        moveCharacter character

    else
        character


oppositeDirection : Direction -> Direction
oppositeDirection direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


ghostsMoveLogic : Bool -> Time.Posix -> Board -> Character -> Character
ghostsMoveLogic isShield posix board character =
    -- We chack 3 direction, left, ahead, right to see wich is available.
    -- If only ahead is available. We move ahead.
    -- If two or more directions are available, we decide "randomly" in which
    -- direction to go.
    -- If nono of these directions are available, we go back
    let
        avDirections : Array.Array Direction
        avDirections =
            ghostAvailableDirections isShield character board
    in
    if Array.length avDirections > 0 then
        -- We have a direction to go, let's go there,
        -- need to pick one random
        let
            index : Int
            index =
                Helpers.modBy
                    (Array.length avDirections)
                    (Time.posixToMillis posix)

            direction : Direction
            direction =
                avDirections
                    |> Array.get index
                    |> Maybe.withDefault Left
        in
        moveCharacter { character | direction = direction }

    else
        -- No places to go, let's go back
        moveIfPossibleGhost { isShield = isShield } board { character | direction = oppositeDirection character.direction }


ghostAvailableDirections : Bool -> Character -> Board -> Array.Array Direction
ghostAvailableDirections isShield character board =
    Array.foldl
        (\direction acc ->
            if direction == oppositeDirection character.direction then
                -- We ignore this case, otherwise character will randomly
                -- go back (do they?)
                acc

            else if canGoGhost { isShield = isShield } (levelGet (move direction character) board) then
                Helpers.arrayPushLast direction acc

            else
                acc
        )
        Array.empty
        directions


canGoGhost : { isShield : Bool } -> String -> Bool
canGoGhost { isShield } char =
    if isShield then
        -- Here cannot step on the player
        Helpers.arrayMember char tilesWhereGhostCanGoWithShield

    else
        -- Here can step on the player
        Helpers.arrayMember char tilesWhereGhostCanGoWithoutShield


canGoPlayer : String -> Bool
canGoPlayer char =
    Helpers.arrayMember char tilesWherePlayerCanGo


move : Direction -> Position a -> Position a
move direction character =
    case direction of
        Up ->
            { character | y = character.y - 1 }

        Down ->
            { character | y = character.y + 1 }

        Left ->
            { character | x = character.x - 1 }

        Right ->
            { character | x = character.x + 1 }


moveCharacter : Character -> Character
moveCharacter character =
    move character.direction character


mirrotHorizontally : Board -> Board
mirrotHorizontally board =
    board
        |> Array.map
            (\row ->
                row
                    ++ (row
                            |> String.reverse
                            |> String.dropLeft 1
                            |> flipBoxCorners
                       )
            )


levelGet : Position a -> Board -> String
levelGet position board =
    board
        |> (\array1 ->
                case Array.get position.y array1 of
                    Just row ->
                        let
                            rowAsArray : Array.Array String
                            rowAsArray =
                                row
                                    |> String.split ""
                                    |> Helpers.arrayFromList
                        in
                        Array.get (position.x * 2) rowAsArray

                    Nothing ->
                        Nothing
           )
        |> Maybe.withDefault "│"


boardSetDoubleWidth : String -> Position a -> Board -> Board
boardSetDoubleWidth =
    boardSetHelper (\i -> i * 2)


boardSetSingleWidth : String -> Position a -> Board -> Board
boardSetSingleWidth =
    boardSetHelper identity


boardSetHelper : (Int -> Int) -> String -> Position a -> Board -> Board
boardSetHelper func char position board =
    board
        |> (\array1 ->
                case Array.get position.y array1 of
                    Just row ->
                        let
                            newRow : String
                            newRow =
                                row
                                    |> String.split ""
                                    |> Helpers.arrayFromList
                                    |> (\array2 ->
                                            case Array.get (func position.x) array2 of
                                                Just _ ->
                                                    Array.set (func position.x) char array2

                                                Nothing ->
                                                    array2
                                       )
                                    |> Helpers.stringJoin ""
                        in
                        Array.set position.y newRow array1

                    Nothing ->
                        array1
           )


playerChar : Character -> String
playerChar character =
    if Helpers.modBy 2 (character.x + character.y) == 0 then
        playerShapes.full

    else
        case character.direction of
            Up ->
                playerShapes.up

            Down ->
                playerShapes.down

            Left ->
                playerShapes.left

            Right ->
                playerShapes.right


setCharacters : Board -> Array.Array Character -> Board
setCharacters board characters =
    Array.foldl
        (\character level_ ->
            if character.flavor == Player then
                boardSetDoubleWidth
                    (playerChar character)
                    { x = character.x
                    , y = character.y
                    }
                    level_

            else
                boardSetDoubleWidth
                    character.char
                    { x = character.x
                    , y = character.y
                    }
                    level_
        )
        board
        characters


addPlayerAndGhostsToLevel : GameStatus -> Array.Array Character -> Character -> Board -> Board
addPlayerAndGhostsToLevel status ghosts player board =
    ghosts
        |> (\array ->
                if status == Idle then
                    array

                else
                    Helpers.arrayPushLast player array
           )
        |> setCharacters board


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause || model.status == Quitting || model.status == Won || model.status == Lost then
        Sub.none

    else
        Time.every (toFloat (Helpers.round (1000 / toFloat model.fps))) Every


arrayFind : a -> Array.Array a -> Maybe Int
arrayFind predicate array =
    let
        findIndex : Int -> Maybe Int
        findIndex index =
            if index >= Array.length array then
                Nothing

            else
                case Array.get index array of
                    Nothing ->
                        Nothing

                    Just element ->
                        if predicate == element then
                            Just index

                        else
                            findIndex (index + 1)
    in
    findIndex 0


superimposeText : Array.Array String -> Position a -> Board -> Board
superimposeText strings position board =
    strings
        |> Helpers.arrayIndexedFoldl
            (\indexY row boardA ->
                row
                    |> String.split ""
                    |> Helpers.arrayFromList
                    |> Helpers.arrayIndexedFoldl
                        (\indexX char boardB ->
                            boardSetSingleWidth char { x = position.x + indexX, y = position.y + indexY } boardB
                        )
                        boardA
            )
            board


addOverlay : Bool -> GameStatus -> Array.Array String -> Level -> Board -> Board
addOverlay isPause status msg level_ board =
    if Array.length msg == 0 then
        board

    else
        let
            margin : Int
            margin =
                if status == Playing && not isPause then
                    0

                else
                    1

            position : Position {}
            position =
                { x = (level_.width - width) // 2
                , y = (level_.height - height) // 2
                }

            width : Int
            width =
                Array.foldl (\row acc -> max (String.length row) acc) 0 msg + (margin * 4)

            height : Int
            height =
                Array.length msg + margin

            headerFooter : String
            headerFooter =
                String.repeat width "`"

            addHederFooter : Array.Array String -> Array.Array String
            addHederFooter array =
                if margin == 1 then
                    array
                        |> Helpers.arrayPushLast headerFooter
                        |> Helpers.arrayPrepend (Helpers.arrayFromList [ headerFooter ])

                else
                    array

            msgFull : Array.Array String
            msgFull =
                (msg
                    |> Array.map
                        (\row ->
                            row
                                |> String.replace " " "`"
                                |> (\r ->
                                        String.repeat ((width - String.length r) // 2) "`"
                                            ++ r
                                            ++ String.repeat ((width - String.length r) // 2) "`"
                                   )
                                |> (\r ->
                                        if String.length r < width then
                                            r ++ "`"

                                        else
                                            r
                                   )
                        )
                )
                    |> addHederFooter
        in
        superimposeText msgFull position board


scoreCalculator : { a | initialDots : Int, counter : Int, points : Int, dots : Int } -> Int
scoreCalculator { initialDots, counter, points, dots } =
    (initialDots - dots) + (points * 10) - (counter // 10)


titleLength : Int
titleLength =
    String.length title


viewHeader : String -> Int -> String
viewHeader score width =
    score ++ String.repeat (width - String.length score - titleLength) " " ++ title


type alias Acc tile =
    { textTemp : String
    , previousTileType : String
    , tiles : Array.Array tile
    }


view :
    { charToTile : Int -> Tile -> String -> Acc tile -> Acc tile
    , tilesToRow : Int -> Array.Array tile -> row
    }
    -> Model
    -> Array.Array row
view funcs model =
    model.level.board
        |> addPlayerAndGhostsToLevel model.status model.ghosts model.player
        |> addOverlay model.pause model.status (overlayMain model) model.level
        |> addOverlay model.pause model.status (overlayPause model) model.level
        |> Helpers.arrayPrepend (Helpers.arrayFromList [ viewHeader (scoreText model) model.level.width ])
        |> Array.map (\row -> row ++ "\n")
        |> Array.indexedMap
            (\index row ->
                row
                    |> String.split ""
                    |> Helpers.arrayFromList
                    |> Array.foldl
                        (\char acc ->
                            let
                                tileType : Tile
                                tileType =
                                    charToTileType
                                        model.playingMode
                                        (model.status == Playing && not model.pause)
                                        model.shield
                                        char
                            in
                            funcs.charToTile
                                index
                                tileType
                                char
                                acc
                        )
                        { textTemp = ""
                        , previousTileType = ""
                        , tiles = Array.empty
                        }
                    |> (\resultFromFoldl -> funcs.tilesToRow index resultFromFoldl.tiles)
            )


charToTileType : PlayingMode -> Bool -> Bool -> String -> Tile
charToTileType playingMode isPlaying isShielded char =
    if Helpers.arrayMember char charsInLevel then
        if isPlaying then
            if isShielded then
                TileLevelWhileShield

            else
                TileLevelWhilePlaying

        else
            TileLevelWhileIdle

    else if Helpers.arrayMember char charsInDots then
        TileDot

    else if Helpers.arrayMember char charsPlayer then
        TilePlayer

    else if Helpers.arrayMember char charsNotVisible then
        TileNotVisible

    else if char == "`" then
        TileModalNotVisible

    else if char == "\n" then
        TileEndOfLine

    else if String.contains char "-~⍫!?0123456789ABCDEFGHIJKLMNOPQRSTUVWZXY[]↘↗↑←↓→.:" then
        TileModalVisible

    else
        case arrayFind char charGhosts of
            Just index ->
                if playingMode == Escaping then
                    TileGhostEscaping index

                else
                    TileGhostHunting

            Nothing ->
                TileNoOp


findCharInString : String -> String -> Array.Array Int
findCharInString char string =
    if String.contains char string then
        Helpers.arrayIndexedFoldl
            (\index charFromString acc ->
                if char == charFromString then
                    Helpers.arrayPushLast index acc

                else
                    acc
            )
            Array.empty
            (Helpers.stringSplit "" string)

    else
        Array.empty


findCharInBoard : String -> Board -> Array.Array (Position {})
findCharInBoard char board =
    Helpers.arrayIndexedFoldl
        (\indexY row acc ->
            Array.foldl
                (\indexX acc2 ->
                    Helpers.arrayPushLast { x = indexX // 2, y = indexY } acc2
                )
                acc
                (findCharInString char row)
        )
        Array.empty
        board


charsInDots : Array.Array String
charsInDots =
    Helpers.arrayFromList [ tiles.dotLarge, tiles.dotSmall ]


charsNotVisible : Array.Array String
charsNotVisible =
    Helpers.arrayFromList [ tiles.canGoButNoDot, tiles.onlyGhosts, tiles.onlyPlayer ]


charsPlayer : Array.Array String
charsPlayer =
    Helpers.arrayFromList [ playerShapes.full, playerShapes.up, playerShapes.down, playerShapes.left, playerShapes.right ]


tilesWhereGhostCanGoWithShield : Array.Array String
tilesWhereGhostCanGoWithShield =
    Helpers.arrayFromList [ tiles.dotSmall, tiles.dotLarge, tiles.onlyGhosts, tiles.canGoButNoDot ]


tilesWhereGhostCanGoWithoutShield : Array.Array String
tilesWhereGhostCanGoWithoutShield =
    -- When no shield, ghost can go over player
    Helpers.arrayPrepend tilesWhereGhostCanGoWithShield charsPlayer


tilesWherePlayerCanGo : Array.Array String
tilesWherePlayerCanGo =
    Helpers.arrayFromList [ tiles.dotSmall, tiles.dotLarge, tiles.canGoButNoDot, tiles.onlyPlayer ]


boolToOnOff : Bool -> String
boolToOnOff bool =
    if bool then
        "ON"

    else
        "OFF"


debugText : Model -> Array.Array String
debugText model =
    let
        key : String
        key =
            if model.keyDown == " " then
                "Space"

            else
                model.keyDown

        status : String
        status =
            case model.status of
                Playing ->
                    "Playing"

                Idle ->
                    "Idle"

                Quitting ->
                    "Quitting"

                Won ->
                    "Won"

                Lost ->
                    "Lost"

                Menu ->
                    "Menu"

        modeToString : PlayingMode -> String
        modeToString mode =
            case mode of
                HuntingGhosts { since } ->
                    "Hunting " ++ String.fromInt since

                Escaping ->
                    "Escaping"
    in
    [ "Frames " ++ String.fromInt model.counter
    , "Points " ++ String.fromInt model.points
    , "  Dots " ++ String.fromInt model.dots
    , "   Key " ++ key
    , "Ghosts " ++ String.fromInt (Array.length model.ghosts)
    , "   FPS " ++ String.fromInt model.fps
    , " Pause " ++ boolToOnOff model.pause
    , "Status " ++ status
    , "Shield " ++ boolToOnOff model.shield
    , "  Mode " ++ modeToString model.playingMode
    , "Player " ++ String.fromInt model.player.x ++ "," ++ String.fromInt model.player.y
    ]
        |> Helpers.arrayFromList
        |> Array.map (\s -> "   " ++ String.toUpper s)
        |> Helpers.arrayPrepend (Helpers.arrayFromList [ "" ])
        |> Helpers.arrayPrepend (keysMenu model)
        |> Helpers.arrayPrepend (Helpers.arrayFromList [ "", "", "" ])
        |> Array.map (\s -> " " ++ String.toUpper s)


scoreText : { a | counter : Int, dots : Int, initialDots : Int, points : Int } -> String
scoreText model =
    let
        score : Int
        score =
            scoreCalculator model
    in
    if score <= 0 then
        ""

    else
        " SCORE " ++ String.fromInt (scoreCalculator model) ++ " "


overlayMain : Model -> Array.Array String
overlayMain model =
    let
        msgEnterToPlay : Array.Array String
        msgEnterToPlay =
            Helpers.arrayFromList
                [ "     PLAY [ENTER]"
                , "FAST PLAY [F]    "
                , "     QUIT [Q]    "
                , "     MENU [M]    "
                ]
    in
    case model.status of
        Idle ->
            msgEnterToPlay

        Quitting ->
            Helpers.arrayFromList [ "QUIT THE GAME?", "", "[Y]ES  [N]O" ]

        Playing ->
            Array.empty

        Won ->
            Helpers.arrayPrepend (Helpers.arrayFromList [ "LEVEL COMPLETED!", "", String.replace " " "`" (scoreText model), "" ]) msgEnterToPlay

        Lost ->
            Helpers.arrayPrepend (Helpers.arrayFromList [ "YOU LOST!", "", String.replace " " "`" (scoreText model), "" ]) msgEnterToPlay

        Menu ->
            keysMenu model


overlayPause : { a | pause : Bool } -> Array.Array String
overlayPause model =
    if model.pause then
        Helpers.arrayFromList [ "PAUSED", "", "  ANY KEY TO RESUME  " ]

    else
        Helpers.arrayFromList []


title : String
title =
    "github.com/lucamug/functional-pacman "


keysMenu : { a | isRunningInTerminal : Bool } -> Array.Array String
keysMenu { isRunningInTerminal } =
    Helpers.arrayFromList
        ([ "     PLAY [ENTER]  "
         , "FAST PLAY [F]      "
         , "     MOVE [W A S D]"
         , "          [↑ ← ↓ →]"
         , "    PAUSE [SPACE]  "
         , "     MENU [M]      "
         , "     QUIT [Q]      "
         , "      FPS [Y]↘ [U]↗"
         , "   GHOSTS [I]↘ [O]↗"
         , "    DEBUG [B]      "
         , "   SHIELD [L]      "
         , "   LEVELS [1]~[" ++ String.fromInt (Array.length levels) ++ "]  "
         ]
            ++ (if isRunningInTerminal then
                    [ "     EXIT [ESC]    " ]

                else
                    []
               )
        )


playerShapes : { down : String, full : String, left : String, right : String, up : String }
playerShapes =
    { full = "☻", down = "∩", up = "u", left = "⊃", right = "c" }


charsInLevel : Array.Array String
charsInLevel =
    Helpers.arrayFromList [ "╰", "╮", "╭", "╯", "─", "│" ]


flipBoxCorners : String -> String
flipBoxCorners string =
    -- Change "╮" to "╭", "╭" to "╮", etc.
    string
        |> String.replace "╮" "‡"
        |> String.replace "╭" "╮"
        |> String.replace "‡" "╭"
        |> String.replace "╯" "‡"
        |> String.replace "╰" "╯"
        |> String.replace "‡" "╰"


charGhosts : Array.Array String
charGhosts =
    Helpers.arrayFromList [ "█", "▉", "▓", "▒" ]


tiles : { dotSmall : String, dotLarge : String, onlyPlayer : String, onlyGhosts : String, canGoButNoDot : String, spawnPlayerLeft : String, spawnPlayerUp : String, spawnPlayerRight : String, spawnPlayerDown : String, spawnGhosts : String, teleportUpDown : String, teleportLeftRight : String }
tiles =
    { dotSmall = "•"
    , dotLarge = "●"
    , onlyPlayer = "*"
    , onlyGhosts = "="
    , canGoButNoDot = "+"
    , spawnPlayerLeft = "←"
    , spawnPlayerUp = "↑"
    , spawnPlayerRight = "→"
    , spawnPlayerDown = "↓"
    , spawnGhosts = "†"
    , teleportUpDown = "↕"
    , teleportLeftRight = "↔"
    }


board0 : Board
board0 =
    Helpers.arrayFromList
        [ "╭─────────────────────────╮ "
        , "│ • • • • • • • • • • • • │ "
        , "│ • ╭─────╮ • ╭───────╮ • │ "
        , "│ ● │     │ • │       │ • │ "
        , "│ • ╰─────╯ • ╰───────╯ • ╰─"
        , "│ • • • • • • • • • • • • • "
        , "│ • ╭─────╮ • ╭─╮ • ╭───────"
        , "│ • ╰─────╯ • │ │ • ╰─────╮ "
        , "│ • • • • • • │ │ • • • • │ "
        , "╰─────────╮ • │ ╰─────╮ + │ "
        , "          │ • │ ╭─────╯ + ╰─"
        , "          │ • │ │ + + + + + "
        , "          │ • │ │ + ╭───=   "
        , "──────────╯ • ╰─╯ + │ = =   "
        , "↔ * * * * * • + + + │ =     "
        , "──────────╮ • ╭─╮ + │ = = † "
        , "          │ • │ │ + ╰───────"
        , "          │ • │ │ + + + + + "
        , "          │ • │ │ + ╭───────"
        , "╭─────────╯ • ╰─╯ + ╰─────╮ "
        , "│ • • • • • • • • • • • • │ "
        , "│ • ╭─────╮ • ╭───────╮ • │ "
        , "│ • ╰───╮ │ • ╰───────╯ • ╰─"
        , "│ ● • • │ │ • • • • • • • → "
        , "╰───╮ • │ │ • ╭─╮ • ╭───────"
        , "╭───╯ • ╰─╯ • │ │ • ╰─────╮ "
        , "│ • • • • • • │ │ • • • • │ "
        , "│ • ╭─────────╯ ╰─────╮ • │ "
        , "│ • ╰─────────────────╯ • ╰─"
        , "│ • • • • • • • • • • • • • "
        , "╰───────────────────────────"
        ]
        |> mirrotHorizontally


board1 : Board
board1 =
    Helpers.arrayFromList
        [ "──────────────────────────────────────────────────────────╮"
        , "↔ → • • • • • • • • • • • • • • • • • • • • • • • • • • • │"
        , "╭─────────────────────────────────────────────────────╮ • │"
        , "│ • • • • • • • • • • • • • • • ● • • • • • • • • • • │ • │"
        , "│ • ╭─────────────────────────────────────────────╮ • │ • │"
        , "│ • │ • • • • • • • • • • • • • • • • • • • • • • │ • │ • │"
        , "│ • │ • ╭─────────────────────────────────────╮ • │ • │ • │"
        , "│ • │ • │ • • • • • • • • • • • ● • • • • • • │ • │ • │ • │"
        , "│ • │ • │ • ╭─────────────────────────────╮ • │ • │ • │ • │"
        , "│ • │ • │ • │ • • • • • • • • • • • • • • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • ╭─────────────────────╮ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • • • • • • • • • • │ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • ╭─────────────╮ • │ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • │ • • • • • • │ • │ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • │ • ╭─────╮ • │ • │ • │ • │ • │ • │ • │"
        , "│ • │ † │ • │ • │ • │ • │ • ↔ │ • │ • │ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • │ • │ • ──╯ • │ • │ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • │ • │ • • • • │ • │ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • │ • ╰─────────╯ • │ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • │ • • • • • • • • │ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • ╰─────────────────╯ • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • │ • • • • • • • • • • • • │ • │ • │ • │ • │"
        , "│ • │ • │ • │ • ╰─────────────────────────╯ • │ • │ • │ • │"
        , "│ • │ • │ • │ • • • • • • • • • • • • • • • • │ • │ • │ • │"
        , "│ • │ • │ • ╰─────────────────────────────────╯ • │ • │ • │"
        , "│ • │ • │ • • • • • • • • • • • • • • • • • • • • │ • │ • │"
        , "│ • │ • ╰─────────────────────────────────────────╯ • │ • │"
        , "│ • │ • • • • • • • • • • • • • • • • • • • • • • • • │ • │"
        , "│ • ╰─────────────────────────────────────────────────╯ • │"
        , "│ • • • • • • • • • • • • • • • • • • • • • • • • • • • • │"
        , "╰─────────────────────────────────────────────────────────╯"
        ]


board2 : Board
board2 =
    Helpers.arrayFromList
        [ "╭───────────────────────────────╮ ↕ ╭─────────╮"
        , "│                               │ • │         │"
        , "╰───────────────────────────────╯ • │         │"
        , "↔ • • • • • • • • • • • • • • • • • │         │"
        , "╭─────────╮ • ╭─────╮ • ╭───────╮ • │         │"
        , "│         │ ● │     │ • │       │ • │         │"
        , "│         │ • ╰─────╯ • ╰───────╯ • │         │"
        , "│         │ • • • • • • • • • • • ↑ │         │"
        , "│         │ • ╭─────╮ • ╭─╮ • ╭─────╯         │"
        , "│         │ • │     │ • │ │ • │               │"
        , "│         │ • ╰─────╯ • │ │ • ╰───────────────╯"
        , "│         │ • • • • • • │ │ • • • • • • • • • ↔"
        , "│         │ • ╭─────╮ = │ ╰───────────────────╮"
        , "│         │ • │     │ = ╰───╮                 │"
        , "│         │ • │     │ = = † │                 │"
        , "│         │ • │     ╰───────╯                 │"
        , "╰─────────╯ ↕ ╰───────────────────────────────╯"
        ]


board3 : Board
board3 =
    Helpers.arrayFromList
        [ "╭─────────────────────────────────────────────────────────────────╮"
        , "│ • • • • • • • • • ● • • • • • • • • • • • • • • • • • • • • • • │"
        , "│ • ╭─────────────╮ • ╭───────────────────────────────────────╮ • │"
        , "│ • │ ╭───────────╯ • ╰─────────────────────────────────────╮ │ • │"
        , "│ • │ │╭╮╭╮╭╮╭╮╭╮╭╮ • ╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮│ │ • │"
        , "│ • │ ╰╯╰╯╰╯╰╯╰╯╰╯│ • │╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯ │ • │"
        , "│ • │ ╭─────────╮ │ • ╰───────────────────────────────────────╯ • │"
        , "│ • ╰─╯ ╭───────╯ │ • • • • • • • • • • • • • • • • • • • • • • • │"
        , "│ • = † │╭╮╭╮╭╮╭╮╭╯ • ╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭─╮ ↓ │"
        , "│ • = = ╰╯╰╯╰╯╰╯╰╯  • │╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯ │ • │"
        , "│ • ╭─────────────╮ • ╰───────────────────────────────────────╯ • │"
        , "│ • │ ╭───────────╯ • • • • • • • • • • • • • • • • • • • • • • • │"
        , "│ • │ │╭╮╭╮╭╮╭╮╭╮╭╮ • ╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭─╮ • │"
        , "│ • │ ╰╯╰╯╰╯╰╯╰╯╰╯│ • │╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯ │ • │"
        , "│ • │ ╭╮╭╮╭╮╭╮╭╮╭╮│ • │╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮ │ • │"
        , "│ • ╰─╯╰╯╰╯╰╯╰╯╰╯╰╯ • ╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰─╯ • │"
        , "│ • • • • • • • • • • • • • • • • • • • • • • • • • • • • • ● • • │"
        , "╰─────────────────────────────────────────────────────────────────╯"
        ]
