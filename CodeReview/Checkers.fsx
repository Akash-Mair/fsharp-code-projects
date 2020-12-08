
open System

type Position = int * int 
type Movement = Capture | Move | Blocked 
type Piece = King | Normal

type Checker = Piece * Movement

//type PlayerChecker = Player1Checker of Checker | Player2Checker of Checker 
type Player = Player1 | Player2

type SquareState = 
    | Player1Checker of Checker
    | Player2Checker of Checker 
    | Empty 

type Color = Black | White | Red
type Square = Color * Position * SquareState

type Board = { Squares: Square list }

//type Errors =
//    | MovingOtherPlayersCheckerError of string
//    | BlockedCheckerError of string
//    | UnvalidMoveError of string
//    
type GameStatus =
    | Player1Wins
    | Player2Wins
    | InProgress

type GameResult =
    | SuccessfulMove of Board * Player
    | Error of string 

let isEven x = x % 2 = 0

let (=>) x fn = x |> List.map fn 

let emptyBoard =
   { Squares =
        ([1..8], [1..8])
        ||> List.allPairs
        |> List.sortBy snd
        => Position
        => fun x -> (x, Empty)
        => fun s -> let (pos, state) = s
                    let (x,y) = pos  
                    match y with
                    | z when y |> isEven -> if not (isEven x) then (White, pos, state) else (Black, pos, state)
                    | w when (isEven y) |> not -> if x |> isEven then (White, pos, state) else (Black, pos, state)
            
   }

let placePiece player square: Square =
    let (color, pos, state) = square
    match player with
    | Player1 ->
        match color with
        | Black when (pos |> snd |> (=) 1) ->  (color, pos, (Normal, Blocked) |> Player1Checker )
        | Black when (pos |> snd |> (=) 2) ->  (color, pos, (Normal, Blocked) |> Player1Checker )
        | Black when (pos |> snd |> (=) 3) ->  (color, pos, (Normal, Move) |> Player1Checker )
    
        | _ -> (color, pos, state)

    | Player2 ->
        match color with
        | Black when (pos |> snd |> (=) 6) ->  (color, pos, (Normal, Move) |> Player2Checker )
        | Black when (pos |> snd |> (=) 7) ->  (color, pos, (Normal, Blocked) |> Player2Checker )
        | Black when (pos |> snd |> (=) 8) ->  (color, pos, (Normal, Blocked) |> Player2Checker )
        | _ -> (color, pos, state)

let setup emptyBoard =
    { Squares = emptyBoard.Squares 
                    => placePiece Player1
                    => placePiece Player2
    }

let  newGame (): Board = emptyBoard |> setup

let forwardPositionsP1 (x,y) =
    match x with
    | 1 -> [(x+1,y+1)] 
    | 8 -> [(x-1,y+1)] 
    | _ -> [(x-1,y+1); (x+1,y+1)]
    
let backwardPositionsP1 (x,y) =
    match x with
    | 1 -> [(x+1,y-1)] 
    | 8 -> [(x-1,y-1)] 
    | _ -> [(x-1,y-1); (x+1,y-1)] 
        
let forwardPositionsP2 = backwardPositionsP1
let backwardPositionsP2 = forwardPositionsP1

let checkerMoveSetP1 pos = pos |> forwardPositionsP1

let checkerMoveSetP2 pos = pos |> forwardPositionsP2

let kingMoveSetP1 pos = pos
                      |> forwardPositionsP1
                      |> (@) (pos |> backwardPositionsP1)
    
let kingMoveSetP2 pos = pos
                      |> forwardPositionsP2
                      |> (@) (pos |> backwardPositionsP2)
                      
let squareFromPosition board pos =
    board.Squares
    |> List.find (fun x -> let (_,position,_) = x
                           position = pos)

    
let forwardJumpP1 (x,y) (z,w) =
    match z with
    | a when x+1 = z -> (z+1, w+1)
    | b when x-1 = z -> (z-1, w+1)

let backwardJumpP1 (x,y) (z,w) =
    match z with
    | a when x-1 = z -> (z-1,w-1)
    | b when x+1 = z -> (z+1,w-1)

let forwardJumpP2 = backwardJumpP1

let backwardJumpP2 = forwardJumpP1

let emptySquare:(Square -> bool) =
    function
    | (_,_,Empty) -> true
    | _ -> false
    
let checkForOpponentsCheckerForwardAdjacentP1 pos board square =
    let (_,posOfForwardSquare, state) = square
    match state with
    | Player2Checker checker ->
        forwardJumpP1 pos posOfForwardSquare
        |> squareFromPosition board |> Some 
    | _ -> None

let checkForOpponentsCheckerBackwardAdjacentP1 pos board square =
    let (_,posOfForwardSquare, state) = square
    match state with
    | Player2Checker checker ->
        backwardJumpP1 pos posOfForwardSquare
        |> squareFromPosition board |> Some 
    | _ -> None

let foundOpponentCheckersAdjacent list = list |> List.choose id 

let adjacentSquares board moveSet pos =
    pos 
    |> moveSet
    => squareFromPosition board

let jumpIfValid (pos:Position) (state:SquareState) list: Square option =
    list |> List.filter emptySquare
         |> List.length
         |> function
            | 0 -> None 
            | _ -> Some (Black, pos, state)

let (|CanJump|_|) board (square: Square) =
    let (_, pos, state) = square
    match state with
    | Player1Checker checker ->
        match checker with
        | (Normal,_) -> pos
                        |> adjacentSquares board checkerMoveSetP1
                        => checkForOpponentsCheckerForwardAdjacentP1 pos board 
                        |> foundOpponentCheckersAdjacent
                        |> jumpIfValid pos (Player1Checker (Normal, Capture))                     
        | (King,_) -> pos
                       |> adjacentSquares board checkerMoveSetP1
                       => checkForOpponentsCheckerForwardAdjacentP1 pos board 
                       |> (@)  (pos
                               |> adjacentSquares board backwardPositionsP2
                               => checkForOpponentsCheckerBackwardAdjacentP1 pos board)
                       |> foundOpponentCheckersAdjacent
                       |> jumpIfValid pos (Player1Checker (King, Capture))
    | Player2Checker checker ->
        match checker with
        | (Normal,_) -> pos
                        |> checkerMoveSetP2
                        => squareFromPosition board
                        => fun x -> let (_,posOfForwardSquare, state) = x
                                    match state with
                                    | Player1Checker checker ->
                                        forwardJumpP2 pos posOfForwardSquare
                                        |> squareFromPosition board |> Some 
                                    | _ -> None 
                        |> List.choose id
                        |> List.filter emptySquare
                        |> List.length > 0 
                        |> function
                           | true -> Some (Black, pos, (Normal, Capture) |> Player2Checker)
                           | false -> None 
        | (King,_) -> pos
                       |> checkerMoveSetP2
                       => squareFromPosition board
                       => fun x -> let (_,posOfForwardSquare, state) = x
                                   match state with
                                    | Player1Checker checker ->
                                        forwardJumpP2 pos posOfForwardSquare
                                        |> squareFromPosition board |> Some 
                                    | _ -> None 
                       |> (@)  (pos |> backwardPositionsP2
                               => squareFromPosition board
                               => fun x -> let (_,posOfForwardSquare, state) = x
                                           match state with
                                           | Player1Checker checker ->
                                               backwardJumpP2 pos posOfForwardSquare
                                               |> squareFromPosition board |> Some 
                                               | _ -> None 
                                               )
                       |> List.choose id
                       |> List.filter emptySquare
                       |> List.length > 0 
                       |> function
                           | true -> Some (Black, pos, (Normal, Capture) |> Player2Checker)
                           | false -> None    
        
    | Empty -> None 
    
let (|CanMove|_|) board (square:Square) =
        let (color, pos, state) = square
        match state with
        | Player1Checker checker ->
            match checker with
            | (Normal, _) -> pos
                             |> checkerMoveSetP1
                             => squareFromPosition board
                             |> List.filter emptySquare
                             |> List.length > 0
                             |> function
                                | true -> Some (color, pos, (Normal, Move) |> Player1Checker)
                                | false -> None
            | (King, _) -> pos
                           |> kingMoveSetP1
                           => squareFromPosition board
                           |> List.filter emptySquare
                           |> List.length > 0
                             |> function
                                | true -> Some (color, pos, (King, Move)|> Player1Checker)
                                | false -> None
        | Player2Checker checker ->
            match checker with
            | (Normal, _) -> pos
                             |> checkerMoveSetP2
                             => squareFromPosition board
                             |> List.filter emptySquare
                             |> List.length > 0
                             |> function
                                | true -> Some (color, pos, (Normal, Move) |> Player2Checker)
                                | false -> None
            | (King, _) -> pos
                           |> kingMoveSetP2
                           => squareFromPosition board
                           |> List.filter emptySquare
                           |> List.length > 0
                             |> function
                                | true -> Some (color, pos, (King, Move) |> Player2Checker)
                                | false -> None
        | _ -> None
        
        
let (|IsBlocked|_|) board (square:Square) =
        let (color, pos, state) = square
        match state with
        | Player1Checker checker ->
            match checker with
            | (Normal, _) -> pos
                             |> checkerMoveSetP1
                             => squareFromPosition board
                             |> List.filter emptySquare
                             |> List.length = 0
                             |> function
                                | true -> Some (color, pos, (Normal, Blocked)|> Player1Checker)
                                | false -> None
            | (King, _) -> pos
                           |> kingMoveSetP1
                           => squareFromPosition board
                           |> List.filter emptySquare
                           |> List.length = 0
                             |> function
                                | true -> Some (color, pos, (King, Blocked) |> Player1Checker)
                                | false -> None
        | Player2Checker checker ->
            match checker with
            | (Normal, _) -> pos
                             |> checkerMoveSetP2
                             => squareFromPosition board
                             |> List.filter emptySquare
                             |> List.length = 0
                             |> function
                                | true -> Some (color, pos, (Normal, Blocked) |> Player2Checker)
                                | false -> None
            | (King, _) -> pos
                           |> kingMoveSetP2
                           => squareFromPosition board
                           |> List.filter emptySquare
                           |> List.length = 0
                             |> function
                                | true -> Some (color, pos, (King, Blocked) |> Player2Checker)
                                | false -> None
        | _ -> None
 
let calculateMovementForChecker board (square:Square) =
    match square with
    | CanJump board square -> square
    | CanMove board square -> square
    | IsBlocked board square -> square
    | _ -> square 
    
        
let calculateStateForCheckersOnBoard board =
    { Squares =
        board.Squares
        => calculateMovementForChecker board
    }


let moveChecker (board:Board) (selectedSqaure: Square) (desiredPosition: Position) =
    let (color, pos, state) = selectedSqaure
    { Squares =
        board.Squares
        => fun s -> let (currentColor, currentPos, currentState) = s
                    match s with
                    | moveTo when (currentPos = desiredPosition) -> (currentColor, desiredPosition, state) 
                    | on when s = selectedSqaure -> (color, pos, Empty)
                    | _ -> s      
    } |> calculateStateForCheckersOnBoard

let capturedCheckerPosititon (x,y) (v,w) =
    match (x,y) with
    | a when (x+2 = v) && (y+2 = w) -> (x+1, y+1)
    | b when (x-2 = v) && (y+2 = w) -> (x-1, y+1)
    | c when (x+2 = v) && (y-2 = w) -> (x+1, y-1)
    | d when (x-2 = v) && (y-2 = w) -> (x-1, y-1)
  
let takeChecker (board:Board) (selectedSqaure: Square) (desiredPosition: Position) =
    let (_,pos, state) = selectedSqaure
    let takenPos = capturedCheckerPosititon pos desiredPosition 
    let takenChecker = takenPos |> squareFromPosition board               
    let updatedBoard =  calculateStateForCheckersOnBoard {
        Squares =
            board.Squares
            => fun x -> let (currentColor, currentPos, currentState) = x
                        match x with 
                        | taken when x = takenChecker  -> (Black, takenPos, Empty)
                        | on when currentPos = pos -> (Black, pos, Empty)
                        | moveTo when (currentPos =  desiredPosition) -> (currentColor, desiredPosition, state) 
                        | _ -> x
        } 
    
    let jumpedPosition = desiredPosition
    let jumpedSquare = jumpedPosition |> squareFromPosition updatedBoard
    
    match jumpedSquare with
    | (_, __, state) ->
        match state with
        | Player1Checker checker ->
            match checker with
            | (_,Capture) -> (updatedBoard, Player1)
            | _ -> (updatedBoard, Player2)
        | Player2Checker checker ->
            match checker with
            | (_,Capture) -> (updatedBoard, Player2)
            | _ -> (updatedBoard, Player1)    
    
    
    // if he can capture again then you need to run checkersThatCanCapture fn and recurse this fn 
    
let checkerThatP1HasToPlay square =
    let (_,_, state) = square
    match state with
    Player1Checker checker ->
        match checker with
        | (_,Capture) -> true
        | _ -> false
    | _ -> false

let checkerThatP2HasToPlay square =
    let (_,_, state) = square
    match state with
    Player2Checker checker ->
        match checker with
        | (_,Capture) -> true
        | _ -> false
    | _ -> false

let checkersThatCanCapture (board,player) = //Middleware to run before moves
   (
       board, player, 
       match player with 
       | Player1 -> board.Squares
                    |> List.filter checkerThatP1HasToPlay
       | Player2 -> board.Squares
                    |> List.filter checkerThatP2HasToPlay
   )

let move checkersWithCapture (selectedSqaure: Square) (desiredPosition: Position) =
    match checkersWithCapture with
    | (board, player, []) ->
        match player with
        | Player1 ->
            match selectedSqaure with
            | (_, pos, Player1Checker checker) ->
                match checker with
                | (Normal, Move) ->  pos
                                     |> checkerMoveSetP1
                                     |> List.contains desiredPosition
                                     |> function
                                        | true -> (moveChecker board selectedSqaure desiredPosition, Player2) |> SuccessfulMove
                                        | false -> "Unvalid Move" |> Error
                | (Normal, Blocked) -> "Blocked Checker" |> Error
                | (King, Move) -> pos
                                  |> kingMoveSetP1
                                  |> List.contains desiredPosition
                                  |> function
                                     | true -> (moveChecker board selectedSqaure desiredPosition, Player2) |> SuccessfulMove
                                     | false -> "Unvalid Move"  |> Error
                | (King, Blocked) -> "Blocked Checker" |> Error
            | _ -> "MovingOtherPlayersCheckerError" |> Error
        | Player2 ->
            match selectedSqaure with
            | (_, pos, Player2Checker checker) ->
                match checker with
                | (Normal, Move) ->  pos
                                     |> checkerMoveSetP2
                                     |> List.contains desiredPosition
                                     |> function
                                        | true -> (moveChecker board selectedSqaure desiredPosition, Player1) |> SuccessfulMove
                                        | false -> "UnvalidMoveError" |> Error
                | (Normal, Blocked) -> "BlockedCheckerError" |> Error
                | (King, Move) -> pos
                                  |> kingMoveSetP2
                                  |> List.contains desiredPosition
                                  |> function
                                     | true -> (moveChecker board selectedSqaure desiredPosition, Player1) |> SuccessfulMove
                                     | false ->  "UnvalidMoveError" |> Error
                | (King, Blocked) -> "BlockedCheckerError" |> Error
            | _ -> "MovingOtherPlayersCheckerError" |> Error
                      
    | (board, player, captureCheckers) ->
        match player with
        | Player1 -> captureCheckers
                     |> List.contains selectedSqaure
                     |> function
                        | true -> let (color, pos, state) = selectedSqaure
                                  match state with
                                  | Player1Checker checker ->
                                      match checker with
                                      | (Normal, Capture) -> pos
                                                             |> checkerMoveSetP1
                                                             => squareFromPosition board
                                                             => fun x -> let (_,posOfForwardSquare, state) = x
                                                                         match state with
                                                                         | Player2Checker checker ->
                                                                            forwardJumpP1 pos posOfForwardSquare
                                                                            |> squareFromPosition board |> Some 
                                                                         | _ -> None 
                                                             |> List.choose id
                                                             |> List.filter emptySquare
                                                             |> List.contains (Black, desiredPosition, Empty)
                                                             |> function
                                                                | true -> takeChecker board selectedSqaure desiredPosition |> SuccessfulMove
                                                                | false -> "UnvalidMoveError "|> Error
                                      | (King, Capture) ->   pos
                                                             |> kingMoveSetP1
                                                             => squareFromPosition board
                                                              => fun x -> let (_,posOfForwardSquare, state) = x
                                                                          match state with
                                                                          | Player2Checker checker ->
                                                                              forwardJumpP1 pos posOfForwardSquare
                                                                              |> squareFromPosition board |> Some 
                                                                          | _ -> None 
                                                              |> (@)  (pos |> backwardPositionsP2
                                                                       => squareFromPosition board
                                                                       => fun x -> let (_,posOfForwardSquare, state) = x
                                                                                   match state with
                                                                                   | Player2Checker checker ->
                                                                                       backwardJumpP1 pos posOfForwardSquare
                                                                                       |> squareFromPosition board |> Some 
                                                                                       | _ -> None 
                                                                                       )
                                                             |> List.choose id
                                                             |> List.filter emptySquare
                                                             |> List.contains (Black, desiredPosition, Empty)
                                                             |> function
                                                                | true -> takeChecker board selectedSqaure desiredPosition |> SuccessfulMove
                                                                | false -> "UnvalidMoveError" |> Error
                                  | _ -> "MovingOtherPlayersCheckerError" |> Error              
                        | _ -> "UnvalidMoveError" |> Error
        | Player2 -> captureCheckers
                     |> List.contains selectedSqaure
                     |> function
                        | true -> let (color, pos, state) = selectedSqaure
                                  match state with
                                  | Player2Checker checker ->
                                      match checker with
                                      | (Normal, Capture) -> pos
                                                             |> checkerMoveSetP2
                                                             => squareFromPosition board
                                                             => fun x -> let (_,posOfForwardSquare, state) = x
                                                                         match state with
                                                                         | Player1Checker checker ->
                                                                            forwardJumpP2 pos posOfForwardSquare
                                                                            |> squareFromPosition board |> Some 
                                                                         | _ -> None 
                                                             |> List.choose id
                                                             |> List.filter emptySquare
                                                             |> List.contains (Black, desiredPosition, Empty)
                                                             |> function
                                                                | true -> takeChecker board selectedSqaure desiredPosition |> SuccessfulMove
                                                                | false -> "UnvalidMoveError" |> Error
                                      | (King, Capture) ->   pos
                                                             |> kingMoveSetP2
                                                             => squareFromPosition board
                                                              => fun x -> let (_,posOfForwardSquare, state) = x
                                                                          match state with
                                                                          | Player1Checker checker ->
                                                                              forwardJumpP1 pos posOfForwardSquare
                                                                              |> squareFromPosition board |> Some 
                                                                          | _ -> None 
                                                              |> (@)  (pos |> backwardPositionsP2
                                                                       => squareFromPosition board
                                                                       => fun x -> let (_,posOfForwardSquare, state) = x
                                                                                   match state with
                                                                                   | Player1Checker checker ->
                                                                                       backwardJumpP1 pos posOfForwardSquare
                                                                                       |> squareFromPosition board |> Some 
                                                                                       | _ -> None 
                                                                                       )
                                                             |> List.choose id
                                                             |> List.filter emptySquare
                                                             |> List.contains (Black, desiredPosition, Empty)
                                                             |> function
                                                                | true -> takeChecker board selectedSqaure desiredPosition |> SuccessfulMove
                                                                | false -> "UnvalidMoveError" |> Error
                                  | _ -> "MovingOtherPlayersCheckerError" |> Error          
                        | _ -> "Unvalid move" |> Error

let bind switchfn twoTrackInput =
    match twoTrackInput with
    | SuccessfulMove (a,b) -> switchfn (a,b)
    //| Error f -> Failure f

let (>>=) twoTrackInput switchfn = bind switchfn twoTrackInput                      
                      
let game = newGame () 