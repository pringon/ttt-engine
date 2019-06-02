# Engine Planning

## Principles

- Try to win as fast as possible.
- If winning is impossible try to draw as *fast* as possible.
- If drawing is impossible try to lose as *slow* as possible.

## Strategy

- Get a board state:
  - Compute next board state:
    - Get all possible moves.
    - Take all possible moves.
    - Continue until you reach a terminal state (Win, Draw,, Loss).
  - Find best move from list of possible moves (by score).
- Score:
  - Win: 100 points.
  - Draw: 0 points.
  - Loss: -100 points.
  - Each player turn decrements score by one.

## Types

### Square 

- State of a square.
- Available values are X, O and Empty.

## Board

- A 3x3 matrix of Squares.
- Contains a possible game state.

## State

- The current state of the game
- Available values are Win, Loss, Draw, Unfinished.

## Move

- A move that can be taken.
- Equivalent to a tupple containing two integers.

## Color

- Determines which player should act next.
- Available values are 1 (engine) and -1 (human).

## Functions

### getMoves

- params:
  - current board state (Board).
- returns:
  - a list of empty squares in the board ([(Move)])

### takeMove

- params:
  - current board state (Board).
  - move to be taken ((Int, Int)).
- returns:
  - new board state after the move was taken (Board).

### getState

- params:
  - current board state (Board).
-returns:
  - the state of the game (State).

### findBestMove

- params:
  - current board state (Board).
  - the current player to act (Color).
- returns:
  - a tuple containing the best move and the score associated with it ((Move, Int))
