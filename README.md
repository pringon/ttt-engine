# A tic tac toe engine.

Uses the negamax algorithm to evaluate possible moves and choose the most optimal one given the current board state.

## Requirements

- You will need to have stack installed: https://docs.haskellstack.org/en/stable/README/

## Usage

- ```stack setup```
  - Downloads the compiler in an isolated location, if you haven't already done so.
- ```stack test```
  - Will run the test suite
- ```stack run```
  - Runs service.

- In order to get a move suggestion from the api you have to feed it a 2d string matrix containing combinations of the following:
  - 'X' - Symbol for the player.
  - 'O' - Symbol for the engine.
  - '' - Represents an empty square.
