# The Game of Life
## Project description
We develop Convay's Game of Life by using Haskell programming language.

## Implementation
The game have to be implemented in two variants: basic variant with classic rules and HashLife variant optimised with memoized algorythm.

## GUI
I suppose the game will have console interface (probably by using [vty haskell module](https://github.com/jtdaugherty/vty))

## Output
The output data have to be represented with .gif image, so we must write the module for creating .gif images with source of 2D matrix

## Requirements

* System.Console.ANSI module with clearScreen function

## Run

Not working with `runhaskell` command. I have no idea why.

```bash
$ ghci
Prelude> :l Main.hs
*Main> run
```
