# Battleships
The game Battleships was made as a final project at the course of Functional Programming in December of 2018. Written in Haskell.

## Compile and Run
The code can be compiled with following command
```bash
    stack ghc -- main.hs -o Battleships
```
You can run the code interactively
```bash
    Battleships
```

### Format of coordinates
(x,y) where x is a column and y is a row.

### Symbols
~ — empty cell

\# — undamaged ship cell

o — damaged ship cell

x — destroyed ship cell

· — miss

## Authors
Volkov Nikolay      —   game initialization, ships placement, validation of coordinates
Kadyrov Amir        —   bot's actions
Serazetdiov Damir   —   game logic, field displaying, processing of turns, shots
