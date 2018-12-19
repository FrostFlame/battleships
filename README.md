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
~ — empty cell\\n
\# — undamaged ship cell\\n
o — damaged ship cell\\n
x — destroyed ship cell\\n
· — miss

## Authors
Volkov Nikolay      —   game initialization, ships placement, validation of coordinates\\n
Kadyrov Amir        —   bot's actions\\n
Serazetdiov Damir   —   game logic, field displaying, processing of turns, shots\\n