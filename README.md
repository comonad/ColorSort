This is an example Haskell implementation of a BFS solver for games like

ColorSort, WaterSort etc...
https://poki.com/en/g/water-color-sort

The solver is based on a custom monad "Prog", which allows parallelized BFS and allows to join/cut branches based on already visited game states.

Input is written within its source code (app/ColorSort/ColorSort.hs), the solution is calculated with

    stack run ColorSort



I tried to use the same solver for the problem HouseRiddle, but HouseRiddle was way too easy as a problem and too cumbersome to implement the strategy in order to call it beautiful.
I would now suggest using https://hackage.haskell.org/package/holmes for HouseRiddle instead.

    stack run HouseRiddle
