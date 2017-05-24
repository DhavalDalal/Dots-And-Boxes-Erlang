This game is very well described here https://icebreakerideas.com/fun-games-to-play-at-home/#Dots_and_Boxes

To compile the game:

```
1> c(dots_and_boxes).
{ok,dots_and_boxes}
To run the game:

29> dots_and_boxes:auto_play(["foo", "bar"],3,3).
+   +   +

+   +   +

+   +   +

"foo" playing...
+   +   +

+   +   +

+   +---+

"bar" playing...
+   +   +
        |
+   +   +

+   +---+

"foo" playing...
+   +   +
|       |
+   +   +

+   +---+

"bar" playing...
+   +   +
|       |
+---+   +

+   +---+

...
...
...

"bar" playing...
+---+---+
|foo|foo|
+---+---+
|bar|bar|
+---+---+

*** Game Over ***
Game has Drawn!!
Detailed Results: [{"foo",2},{"bar",2}]
```

You can also play this manually by using the dots/2 and boxes/1 functions once to create the grid and then use the join/4 functions for every turn of the player.

```
3> G = dots_and_boxes:boxes(dots_and_boxes:dots(3,3)). 

[{box,[{line,{0,0},{0,1},false}, 
       {line,{0,1},{1,1},false}, 
       {line,{1,1},{1,0},false}, 
       {line,{1,0},{0,0},false}], 
      []}, 
  {box,[{line,{0,1},{0,2},false}, 
        {line,{0,2},{1,2},false}, 
        {line,{1,2},{1,1},false}, 
        {line,{1,1},{0,1},false}], 
      []}, 
  {box,[{line,{1,0},{1,1},false}, 
        {line,{1,1},{2,1},false}, 
        {line,{2,1},{2,0},false}, 
        {line,{2,0},{1,0},false}], 
      []}, 
  {box,[{line,{1,1},{1,2},false}, 
        {line,{1,2},{2,2},false}, 
        {line,{2,2},{2,1},false}, 
        {line,{2,1},{1,1},false}], 
      []}
]

4> {G2, TakeNextTurnAgain} = dots_and_boxes:join({0,0}, {0,1}, foo, G). 

{[{box,[{line,{0,0},{0,1},true}, 
        {line,{0,1},{1,1},false}, 
        {line,{1,1},{1,0},false}, 
        {line,{1,0},{0,0},false}], 
      []}, 
  {box,[{line,{0,1},{0,2},false}, 
        {line,{0,2},{1,2},false}, 
        {line,{1,2},{1,1},false}, 
        {line,{1,1},{0,1},false}], 
      []}, 
  {box,[{line,{1,0},{1,1},false}, 
        {line,{1,1},{2,1},false}, 
        {line,{2,1},{2,0},false}, 
        {line,{2,0},{1,0},false}], 
      []}, 
  {box,[{line,{1,1},{1,2},false}, 
        {line,{1,2},{2,2},false}, 
        {line,{2,2},{2,1},false}, 
        {line,{2,1},{1,1},false}], 
      []}
], false}

5> dots_and_boxes:draw(G2).

+   +   +
|
+   +   +

+   +   +
```

Repeat 4 for another player by passing the next generation of the grid - G2 version.