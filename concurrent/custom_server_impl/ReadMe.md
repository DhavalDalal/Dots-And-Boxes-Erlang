Custom Server
=============

To Compile the game:

```
1> c(game_supervisor).
{ok,game_supervisor}
2> c(game_server).
{ok,game_server}
3> c(render_server).
{ok,render_server}
4> c(dots_and_boxes).
{ok,dots_and_boxes}
```

To run the game in auto play mode:

```
5> game_supervisor:start().
{game_supervisor,<0.86.0>}
{game_server,<0.87.0>}
{render_server,<0.88.0>}
true
6> dots_and_boxes:auto_play([foo,bar],3,3).
+   +   +

+   +   +

+   +   +


Now foo's turn...

+   +   +

+   +---+

+   +   +


Now bar's turn...

+   +   +

+   +---+
|
+   +   +


Now foo's turn...
...
...


+---+---+
|foo|foo|
+---+---+
|foo|bar|
+---+---+


Now foo's turn...

*** Game Over ***
Results = {winner,foo,{scores,[{foo,3},{bar,1}]}}
Removing Game for <0.90.0>, reason normal
game_stopped

```

To Play the game in manual mode:

```
7> Game = dots_and_boxes:start().
<0.92.0>
8> dots_and_boxes:create_game(Game,[foo,bar],2,2).
+   +

+   +

Now foo's turn
ok
9> dots_and_boxes:join(Game,{0,0},{0,1},foo).
+   +
|
+   +

Now bar's turn
ok
10> dots_and_boxes:join(Game,{1,1},{0,1},bar).
+   +
|
+---+

Now foo's turn
ok

11> dots_and_boxes:is_game_over(Game).
{ok,false}

12> dots_and_boxes:scores(Game).
{ok,[{[],1}]}

13> dots_and_boxes:results(Game).
{ok,{winner,[],{scores,[{[],1}]}}}

14> dots_and_boxes:join(Game,{1,1},{1,0},bar).
{error, invalid_turn}

15> dots_and_boxes:join(Game,{1,1},{1,0}, foo).
+   +
|   |
+---+

Now bar's turn

16> dots_and_boxes:join(Game,{0,0},{1,0}, bar).
+---+
|bar|
+---+

Now bar's turn

17> dots_and_boxes:is_game_over(Game).
{ok,true}

18> dots_and_boxes:scores(Game).
{ok,[{foo,1}]}

19> dots_and_boxes:results(Game).
{ok,{winner,foo,{scores,[{foo,1}]}}}

```

Now, kill the client

```
20> exit(list_to_pid("<0.92.0>"), kill).
Removing Game for <0.92.0>, reason killed
true

```

Now, kill the render_server

```
21> exit(list_to_pid("<0.88.0>"), kill).
Supervisor <0.86.0> received EXIT for <0.88.0>, Reason:killed
true

22> dots_and_boxes:auto_play([foo,bar],2,2).
...
...
...
The game runs to completion

```

Now, kill the game_server

```
23> exit(list_to_pid("<0.87.0>"), kill).
Supervisor <0.86.0> received EXIT for <0.87.0>, Reason:killed
true

24> dots_and_boxes:auto_play([foo,bar],2,2).
...
...
...
The game runs to completion

```
