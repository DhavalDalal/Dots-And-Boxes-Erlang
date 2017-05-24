-module (dots_and_boxes).
-export ([start/0, init/0, create_game/4, join/4, is_game_over/1, scores/1, results/1, stop/1, auto_play/3]).

start() ->
  spawn(?MODULE, init, []).

init() ->
  loop().
  
loop() ->
  receive
    {From, Tag, {auto_play, Players, XDots, YDots}} ->
      case game_supervisor:create_game(Players, XDots, YDots) of
        {ok, Game} ->
           From ! {Tag, play(From, Tag, Game, fun() -> rand_dots_pair(XDots,YDots) end)};
           
        Error ->
          From ! {Tag, Error}
       end,
       loop();
      
    {From, Tag, {create, Players, XDots, YDots}} ->
      case game_supervisor:create_game(Players, XDots, YDots) of
        {ok, {game, _, Grid, NextPlayer}} -> 
          {ok, TextUi} = game_supervisor:render_game(Grid),
          From ! {Tag, {TextUi, NextPlayer}};
        Error ->
          From ! {Tag, Error}
      end,
      loop();
      
    {From, Tag, {join, D1, D2, Player}} ->
      case game_supervisor:join(D1, D2, Player) of 
        {ok, {game, _, Grid, NextPlayer}} ->
          {ok, TextUi} = game_supervisor:render_game(Grid),
          From ! {Tag, {TextUi, NextPlayer}};
          
        Error ->
          From ! {Tag, Error}
      end,
      loop();
      
    {From, Tag, is_game_over} -> 
      From ! {Tag, game_supervisor:is_game_over()},
      loop();

    {From, Tag, scores} -> 
      From ! {Tag, game_supervisor:scores()},
      loop();
      
    {From, Tag, results} -> 
      From ! {Tag, game_supervisor:results()},
      loop();

    {From, Tag, stop} ->
      From ! {Tag, game_stopped}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functional Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

auto_play(Players, XDots, YDots) ->
  Game = start(),
  Tag = make_ref(),
  Game ! {self(), Tag, {auto_play, Players, XDots, YDots}},
  auto_play_loop(Game, Tag).
  
auto_play_loop(Game, Tag) ->
  receive
    {Tag, game_over, {_, Results}} ->
      io:format("*** Game Over ***~n"),
      io:format("Results = ~p~n", [Results]),
      stop(Game);
      
    {Tag, {TextUi, NextPlayer}} -> 
      io:format(TextUi),
      io:format("~nNow ~p's turn...~n~n", [NextPlayer]),
      auto_play_loop(Game, Tag);

    {Tag, {error, Message}} ->
      io:format(Message),
      stop(Game)
  end.

create_game(Client, Players, XDots, YDots) ->
  case rpc(Client, {create, Players, XDots, YDots}) of
    Error = {error, _} ->
      Error;
      
    {TextUi, NextPlayer} ->
      io:format(TextUi),
      io:format("Now ~p's turn~n", [NextPlayer])
  end.

join(Client, D1, D2, Player) ->
  case rpc(Client, {join, D1, D2, Player}) of
    Error = {error, _} -> 
      Error;
      
    {Game, NextPlayer} ->
      io:format(Game),
      io:format("Now ~p's turn~n", [NextPlayer])
  end.
  
is_game_over(Client) ->
  rpc(Client, is_game_over).

scores(Client) ->
  rpc(Client, scores).

results(Client) ->
  rpc(Client, results).

stop(Client) ->
  rpc(Client, stop).

rpc(Client, Query) ->
  Tag = make_ref(),
  Client ! {self(), Tag, Query},
  receive
    {Tag, Response} ->
      Response
  end.

play(Client, Tag, Game={_, _,Grid,NextPlayer}, RandF) ->
  {ok, TextUi} = game_supervisor:render_game(Grid),
  Client ! {Tag, {TextUi, NextPlayer}},
  case game_supervisor:is_game_over() of
    {ok, true} -> 
      Client ! {Tag, game_over, game_supervisor:results()};
      
    {ok, false} -> 
      continue_play(Client, Tag, Game, RandF)
  end.

continue_play(Client, Tag, Game, RandF) ->
  case turn(Game, RandF) of
    {ok, NextGame} ->
      play(Client, Tag, NextGame, RandF);
      
    Error ->
      Client ! {Tag, Error}
  end.

turn(Game, RandomF) ->
  {game, _, Grid, Player} = Game,
  {D1,D2} = RandomF(),
  case line_not_exists(D1, D2, Grid) of
    true -> 
      game_supervisor:join(D1, D2, Player);
      
    false -> 
      turn(Game, RandomF)
  end.

line_not_exists(D1, D2, Grid) ->
  Line = {line,D1,D2,false},
  GLines = lists:foldr(fun({box, Lines, _}, A) -> lists:append(A, Lines) end, [], Grid),
  contains(Line, GLines).

contains(_, []) -> false;

contains(Line = {line,A,B,false}, [L|Ls]) ->
  case L of
    Line -> true;
    {line,P,Q,false} when (A==Q) and (B==P) -> true;
    _ -> contains(Line, Ls)
  end.

rand_dots_pair(XDots, YDots) ->
  D1 = rand_dot(XDots,YDots),
  D2 = rand_dot(XDots,YDots),
  case distance(D1, D2) == 1 of
    true -> {D1,D2};
    false -> rand_dots_pair(XDots, YDots)
  end.

distance({X1,Y1}, {X2,Y2}) ->
  math:sqrt((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)).

rand_dot(XDots,YDots) -> {rand(XDots),rand(YDots)}.

rand(N) when N >= 2 -> rand:uniform(N) - 1.

