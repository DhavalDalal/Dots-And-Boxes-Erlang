-module (game_server).
-export ([start/0, init/1, stop/0]).
-export ([create_game/3, join/3, scores/0, results/0, is_game_over/0, games_count/0, remove_game/0]).

start() ->
  register(?MODULE, spawn(?MODULE, init, [self()])),
  whereis(?MODULE).
  
init(SupervisorPid) ->
  process_flag(trap_exit, true),
  Games = [],
  loop(SupervisorPid, Games).
  
loop(SupervisorPid, Games) ->
  receive
    {From,Tag,count} ->
      From ! {Tag, {ok, length(Games)}},
      loop(SupervisorPid, Games);
      
    {From,Tag,{create,Players,XDots,YDots}} ->
      case find_game(From, Games) of
        {ok, _} -> 
          From ! {Tag, {error, game_already_present}},
          loop(SupervisorPid, Games);
          
        _Error ->
          case new_game(Players, XDots, YDots) of
            {ok, Game} ->
               NewGames = add_game(From, Game, Games),
               link(From),
               From ! {Tag, {ok, Game}},
               loop(SupervisorPid, NewGames);
               
            Error ->
              From ! {Tag, Error},
              loop(SupervisorPid, Games)
          end
      end;
      
    {From,Tag,{join,D1,D2,Player}} ->
      case find_game(From, Games) of
        {ok, Game} ->
          case turn(Player, D1, D2, Game) of
            {ok, NextGame} ->
              From ! {Tag, {ok, NextGame}},
              loop(SupervisorPid, update_game({From, Game}, NextGame, Games));
              
            Error ->
              From ! {Tag, Error},
              loop(SupervisorPid, Games)
          end;
          
        Error -> 
          From ! {Tag, Error},
          loop(SupervisorPid, Games)
      end;
      
    {From,Tag,scores} ->  
      case find_game(From, Games) of
        {ok, {game, _, Grid, _}} ->
          Results = scores(Grid),
          From ! {Tag, {ok, Results}};
          
        Error -> 
          From ! {Tag, Error}
      end,
      loop(SupervisorPid, Games);
      
    {From,Tag,results} ->  
      case find_game(From, Games) of
        {ok, {game, _, Grid, _}} ->
          Results = results(Grid),
          From ! {Tag, {ok, Results}};
          
        Error -> 
          From ! {Tag, Error}
      end,
      loop(SupervisorPid, Games);

    {From,Tag,is_game_over} ->     
      case find_game(From, Games) of
        {ok, {game, _, Grid, _}} ->
          From ! {Tag, {ok, game_over(Grid)}};
          
        Error -> 
          From ! {Tag, Error}
      end,
      loop(SupervisorPid, Games);
      
     {From,Tag,remove_game} ->
        case find_game(From, Games) of
          {ok, Game} ->
            NewGames = lists:delete({From,Game},Games),
            From ! {Tag, {ok, removed}},
            loop(SupervisorPid, NewGames);
            
          Error -> 
            From ! {Tag, Error},
            loop(SupervisorPid, Games)
        end;
      
     {'EXIT', SupervisorPid, Reason} ->
       io:format("Supervisor shutdown...~p~n", [Reason]),
       io:format("Stopped Game Server~n");
    
     {'EXIT', Client, Reason} ->
       io:format("Removing Game for ~p, reason ~p~n", [Client, Reason]),
       case find_game(Client, Games) of
         {ok, Game} ->
            loop(SupervisorPid, lists:delete({Client,Game}, Games));
          Error ->
            io:format("Not removed game for ~p, reason ~p~n", [Client, Error]),
            loop(SupervisorPid, Games)
        end;
        
     % {From, Tag, remove_completed_games} ->
     % Hot code loading
     
    {From, Tag, stop} ->
      From ! {Tag, stopped_game_server}
  end.


new_game(Players, XDots, YDots) when (XDots >= 2) and (YDots >= 2) and (length(Players) > 1) ->
  Grid = boxes(dots(XDots, YDots)),
  NextPlayer = lists:nth(1, Players),
  {ok, {game, Players, Grid, NextPlayer}};
  
new_game(_, _, _) ->
  {error, "Choose more than 2 players and/or have number of dots in X and Y direction as atleast 2!\n"}.
  
    
add_game(Pid, Game, Games) ->
  [{Pid, Game} | Games].
    
update_game(Game = {Pid, _}, NextGame, Games) ->
  [{Pid,NextGame} | lists:delete(Game, Games)].
  
find_game(Pid, Games) ->
  case lists:keyfind(Pid, 1, Games) of
    false ->
      {error, game_not_found};
      
    {_, Game} -> 
      {ok, Game}
  end.

turn(Player, D1, D2, {game, Players, Grid, Player}) ->
  case join(D1, D2, Player, Grid) of
    {NextGrid, true} -> 
      {ok, {game, Players, NextGrid, Player}};

    {NextGrid, false} -> 
      PlayerTurnNext = next(Player, Players),
      {ok, {game, Players, NextGrid, PlayerTurnNext}}
  end;

turn(_, _, _, _) ->
   {error, invalid_turn}.
  
next(Player, Players) ->
  case index_of(Player, Players) of
    not_found -> Player;
    Index -> 
      Size = length(Players),
      lists:nth((Index rem Size) + 1, Players)
  end.
 
index_of(Item, List) ->
  index_of(Item, List, 1).

index_of(_, [], _) -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index + 1).
  
stop() ->
  Tag = make_ref(),
  ?MODULE ! {self(), Tag, stop},
  receive
    {Tag, Response} -> Response
  end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functional Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_game(Players, XDots, YDots) ->
  rpc(self(), ?MODULE, {create, Players, XDots, YDots}).
  
join(D1, D2, Player) ->
  rpc(self(), ?MODULE, {join, D1, D2, Player}).

is_game_over() ->
  rpc(is_game_over).
  
scores() ->
  rpc(scores).

results() ->
  rpc(results).

games_count() ->
  rpc(count).

remove_game() ->
  rpc(remove_game).

rpc(Query) ->
  rpc(self(), ?MODULE, Query).

rpc(Client, Server, Query) ->
  Tag = make_ref(),
  Server ! {Client, Tag, Query},
  receive
    {Tag, {ok, Response}} -> Response;
    {Tag, Error} -> Error
  end.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type point() :: {integer(),integer()}.
-type line() :: {atom(), point(), point(), boolean()}.
-type dots() :: [point()].
-type box() :: {atom(), [line()], string()}.
-type grid() :: [box()].

-spec dots(integer(), integer()) -> dots().
dots(XDots, YDots) ->
  [{X,Y} || X <- lists:seq(0, XDots - 1), Y <- lists:seq(0, YDots - 1)].

-spec boxes(dots()) -> [box()].
boxes([]) -> [];
boxes(Ds) -> boxes(Ds, 1, length(Ds), []).
  
boxes(Dots, Begin, End, Bs) when Begin =< End ->
  Dot = lists:nth(Begin, Dots),
  BoxPts = box_points_for(Dot),
  case all_present(BoxPts, Dots) of
    true -> 
      B = {box, lines(BoxPts), []},
      boxes(Dots, Begin + 1, End, [B | Bs]);
    false -> 
      boxes(Dots, Begin + 1, End, Bs)
  end;
  
boxes(_,_,_,Bs) -> lists:sort(Bs).
  
all_present(BoxPts, Pts) -> BoxPts -- Pts == [].
  
box_points_for(BottomLeft) ->
  {X1, Y1} = BottomLeft,
  BoxSize = 1,
  X2 = X1 + BoxSize, Y2 = Y1 + 1,
  BottomRight = {X2, Y1},
  TopRight = {X2,Y2},
  TopLeft = {X1,Y2},
  [BottomLeft,TopLeft,TopRight,BottomRight].

lines([BottomLeft,TopLeft,TopRight,BottomRight]) ->
  %clockwise lines in a box
  [{line,BottomLeft,TopLeft,false}, 
   {line,TopLeft,TopRight,false}, 
   {line,TopRight,BottomRight,false}, 
   {line,BottomRight,BottomLeft,false}].  
  
-spec join(point(),point(),string(),grid()) -> {grid(), boolean()}.
% return pair contains a boolean to indicate
% take another turn for the last player.
join(D1, D2, Player, Grid) ->
  NextG = [mark_box(B,D1,D2,Player) || B <- Grid],
  {NextG, was_box_signed(Grid, NextG)}.

was_box_signed(PrevG, NextG) ->
  PPs = [B || B <- PrevG, signed_box(B)],
  NPs = [B || B <- NextG, signed_box(B)],
  length(NPs) - length(PPs) == 1. 

signed_box({box,_,P}) -> P /= [].

% 4 arg mark_box
mark_box(B = {box,Lines,[]}, D1, D2, Player) ->
  PlayerLine = {line,D1,D2,false},
  case contains(PlayerLine, Lines) of
    true -> signBox(mark_box(D1,D2,B), Player);
    false -> B
  end;
  
mark_box(Box,_,_,_) -> Box.

signBox(MBox = {box,MLines,_}, Player) ->
  case fourth_side_complete(MBox) of
    true -> {box,MLines,Player};
    false -> MBox  
  end.

% 3 arg mark_box
mark_box(D1, D2, {box,Lines,[]}) ->
  MLines = lists:map(fun 
    ({line,A,B,false}) when ((A==D1) and (B==D2)) -> join_dots(D1, D2);
    ({line,A,B,false}) when ((A==D2) and (B==D1)) -> join_dots(D2, D1); 
    (Line) -> Line
  end, Lines),
  {box,MLines,[]}.

contains(_, []) -> false;

contains(Line = {line,A,B,false}, [L|Ls]) ->
  case L of 
    Line -> true;
    {line,P,Q,false} when (A==Q) and (B==P) -> true;
    _ -> contains(Line, Ls)
  end.
  
fourth_side_complete({box,Lines,_}) ->
  lists:all(fun({line,_,_,Marked}) -> Marked == true end, Lines).
  
join_dots({X,Y1}, {X,Y2}) when abs(Y2-Y1) == 1 -> 
  {line,{X,Y1},{X,Y2},true};
join_dots({X1,Y}, {X2,Y}) when abs(X2-X1) == 1 -> 
  {line,{X1,Y},{X2,Y},true};
join_dots(_, _) ->
  badarg.

game_over(Grid) -> 
  lists:all(fun signed_box/1, Grid).

scores(Grid) ->
  Ps = [P || {box,_,P} <- Grid],
  Rs = frequency(Ps),
  lists:reverse(lists:keysort(2, Rs)).
    
frequency(Xs) -> frequency(Xs, []).
frequency([], Acc) -> Acc;
frequency(Xs = [X|_], Acc) -> 
  {Ys,Ns} = lists:partition(fun(E) -> E == X end, Xs),
  frequency(Ns, [{hd(Ys),length(Ys)} | Acc]).
  
results(Grid) ->
  All = [{Winner, Score}|Losers] = scores(Grid),
  case lists:keymember(Score, 2, Losers) of
    true -> {game_drawn, no_winner, {scores, All}};
    false -> {winner, Winner, {scores, All}}
  end.

      
