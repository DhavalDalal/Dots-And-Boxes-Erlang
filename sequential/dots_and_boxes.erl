% https://icebreakerideas.com/fun-games-to-play-at-home/#Dots_and_Boxes
-module (dots_and_boxes).
-export ([dots/2, boxes/1, join/4, draw/1, auto_play/3]).

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
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
% All Auto Play related functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

-spec auto_play([atom()], integer(), integer()) -> [any()].
auto_play(Players, XDots, YDots) when (XDots >= 2) and (YDots >= 2) and (length(Players) >= 2) ->
  Grid = boxes(dots(XDots, YDots)),
  % io:format("Initial Grid = ~p~n", [Grid]),
  draw(Grid),
  play(Players, Grid, fun() -> rand_dots_pair(XDots, YDots) end);

auto_play(_, _, _) ->
  io:format("Choose more than 2 players and have number of dots in X and Y direction as atleast 2!~n").
    
play(Players, Grid, RandF) ->
  case game_over(Grid) of
    true -> show_results(Grid);
    false -> continue_play(Players, Grid, RandF)
  end.
  
continue_play(Players = [P|Ps], G, RandF) -> 
  io:format("~p playing...~n", [P]),
  {NextG, SamePlayerTurn} = turn(P, G, RandF),
  draw(NextG), 
  case SamePlayerTurn of 
    true -> 
      io:format("~p taking another turn!~n", [P]),
      play(Players, NextG, RandF);
    false -> 
      play(lists:append(Ps,[P]), NextG, RandF)
  end.

show_results(Grid) ->
  All = [{Winner, Score}|Losers] = results(Grid),
  io:format("*** Game Over ***~n"),
  case lists:keymember(Score, 2, Losers) of
    true -> io:format("Game has Drawn!!~n");
    false -> io:format("Winner => ~p~n", [Winner])
  end,
  io:format("Detailed Results: ~p~n", [All]).

results(Grid) ->
  Ps = [P || {box,_,P} <- Grid],
  Rs = frequency(Ps),
  lists:reverse(lists:keysort(2, Rs)).
    
game_over(Grid) -> 
  lists:all(fun signed_box/1, Grid).
  
frequency(Xs) -> frequency(Xs, []).
frequency([], Acc) -> Acc;
frequency(Xs = [X|_], Acc) -> 
  {Ys,Ns} = lists:partition(fun(E) -> E == X end, Xs),
  frequency(Ns, [{hd(Ys),length(Ys)} | Acc]).
  
turn(Player, Grid, RandomF) ->
  {D1,D2} = RandomF(),
  case line_not_exists(D1, D2, Grid) of
    true -> join(D1, D2, Player, Grid);
    false -> turn(Player, Grid, RandomF)
  end.

line_not_exists(D1, D2, Grid) ->
  Line = {line,D1,D2,false},
  GLines = lists:foldr(fun({box, Lines, _}, A) -> lists:append(A, Lines) end, [], Grid),
  contains(Line, GLines).
  
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All drawing related functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rasterize(Grid) ->
  GridLines = lists:flatmap(fun(B) ->
     annotate_lines(B) 
  end, Grid),
  Lines = lists:filter(fun({_,Aligned,_}) ->
    Aligned /= vertical_left_ignore
  end, GridLines),
  {HLines,VLines} = lists:partition(fun({_, Aligned, _}) -> Aligned == horizontal end, Lines),
  SortedHLines = lists:usort(HLines),
  {{_,_,{X,Y},_},_,_} = lists:last(SortedHLines),
  rasterize([], 0, 0, X, Y, SortedHLines, VLines).

rasterize(Acc, _, _, _, _, [], []) -> Acc;

rasterize(Acc, _, _, _, _, HLines, []) -> 
  lists:append(Acc, [[HLines,[]]]);

rasterize(Acc, X, Y, XMax, YMax, HLines, VLines) when (X =< XMax) or (Y =< YMax) ->
  {Hs,HLs} = lists:partition(
    fun({{_,{_,Y1},{_,Y2},_},_,_}) -> 
      (Y1 == Y) and (Y2 == Y) 
    end, HLines),
  {Vs, VLs} = lists:partition(
    fun({{_,{_,Y1},{_,Y2},_},_,_}) -> 
      (Y == Y1) and (Y2 == (Y + 1)) 
    end, VLines),
  NewAcc = lists:append(Acc,[[Hs,lists:sort(Vs)]]),
  rasterize(NewAcc,X+1,Y+1,XMax,YMax,HLs,VLs).
  
-spec annotate_lines(box()) -> [any()].
annotate_lines(B) ->
  {box,[VLeft,HUpper,VRight,HLower],TakenBy} = B,
  [
    {sort_line(HLower),horizontal, []},
    {sort_line(VRight),vertical_right, TakenBy},
    {sort_line(HUpper),horizontal, []},
    case VLeft of
      {line, {0, _}, {0, _}, _} -> 
        {sort_line(VLeft),vertical_first_col, []};
      _ ->
        {sort_line(VLeft),vertical_left_ignore, []}
    end
  ].
  
sort_line({line, D1, D2, Present}) when D1 > D2 ->
  {line, D2, D1, Present};
sort_line(L) -> L.
    
draw(Grid) ->
  % io:format("Grid = ~p~n", [Grid]),
  RasterLines = rasterize(Grid),
  % io:format("RasterLines = ~p~n", [RasterLines]),
  lists:foreach(fun([RLines, CLines]) ->
    draw_row_lines(RLines),
    draw_col_lines(CLines)
  end, RasterLines).

draw_row_lines(Lines) ->
  lists:foreach(fun(Line) -> 
    case align(Line) of
      horizontal -> io:format("+---");
      horizontal_blank -> io:format("+   ")
    end 
  end, Lines),
  io:format("+~n").
                 
draw_col_lines(Lines) ->
  lists:foreach(fun(Line) -> 
    case align(Line) of
      {vertical_blank, first_column} ->     
        io:format(" ");
      {vertical, first_column} -> 
        io:format("|");
      vertical_blank -> 
        io:format("    ");
      vertical -> 
        io:format("   |");
      {vertical, TakenBy} -> 
        io:format("~3s|", [TakenBy])
    end
  end, Lines),
  io:format("~n").
  
align({Line, vertical_first_col, _}) ->
  case Line of
    {line,_,_,false} ->
      {vertical_blank, first_column};
    {line,_,_,true} ->
      {vertical, first_column}
  end;

align(LInfo = {_, vertical_right, TakenBy}) ->
  case LInfo of 
    {{line,_,_,false},_,_} -> vertical_blank;
    {{line,_,_,true},_,[]} -> vertical;
    {{line,_,_,true},_,TakenBy} -> {vertical, TakenBy}
  end;

align({Line, horizontal, _}) -> 
  case Line of
    {line,_,_,false} -> horizontal_blank;
    {line,_,_,true} -> horizontal
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generic functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zipwith(F, Xs, Ys) ->
  zipwith(F, [], Xs, Ys).

zipwith(_F, Acc, [], []) -> Acc;  
zipwith(_F, Acc, _Xs, []) -> Acc;  
zipwith(_F, Acc, [], _Ys) -> Acc;    
zipwith(F, Acc, [X|Xs], [Y|Ys]) ->
  zipwith(F, lists:append(Acc, [F(X,Y)]), Xs, Ys).

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
    
index_of(Item, List) -> 
  index_of(Item, List, 1).

index_of(_, [], _) -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index + 1).
      
