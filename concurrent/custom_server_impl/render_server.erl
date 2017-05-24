-module (render_server).
-export ([start/0, init/0, stop/0]).
-export ([textui/1]).

start() ->
  register(?MODULE, spawn(?MODULE, init, [])),
  whereis(?MODULE).
  
init() ->
  loop().
  
loop() ->
  receive
    {From, Tag, {textui, Grid}} ->
      From ! {Tag, {ok, to_string(Grid)}},
      loop();
      
    stop ->
      io:format("Stopped Render Server~n"),
      ok

     % Hot code loading
  end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functional Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop() ->
  ?MODULE ! stop.

textui(Grid) ->
  rpc({textui, Grid}).

rpc(Query) ->
  rpc(self(), Query).

rpc(Client, Query) ->
  Tag = make_ref(),
  ?MODULE ! {Client, Tag, Query},
  receive
    {Tag, {ok, Response}} -> Response;
    {Tag, Error} -> Error
  end.

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type point() :: {integer(),integer()}.
-type line() :: {atom(), point(), point(), boolean()}.
-type box() :: {atom(), [line()], string()}.
-type grid() :: [box()].

-spec rasterize(grid()) -> [any()].
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
    
to_string(Grid) ->
  RasterLines = rasterize(Grid),
  RasterText = lists:map(fun([RLines, CLines]) ->
    lists:append(
      to_row_line(RLines),
      to_col_line(CLines))
  end, RasterLines),
  lists:concat(RasterText).

to_row_line(Lines) ->
  Row = lists:map(fun(Line) -> 
    case align(Line) of
      horizontal -> "+---";
      horizontal_blank -> "+   "
    end 
  end, Lines),
  lists:concat(lists:append(Row, ["+\n"])).
                 
to_col_line(Lines) ->
  Col = lists:map(fun(Line) -> 
    case align(Line) of
      {vertical_blank, first_column} -> " ";
      {vertical, first_column} -> "|";
      vertical_blank -> "    ";
      vertical -> "   |";
      {vertical, TakenBy} -> 
        Name = atom_to_list(TakenBy),
        string:concat(string:left(Name, 3), "|")
    end
  end, Lines),
  lists:concat(lists:append(Col, ["\n"])).
  
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