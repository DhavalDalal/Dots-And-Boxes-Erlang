-module (game_supervisor).
-export ([start/0, init/0, stop/0]).
-export ([create_game/3, join/3, is_game_over/0, results/0, scores/0, games_count/0, render_game/1]).
-compile(export_all).

% This is game supervisor.  Its simply a router.
% Forwards all requests to appropriate servers.
% Restarts a server, if it crashes.
% 
% Actor Design
% ============
% Processes are not objects, they are fundamental
% units of concurrency. Forget all the thread modeling
% techniques - it’s not even close. Share nothing copy
% everything changes the game.
% One process for each truly concurrent *activity* in
% the system. That is the rule. 
% 
start() ->
  register(?MODULE, spawn(?MODULE, init, [])).
  
init() ->
  SupPid = whereis(?MODULE),
  GameServer = spawn_link_game_server(SupPid),
  RenderServer = spawn_link_render_server(),
  process_flag(trap_exit, true),
  io:format("~p~n~p~n~p~n", [
    {?MODULE, SupPid},
    {game_server, GameServer},
    {render_server, RenderServer}
  ]),
  loop({GameServer, RenderServer}).
  
spawn_link_game_server(SupPid) ->
  spawn_link(game_server, init, [SupPid]).
  
spawn_link_render_server() ->
  spawn_link(render_server, init, []).

loop(SPids = {GameServer, RenderServer}) ->
  receive
    {From, Tag, game_server_request, Query} ->
      GameServer ! {From,Tag,Query},
      loop(SPids);
    
    {From, Tag, render_server_request, Query} ->
      RenderServer ! {From,Tag,Query},
      loop(SPids);
    
    {'EXIT', Pid, Reason} ->
      io:format("Supervisor ~p received EXIT for ~p, Reason:~p~n", [self(), Pid, Reason]),
      NewSPids = restart_server(Pid, SPids),
      loop(NewSPids);
   
    stop ->
      exit(GameServer, kill),
      exit(RenderServer, kill),
      io:format("Stopped Game Supervisor~n")
  end.

restart_server(DiedPid, SPids) ->
  case SPids of 
    {DiedPid, RenderServer} -> 
      SupPid = whereis(?MODULE),
      {spawn_link_game_server(SupPid), RenderServer};
    {GameServer, DiedPid} ->   
      {GameServer, spawn_link_render_server()}
  end.    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functional Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop() ->
  ?MODULE ! stop.
  
create_game(Players, XDots, YDots) ->
  rpc(game_server_request, {create, Players, XDots, YDots}).

join(D1, D2, Player) ->
  rpc(game_server_request, {join, D1, D2, Player}).
  
is_game_over() ->
  rpc(game_server_request, is_game_over).


scores() ->
  rpc(game_server_request, scores).

results() ->
  rpc(game_server_request, results).

games_count() ->
  rpc(game_server_request, count).

render_game(Grid) ->
  rpc(render_server_request, {textui, Grid}).
  
rpc(RequestType, Query) ->
  Tag = make_ref(),
  ?MODULE ! {self(), Tag, RequestType, Query},
  receive
    {Tag, Response} -> Response
  end.
  
