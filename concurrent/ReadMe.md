Concurrent Erlang
=================

The activities of playing a game and rendering are completely different and can happen in different processes.  To that end, there are two server processes,
one for playing the game, a.k.a. ```game_server``` and another for rendering the game as a text ui, a.k.a ```render_server```.

These are both supervised using a ```game_supervisor```.  Game client(s) talk to the supervisor.  The supervisor routes the request either to the game server or the render server appropriately.

~~~
             
                      +-------------------------------+
                      |                               |
                      |                          +----------+ 
                      V                         /|  Render  |
+--------+      +----------+                   / |  Server  |
|  Game  |----->|   Dots   |      +----------+/  +----------+
| Client |      |   And    |----->|   Game   |
|  1..n  |<-----|  Boxes   |      |Supervisor|    
+--------+      +----------+      +----------+\  +----------+
                 Game ^             Router     \ |   Game   |   
                      |                         \|  Server  |  
                      |                          +----------+ 
                      |                               |
                      +-------------------------------+
                    
~~~

In case any of the servers die, they are restarted by the supervisor.  If a ```game_server``` dies all the clients associated with it die too (as the game server maintains games for all the clients and is thus carries state).  However, if a ```render_server``` dies, no clients are affected and is also restarted by the supervisor.

There are 2 implementations:
1. Custom Server
2. Using Gen Server (TODO)

