Concurrent Erlang
=================

The activities of playing the game and rendering are completely different and can happen in different processes.  To that end, there are two server processes,
one for playing the game, a.k.a. ```game_server``` and another for rendering the game as a text ui, a.k.a ```render_server```.

These are supervised using a ```game_supervisor```.  Game client(s) talk to the supervisor.  The supervisor routes requests either to the game server or to the render server appropriately.

~~~
    
          +-------------------------------------+
          |       Reply                         |
          |                        Request +----------+ 
          |                        +------>|  Render  |
          V                        |       |  Server  |
  +--------------+  Request +----------+   +----------+
  |Dots And Boxes|--------->|   Game   |
  +--------------+          |Supervisor|    
  Game    ^                 +----------+   +----------+
  Client  |                 Router |       |   Game   |   
          |                        +------>|  Server  |  
          |                        Request +----------+ 
          |         Reply                       |
          +-------------------------------------+


~~~

In case, if any of the servers die, they are restarted by the supervisor.  If a ```game_server``` dies all the clients associated with it die too.  As the ```game_server``` maintains games for all the clients, it is acceptable to kill the clients associated with it.  However, if a ```render_server``` dies, no clients are affected and is also restarted by the supervisor.  If the supervisor dies, then everything dies.

From scaling perspective, one can increase the number of ```game_server ```s and have the supervisor distribute the requests amongst them.  To achieve this the supervisor will have to keep a routing table as a part of its state, so that it can route the client to its appropriate ```game_server``` instance.

There are 2 implementations:
1. Custom Server
2. Using Gen Server (TODO)

