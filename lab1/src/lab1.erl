-module(lab1).
-author("Mateusz").

%% API
-export([hello_world/0]).
-export([hello_world/1]).

hello_world() ->
  io:format("Hello world!~n").

hello_world(Whom) ->
  io:format("Hello ~s!~n",[Whom]).

%%cd("") - przechodzi do podanej ścieżki, c() - ręczna kompilacja kodu