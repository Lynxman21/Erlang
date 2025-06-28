%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2025 17:10
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Mateusz").

%% API
-export([start/0,stop/0,play/1]).
-export([ping/0,pong/0]).

ping() ->
  receive
    stop -> io:format("Ping dead~n");
    {play,0} ->
      io:format("Ping done~n"),
      ping();
    {play,N} ->
      io:format("Ping~n"),
      pong ! {play,N-1},
      ping();
    _ ->
      io:format("What (Ping)?~n"),
      ping()
  after
    20000 -> io:format("Ping dead~n")
  end.

pong() ->
  receive
    stop -> io:format("Pong dead~n");
    {play,0} ->
      io:format("Pong done~n"),
      pong();
    {play,N} ->
      io:format("Pong~n"),
      ping ! {play,N-1},
      pong();
    _ ->
      io:format("What (Pong)?~n"),
      pong()
  after
    20000 -> io:format("Pong dead~n")
  end.

start() ->
  register(ping,spawn(pingpong,ping,[])),
  register(pong,spawn(pingpong,pong,[])),
  ok.


stop() ->
  ping ! stop,
  pong ! stop,
  ok.

play(N) when is_integer(N),N>0 ->
  ping ! {play, N},
  ok.

