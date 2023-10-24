-module(opty).
-export([start/5, stop/1]).

-define(SRV, 'opty-srv@LAPTOP-MC47CKNN').

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, Reads, Writes, Time) ->
    spawn(?SRV, fun() -> register(s, server:start(Entries)) end),
    L = startClients(Clients, [], Entries, Reads, Writes),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n",
              [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time),
    stop(L).


stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    {s, ?SRV} ! stop,
    io:format("Stopped~n").


startClients(0, L, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes) ->
    Pid = client:start(Clients, Entries, Reads, Writes, {s, ?SRV}),
    startClients(Clients - 1, [Pid | L], Entries, Reads, Writes).


stopClients([]) ->
    ok;
stopClients([Pid | L]) ->
    Pid ! {stop, self()},
    stopClients(L).


waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.
