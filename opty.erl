-module(opty).
-export([start/6, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)


start(Clients, Entries, Reads, Writes, Time, NSubSet) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Reads, Writes, NSubSet),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n",
              [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time),
    stop(L).


stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    s ! stop,
    io:format("Stopped~n").


startClients(0, L, _, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes, NSubSet) ->
    Pid = client:start(Clients, Entries, Reads, Writes, s, NSubSet),
    startClients(Clients - 1, [Pid | L], Entries, Reads, Writes, NSubSet).


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
