-module(handler).
-export([start/3]).


start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).


init(Client, Validator, Store) ->
    TRef = make_ref(),
    handler(Client, Validator, Store, [], [], TRef).


% Reads: [{Entry, Time}]
% Writes: [{N, Entry, Value}]
handler(Client, Validator, Store, Reads, Writes, TRef) ->
    receive
        {read, Ref, N} ->
            case lists:keyfind(N, 1, Writes) of
                {N, _, Value} ->
                    Client ! {value, Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes, TRef);
                false ->
                    store:lookup(N, Store) ! {read, Ref, self(), TRef},
                    handler(Client, Validator, Store, Reads, Writes, TRef)
            end;
        {Ref, Entry, Value, Time} ->
            Client ! {value, Ref, Value},
            handler(Client, Validator, Store, [{Entry, Time} | Reads], Writes, TRef);
        {write, N, Value} ->
            Entry = store:lookup(N, Store),
            Added = lists:keystore(N, 1, Writes, {N, Entry, Value}),
            handler(Client, Validator, Store, Reads, Added, TRef);
        {commit, Ref} ->
            Validator ! {validate, Ref, Reads, Writes, Client, TRef};
        {transaction_done} ->
            lists:foreach(fun({_, Entry, _}) ->
                                  Entry ! {remove, TRef}
                          end,
                          Reads);
        abort ->
            ok
    end.
