-module(validator).
-export([start/0]).


start() ->
    spawn_link(fun() -> init() end).


init() ->
    validator().


validator() ->
    receive
        {validate, Ref, _, Writes, Client, TRef} ->
            Tag = make_ref(),
            send_write_checks(Writes, Tag, TRef),
            % check reads just counts the number of ok answers so it can be used
            % here as well
            case check_reads(length(Writes), Tag) of
                ok ->
                    update(Writes),
                    Client ! {Ref, ok};
                abort ->
                    Client ! {Ref, abort}
            end,
            % send_read_checks(Reads, Tag),
            %% case check_reads(length(Reads), Tag) of
            %%     ok ->
            %%         update(Writes),
            %%         Client ! {Ref, ok};
            %%     abort ->
            %%         Client ! {Ref, abort}
            %% end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.


update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) ->
                          Entry ! {write, Value}
                  end,
                  Writes).


%% send_read_checks(Reads, Tag) ->
%%     Self = self(),
%%     lists:foreach(fun({Entry, Time, _}) ->
%%                           Entry ! {check, Tag, Time, Self}
%%                   end,
%%                   Reads).


send_write_checks(Writes, Tag, TRef) ->
    Self = self(),
    lists:foreach(fun({_, Entry, Value}) ->
                          Entry ! {forward_check, Tag, Value, Self, TRef}
                  end,
                  Writes).


check_reads(0, _) ->
    ok;
check_reads(N, Tag) ->
    receive
        {Tag, ok} ->
            check_reads(N - 1, Tag);
        {Tag, abort} ->
            abort
    end.
