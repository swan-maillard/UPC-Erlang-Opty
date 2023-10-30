-module(entry).
-export([new/1]).


new(Value) ->
    spawn_link(fun() -> init(Value) end).


init(Value) ->
    entry(Value, make_ref(), []).


entry(Value, Time, ReadSet) ->
    receive
        {read, Ref, From, TRef} ->
            From ! {Ref, self(), Value, Time},
            entry(Value, Time, [{TRef, Ref, Time} | ReadSet]);
        {remove, TRef} ->
            entry(Value, Time, lists:filter(fun({EntryTRef, _, _}) -> EntryTRef =/= TRef end, ReadSet));
        {write, New} ->
            entry(New, make_ref(), ReadSet);
        {check, Ref, Readtime, From} ->
            if
                Readtime == Time ->
                    From ! {Ref, ok};
                true ->
                    From ! {Ref, abort}
            end,
            entry(Value, Time, ReadSet);
        {forward_check, Ref, _, From, TRef} ->
            ReadElements = lists:filter(fun({TRefEntry, _, _}) -> TRefEntry =/= TRef end, ReadSet),
            case length(ReadElements) of
                0 ->
                    From ! {Ref, ok};
                _ ->
                    From ! {Ref, abort}
            end,
            entry(Value, Time, ReadSet);
        stop ->
            ok
    end.
