-module(sqlight_ffi).

-export([open/1, close/1]).

open(FileName) ->
    Chars = unicode:characters_to_list(FileName),
    esqlite3:open(Chars).

close(Connection) ->
    % TODO: support returning errors
    ok = esqlite3:close(Connection),
    {ok, nil}.
