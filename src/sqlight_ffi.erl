-module(sqlight_ffi).

-export([
    normalise_result/1, status/0, query/3, exec/2, coerce_value/1, null/0,
    error_info/1
]).

query(Sql, Connection, Arguments) ->
    case esqlite3:q(Sql, Connection, Arguments) of
        {error, Code} when is_integer(Code) -> {error, Code};
        Rows -> {ok, lists:map(fun erlang:list_to_tuple/1, Rows)}
    end.

exec(Sql, Connection) ->
    case esqlite3:exec(Sql, Connection) of
        {error, Code} when is_integer(Code) -> {error, Code};
        ok -> {ok, nil}
    end.

normalise_result(Result) ->
    case Result of
        ok -> {ok, nil};
        {error, Code} when is_integer(Code) -> {error, Code}
    end.

stats(#{used := Used, highwater := Highwater}) ->
    {stats, Used, Highwater}.

status() ->
    #{
        memory_used := MemoryUsed,
        pagecache_used := PagecacheUsed,
        pagecache_overflow := PagecacheOverflow,
        malloc_size := MallocSize,
        parser_stack := ParserStack,
        pagecache_size := PagecacheSize,
        malloc_count := MallocCount
    } = esqlite3:status(),
    {status_info,
        stats(MemoryUsed),
        stats(PagecacheUsed),
        stats(PagecacheOverflow),
        stats(MallocSize),
        stats(ParserStack),
        stats(PagecacheSize),
        stats(MallocCount)
    }.

coerce_value(X) -> X.
null() -> undefined.

error_info(Connection) ->
    #{errmsg := Message, error_offset := Offset} = esqlite3:error_info(Connection),
    {Message, Offset}.
