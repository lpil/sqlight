-module(sqlight_ffi).

-export([normalise_result/1, status/0]).

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
