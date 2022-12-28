-module(sqlight_ffi).

-export([normalise_result/1]).

normalise_result(Result) ->
    case Result of
        ok -> {ok, nil};
        {error, Code} when is_integer(Code) -> {error, Code}
    end.
