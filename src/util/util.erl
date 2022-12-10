-module(util).

-export([bitstring_to_integer/1]).

-include_lib("kernel/include/logger.hrl").

bitstring_to_integer(Str) when is_bitstring(Str) ->
    list_to_integer(binary_to_list(Str));
bitstring_to_integer(Str) ->
    ?LOG_WARNING("Invalid input: ~p", [Str]),
    -1.
