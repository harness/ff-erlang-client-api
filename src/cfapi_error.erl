-module(cfapi_error).

-export([encode/1]).

-export_type([cfapi_error/0]).

-type cfapi_error() :: #{code := binary(), message := binary(), details => maps:map()}.

encode(#{code := Code, message := Message, details := Details}) ->
  #{code => Code, message => Message, details => Details}.
