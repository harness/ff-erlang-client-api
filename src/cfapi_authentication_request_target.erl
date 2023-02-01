-module(cfapi_authentication_request_target).

-export([encode/1]).

-export_type([cfapi_authentication_request_target/0]).

-type cfapi_authentication_request_target() :: #{
                                               identifier := binary(),
                                               name => binary(),
                                               anonymous => boolean(),
                                               attributes => maps:map()
                                             }.

encode(#{identifier := Identifier, name := Name, anonymous := Anonymous, attributes := Attributes}) ->
  #{identifier => Identifier, name => Name, anonymous => Anonymous, attributes => Attributes}.
