-module(cfapi_target_data).

-export([encode/1]).

-export_type([cfapi_target_data/0]).

-type cfapi_target_data() :: #{identifier := binary(), name := binary(), attributes := list()}.

encode(#{identifier := Identifier, name := Name, attributes := Attributes}) ->
  #{identifier => Identifier, name => Name, attributes => Attributes}.
