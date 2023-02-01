-module(cfapi_prerequisite).

-export([encode/1]).

-export_type([cfapi_prerequisite/0]).

-type cfapi_prerequisite() :: #{feature := binary(), variations := list()}.

encode(#{feature := Feature, variations := Variations}) ->
  #{feature => Feature, variations => Variations}.
