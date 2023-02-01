-module(cfapi_clause).

-export([encode/1]).

-export_type([cfapi_clause/0]).

-type cfapi_clause() :: #{
                        id => binary(),
                        attribute := binary(),
                        op := binary(),
                        values := list(),
                        negate := boolean()
                      }.

encode(#{id := Id, attribute := Attribute, op := Op, values := Values, negate := Negate}) ->
  #{id => Id, attribute => Attribute, op => Op, values => Values, negate => Negate}.
