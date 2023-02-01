-module(cfapi_serving_rule).

-export([encode/1]).

-export_type([cfapi_serving_rule/0]).

-type cfapi_serving_rule() :: #{
                              ruleId => binary(),
                              priority := integer(),
                              clauses := list(),
                              serve := cfapi_serve:cfapi_serve()
                            }.

encode(#{ruleId := RuleId, priority := Priority, clauses := Clauses, serve := Serve}) ->
  #{ruleId => RuleId, priority => Priority, clauses => Clauses, serve => Serve}.
