-module(cfclient_serving_rule).

-export([encode/1]).

-export_type([cfclient_serving_rule/0]).

-type cfclient_serving_rule() ::
    #{ 'ruleId' => binary(),
       'priority' := integer(),
       'clauses' := list(),
       'serve' := cfclient_serve:cfclient_serve()
     }.

encode(#{ 'ruleId' := RuleId,
          'priority' := Priority,
          'clauses' := Clauses,
          'serve' := Serve
        }) ->
    #{ 'ruleId' => RuleId,
       'priority' => Priority,
       'clauses' => Clauses,
       'serve' => Serve
     }.
