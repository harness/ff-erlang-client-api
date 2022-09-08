-module(cfclient_evaluation).

-export([encode/1]).

-export_type([cfclient_evaluation/0]).

-type cfclient_evaluation() ::
    #{ 'flag' := binary(),
       'value' := binary(),
       'kind' := binary(),
       'identifier' => binary()
     }.

encode(#{ 'flag' := Flag,
          'value' := Value,
          'kind' := Kind,
          'identifier' := Identifier
        }) ->
    #{ 'flag' => Flag,
       'value' => Value,
       'kind' => Kind,
       'identifier' => Identifier
     }.
