-module(cfclient_variation).

-export([encode/1]).

-export_type([cfclient_variation/0]).

-type cfclient_variation() ::
    #{ 'identifier' := binary(),
       'value' := binary(),
       'name' => binary(),
       'description' => binary()
     }.

encode(#{ 'identifier' := Identifier,
          'value' := Value,
          'name' := Name,
          'description' := Description
        }) ->
    #{ 'identifier' => Identifier,
       'value' => Value,
       'name' => Name,
       'description' => Description
     }.
