-module(cfclient_target_map).

-export([encode/1]).

-export_type([cfclient_target_map/0]).

-type cfclient_target_map() ::
    #{ 'identifier' := binary(),
       'name' := binary()
     }.

encode(#{ 'identifier' := Identifier,
          'name' := Name
        }) ->
    #{ 'identifier' => Identifier,
       'name' => Name
     }.
