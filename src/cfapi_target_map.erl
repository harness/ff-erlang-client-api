-module(cfapi_target_map).

-export([encode/1]).

-export_type([cfapi_target_map/0]).

-type cfapi_target_map() ::
    #{ 'identifier' := binary(),
       'name' := binary()
     }.

encode(#{ 'identifier' := Identifier,
          'name' := Name
        }) ->
    #{ 'identifier' => Identifier,
       'name' => Name
     }.
