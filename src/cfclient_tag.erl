-module(cfclient_tag).

-export([encode/1]).

-export_type([cfclient_tag/0]).

-type cfclient_tag() ::
    #{ 'name' := binary(),
       'value' => binary()
     }.

encode(#{ 'name' := Name,
          'value' := Value
        }) ->
    #{ 'name' => Name,
       'value' => Value
     }.
