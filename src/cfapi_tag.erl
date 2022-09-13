-module(cfapi_tag).

-export([encode/1]).

-export_type([cfapi_tag/0]).

-type cfapi_tag() ::
    #{ 'name' := binary(),
       'value' => binary()
     }.

encode(#{ 'name' := Name,
          'value' := Value
        }) ->
    #{ 'name' => Name,
       'value' => Value
     }.
