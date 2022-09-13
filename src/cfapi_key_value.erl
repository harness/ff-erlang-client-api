-module(cfapi_key_value).

-export([encode/1]).

-export_type([cfapi_key_value/0]).

-type cfapi_key_value() ::
    #{ 'key' := binary(),
       'value' := binary()
     }.

encode(#{ 'key' := Key,
          'value' := Value
        }) ->
    #{ 'key' => Key,
       'value' => Value
     }.
