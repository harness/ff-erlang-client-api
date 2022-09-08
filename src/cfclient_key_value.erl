-module(cfclient_key_value).

-export([encode/1]).

-export_type([cfclient_key_value/0]).

-type cfclient_key_value() ::
    #{ 'key' := binary(),
       'value' := binary()
     }.

encode(#{ 'key' := Key,
          'value' := Value
        }) ->
    #{ 'key' => Key,
       'value' => Value
     }.
