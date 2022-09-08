-module(cfclient_error).

-export([encode/1]).

-export_type([cfclient_error/0]).

-type cfclient_error() ::
    #{ 'code' := binary(),
       'message' := binary(),
       'details' => maps:map()
     }.

encode(#{ 'code' := Code,
          'message' := Message,
          'details' := Details
        }) ->
    #{ 'code' => Code,
       'message' => Message,
       'details' => Details
     }.
