-module(cfclient_authentication_request).

-export([encode/1]).

-export_type([cfclient_authentication_request/0]).

-type cfclient_authentication_request() ::
    #{ 'apiKey' := binary(),
       'target' => cfclient_authentication_request_target:cfclient_authentication_request_target()
     }.

encode(#{ 'apiKey' := ApiKey,
          'target' := Target
        }) ->
    #{ 'apiKey' => ApiKey,
       'target' => Target
     }.
