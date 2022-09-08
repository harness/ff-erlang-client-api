-module(cfclient_authentication_response).

-export([encode/1]).

-export_type([cfclient_authentication_response/0]).

-type cfclient_authentication_response() ::
    #{ 'authToken' := binary()
     }.

encode(#{ 'authToken' := AuthToken
        }) ->
    #{ 'authToken' => AuthToken
     }.
