-module(cfapi_authentication_request).

-export([encode/1]).

-export_type([cfapi_authentication_request/0]).

-type cfapi_authentication_request() :: #{
                                        apiKey := binary(),
                                        target
                                        =>
                                        cfapi_authentication_request_target:cfapi_authentication_request_target()
                                      }.

encode(#{apiKey := ApiKey, target := Target}) -> #{apiKey => ApiKey, target => Target}.
