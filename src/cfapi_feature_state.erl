-module(cfapi_feature_state).

-export([encode/1]).

-export_type([cfapi_feature_state/0]).

-type cfapi_feature_state() :: #{}.

encode(#{}) -> #{}.
