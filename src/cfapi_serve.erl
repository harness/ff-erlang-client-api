-module(cfapi_serve).

-export([encode/1]).

-export_type([cfapi_serve/0]).

-type cfapi_serve() :: #{
                       distribution => cfapi_distribution:cfapi_distribution(),
                       variation => binary()
                     }.

encode(#{distribution := Distribution, variation := Variation}) ->
  #{distribution => Distribution, variation => Variation}.
