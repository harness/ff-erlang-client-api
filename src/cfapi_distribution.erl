-module(cfapi_distribution).

-export([encode/1]).

-export_type([cfapi_distribution/0]).

-type cfapi_distribution() :: #{bucketBy := binary(), variations := list()}.

encode(#{bucketBy := BucketBy, variations := Variations}) ->
  #{bucketBy => BucketBy, variations => Variations}.
