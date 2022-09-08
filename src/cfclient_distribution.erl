-module(cfclient_distribution).

-export([encode/1]).

-export_type([cfclient_distribution/0]).

-type cfclient_distribution() ::
    #{ 'bucketBy' := binary(),
       'variations' := list()
     }.

encode(#{ 'bucketBy' := BucketBy,
          'variations' := Variations
        }) ->
    #{ 'bucketBy' => BucketBy,
       'variations' => Variations
     }.
