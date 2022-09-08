-module(cfclient_prerequisite).

-export([encode/1]).

-export_type([cfclient_prerequisite/0]).

-type cfclient_prerequisite() ::
    #{ 'feature' := binary(),
       'variations' := list()
     }.

encode(#{ 'feature' := Feature,
          'variations' := Variations
        }) ->
    #{ 'feature' => Feature,
       'variations' => Variations
     }.
