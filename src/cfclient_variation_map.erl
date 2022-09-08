-module(cfclient_variation_map).

-export([encode/1]).

-export_type([cfclient_variation_map/0]).

-type cfclient_variation_map() ::
    #{ 'variation' := binary(),
       'targets' => list(),
       'targetSegments' => list()
     }.

encode(#{ 'variation' := Variation,
          'targets' := Targets,
          'targetSegments' := TargetSegments
        }) ->
    #{ 'variation' => Variation,
       'targets' => Targets,
       'targetSegments' => TargetSegments
     }.
