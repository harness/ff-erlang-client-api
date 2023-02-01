-module(cfapi_variation_map).

-export([encode/1]).

-export_type([cfapi_variation_map/0]).

-type cfapi_variation_map() :: #{variation := binary(), targets => list(), targetSegments => list()}.

encode(#{variation := Variation, targets := Targets, targetSegments := TargetSegments}) ->
  #{variation => Variation, targets => Targets, targetSegments => TargetSegments}.
