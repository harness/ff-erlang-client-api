-module(cfapi_weighted_variation).

-export([encode/1]).

-export_type([cfapi_weighted_variation/0]).

-type cfapi_weighted_variation() ::
    #{ 'variation' := binary(),
       'weight' := integer()
     }.

encode(#{ 'variation' := Variation,
          'weight' := Weight
        }) ->
    #{ 'variation' => Variation,
       'weight' => Weight
     }.
