-module(cfclient_feature_config).

-export([encode/1]).

-export_type([cfclient_feature_config/0]).

-type cfclient_feature_config() ::
    #{ 'project' := binary(),
       'environment' := binary(),
       'feature' := binary(),
       'state' := cfclient_feature_state:cfclient_feature_state(),
       'kind' := binary(),
       'variations' := list(),
       'rules' => list(),
       'defaultServe' := cfclient_serve:cfclient_serve(),
       'offVariation' := binary(),
       'prerequisites' => list(),
       'variationToTargetMap' => list(),
       'version' => integer()
     }.

encode(#{ 'project' := Project,
          'environment' := Environment,
          'feature' := Feature,
          'state' := State,
          'kind' := Kind,
          'variations' := Variations,
          'rules' := Rules,
          'defaultServe' := DefaultServe,
          'offVariation' := OffVariation,
          'prerequisites' := Prerequisites,
          'variationToTargetMap' := VariationToTargetMap,
          'version' := Version
        }) ->
    #{ 'project' => Project,
       'environment' => Environment,
       'feature' => Feature,
       'state' => State,
       'kind' => Kind,
       'variations' => Variations,
       'rules' => Rules,
       'defaultServe' => DefaultServe,
       'offVariation' => OffVariation,
       'prerequisites' => Prerequisites,
       'variationToTargetMap' => VariationToTargetMap,
       'version' => Version
     }.
