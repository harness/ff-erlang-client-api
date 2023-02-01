-module(cfapi_feature_config).

-export([encode/1]).

-type cfapi_feature_config() :: #{
                                project := binary(),
                                environment := binary(),
                                feature := binary(),
                                state := cfapi_feature_state:cfapi_feature_state(),
                                kind := binary(),
                                variations := list(),
                                rules => list(),
                                defaultServe := cfapi_serve:cfapi_serve(),
                                offVariation := binary(),
                                prerequisites => list(),
                                variationToTargetMap => list(),
                                version => integer()
                              }.

-export_type([cfapi_feature_config/0]).

encode(
  #{
    project := Project,
    environment := Environment,
    feature := Feature,
    state := State,
    kind := Kind,
    variations := Variations,
    rules := Rules,
    defaultServe := DefaultServe,
    offVariation := OffVariation,
    prerequisites := Prerequisites,
    variationToTargetMap := VariationToTargetMap,
    version := Version
  }
) ->
  #{
    project => Project,
    environment => Environment,
    feature => Feature,
    state => State,
    kind => Kind,
    variations => Variations,
    rules => Rules,
    defaultServe => DefaultServe,
    offVariation => OffVariation,
    prerequisites => Prerequisites,
    variationToTargetMap => VariationToTargetMap,
    version => Version
  }.
