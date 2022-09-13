-module(cfapi_metrics).

-export([encode/1]).

-export_type([cfapi_metrics/0]).

-type cfapi_metrics() ::
    #{ 'targetData' => list(),
       'metricsData' => list()
     }.

encode(#{ 'targetData' := TargetData,
          'metricsData' := MetricsData
        }) ->
    #{ 'targetData' => TargetData,
       'metricsData' => MetricsData
     }.
