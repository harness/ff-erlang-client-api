-module(cfclient_metrics).

-export([encode/1]).

-export_type([cfclient_metrics/0]).

-type cfclient_metrics() ::
    #{ 'targetData' => list(),
       'metricsData' => list()
     }.

encode(#{ 'targetData' := TargetData,
          'metricsData' := MetricsData
        }) ->
    #{ 'targetData' => TargetData,
       'metricsData' => MetricsData
     }.
