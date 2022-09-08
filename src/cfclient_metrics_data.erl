-module(cfclient_metrics_data).

-export([encode/1]).

-export_type([cfclient_metrics_data/0]).

-type cfclient_metrics_data() ::
    #{ 'timestamp' := integer(),
       'count' := integer(),
       'metricsType' := binary(),
       'attributes' := list()
     }.

encode(#{ 'timestamp' := Timestamp,
          'count' := Count,
          'metricsType' := MetricsType,
          'attributes' := Attributes
        }) ->
    #{ 'timestamp' => Timestamp,
       'count' => Count,
       'metricsType' => MetricsType,
       'attributes' => Attributes
     }.
