-module(cfapi_metrics_data).

-export([encode/1]).

-export_type([cfapi_metrics_data/0]).

-type cfapi_metrics_data() :: #{
                              timestamp := integer(),
                              count := integer(),
                              metricsType := binary(),
                              attributes := list()
                            }.

encode(
  #{timestamp := Timestamp, count := Count, metricsType := MetricsType, attributes := Attributes}
) ->
  #{timestamp => Timestamp, count => Count, metricsType => MetricsType, attributes => Attributes}.
