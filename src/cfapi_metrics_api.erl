-module(cfapi_metrics_api).

-export([post_metrics/3, post_metrics/4]).

%% @doc Send metrics to the Analytics server.
%% Send metrics to Analytics server

-spec post_metrics(ctx:ctx(), binary(), binary()) ->
  {ok, [], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
post_metrics(Ctx, Cluster, Environment) -> post_metrics(Ctx, Cluster, Environment, #{}).

-spec post_metrics(ctx:ctx(), binary(), binary(), maps:map()) ->
  {ok, [], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
post_metrics(Ctx, Cluster, Environment, Optional) ->
  OptionalParams = maps:get(params, Optional, #{}),
  Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),
  Method = post,
  Path = [<<"/metrics/", Environment/binary, "">>],
  QS = cfapi_utils:optional_params([cluster], Cluster),
  Headers = [],
  Body1 = OptionalParams,
  ContentTypeHeader = cfapi_utils:select_header_content_type([<<"application/json">>]),
  Opts = maps:get(hackney_opts, Optional, []),
  cfapi_utils:request(Ctx, Method, [Path], QS, ContentTypeHeader ++ Headers, Body1, Opts, Cfg).
