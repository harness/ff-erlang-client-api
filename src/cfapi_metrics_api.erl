-module(cfapi_metrics_api).

-export([post_metrics/3, post_metrics/4]).

% @doc Send metrics to the Analytics server.
-spec post_metrics(ctx:ctx(), binary(), binary()) ->
  {ok, [], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
post_metrics(Ctx, Cluster, Environment) -> post_metrics(Ctx, Cluster, Environment, #{}).

-spec post_metrics(ctx:ctx(), binary(), binary(), maps:map()) ->
  {ok, [], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
post_metrics(Ctx, Cluster, Environment, Opts) ->
  Params = maps:get(params, Opts, #{}),
  Cfg = maps:get(cfg, Opts, application:get_env(kuberl, config, #{})),
  Path = [<<"/metrics/">>, Environment],
  QS = cfapi_utils:optional_params([cluster], Cluster),
  ContentTypeHeader = cfapi_utils:select_header_content_type([<<"application/json">>]),
  HackneyOpts = maps:get(hackney_opts, Opts, []),
  cfapi_utils:request(Ctx, post, Path, QS, ContentTypeHeader, Params, HackneyOpts, Cfg).
