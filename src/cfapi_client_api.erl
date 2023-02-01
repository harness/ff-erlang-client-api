-module(cfapi_client_api).

-export(
  [
    authenticate/1,
    authenticate/2,
    get_all_segments/2,
    get_all_segments/3,
    get_evaluation_by_identifier/4,
    get_evaluation_by_identifier/5,
    get_evaluations/3,
    get_evaluations/4,
    get_feature_config/2,
    get_feature_config/3,
    get_feature_config_by_identifier/3,
    get_feature_config_by_identifier/4,
    get_segment_by_identifier/3,
    get_segment_by_identifier/4,
    stream/2,
    stream/3
  ]
).

% @doc Authenticate with the admin server.
% Returns all target segments for an account.
-spec authenticate(ctx:ctx()) ->
  {ok, cfapi_authentication_response:cfapi_authentication_response(), cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
authenticate(Ctx) -> authenticate(Ctx, #{}).

-spec authenticate(ctx:ctx(), maps:map()) ->
  {ok, cfapi_authentication_response:cfapi_authentication_response(), cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
authenticate(Ctx, Opts) ->
  Cfg = maps:get(cfg, Opts, application:get_env(kuberl, config, #{})),
  Body = maps:get(params, Opts, #{}),
  HackneyOpts = maps:get(hackney_opts, Opts, []),
  Path = <<"/client/auth">>,
  Headers = cfapi_utils:select_header_content_type([<<"application/json">>]),
  cfapi_utils:request(Ctx, post, Path, [], Headers, Body, HackneyOpts, Cfg).

% @doc Retrieve all segments for an account.
-spec get_all_segments(ctx:ctx(), binary()) ->
  {ok, [cfapi_segment:cfapi_segment()], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_all_segments(Ctx, Environment) -> get_all_segments(Ctx, Environment, #{}).

-spec get_all_segments(ctx:ctx(), binary(), maps:map()) ->
  {ok, [cfapi_segment:cfapi_segment()], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_all_segments(Ctx, Environment, Opts) ->
  Params = maps:get(params, Opts, #{}),
  Cfg = maps:get(cfg, Opts, application:get_env(kuberl, config, #{})),
  Path = <<"/client/env/", Environment/binary, "/target-segments">>,
  QS = lists:flatten([]) ++ cfapi_utils:optional_params([cluster], Params),
  Headers = cfapi_utils:select_header_content_type([]),
  HackneyOpts = maps:get(hackney_opts, Opts, []),
  cfapi_utils:request(Ctx, get, Path, QS, Headers, [], HackneyOpts, Cfg).

% @doc Get feature evaluations for target
-spec get_evaluation_by_identifier(ctx:ctx(), binary(), binary(), binary()) ->
  {ok, cfapi_evaluation:cfapi_evaluation(), cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_evaluation_by_identifier(Ctx, Environment, Feature, Target) ->
  get_evaluation_by_identifier(Ctx, Environment, Feature, Target, #{}).

-spec get_evaluation_by_identifier(ctx:ctx(), binary(), binary(), binary(), maps:map()) ->
  {ok, cfapi_evaluation:cfapi_evaluation(), cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_evaluation_by_identifier(Ctx, Environment, Feature, Target, Optional) ->
  _OptionalParams = maps:get(params, Optional, #{}),
  Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),
  Path =
    [<<"/client/env/", Environment/binary, "/target/", Target, "/evaluations/", Feature, "">>],
  QS = cfapi_utils:optional_params([cluster], _OptionalParams),
  Headers = cfapi_utils:select_header_content_type([]),
  Opts = maps:get(hackney_opts, Optional, []),
  cfapi_utils:request(Ctx, get, [Path], QS, Headers, [], Opts, Cfg).

%% @doc Get feature evaluations for target
%%

-spec get_evaluations(ctx:ctx(), binary(), binary()) ->
  {
    ok,
    cfapi_get_evaluations_200_response:cfapi_get_evaluations_200_response(),
    cfapi_utils:response_info()
  }
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_evaluations(Ctx, Environment, Target) -> get_evaluations(Ctx, Environment, Target, #{}).

-spec get_evaluations(ctx:ctx(), binary(), binary(), maps:map()) ->
  {
    ok,
    cfapi_get_evaluations_200_response:cfapi_get_evaluations_200_response(),
    cfapi_utils:response_info()
  }
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_evaluations(Ctx, Environment, Target, Optional) ->
  _OptionalParams = maps:get(params, Optional, #{}),
  Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),
  Method = get,
  Path = [<<"/client/env/", Environment/binary, "/target/", Target, "/evaluations">>],
  QS = cfapi_utils:optional_params([cluster], _OptionalParams),
  Headers = cfapi_utils:select_header_content_type([]),
  Body = [],
  Opts = maps:get(hackney_opts, Optional, []),
  cfapi_utils:request(Ctx, Method, [Path], QS, Headers, Body, Opts, Cfg).

%% @doc Get all feature flags activations
%% All feature flags with activations in project environment

-spec get_feature_config(ctx:ctx(), binary()) ->
  {ok, [cfapi_feature_config:cfapi_feature_config()], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_feature_config(Ctx, Environment) -> get_feature_config(Ctx, Environment, #{}).

-spec get_feature_config(ctx:ctx(), binary(), maps:map()) ->
  {ok, [cfapi_feature_config:cfapi_feature_config()], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_feature_config(Ctx, Environment, Optional) ->
  _OptionalParams = maps:get(params, Optional, #{}),
  Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),
  Method = get,
  Path = [<<"/client/env/", Environment/binary, "/feature-configs">>],
  QS = lists:flatten([]) ++ cfapi_utils:optional_params([cluster], _OptionalParams),
  Headers = [],
  Body1 = [],
  ContentTypeHeader = cfapi_utils:select_header_content_type([]),
  Opts = maps:get(hackney_opts, Optional, []),
  cfapi_utils:request(Ctx, Method, [Path], QS, ContentTypeHeader ++ Headers, Body1, Opts, Cfg).

%% @doc Get feature config
%%

-spec get_feature_config_by_identifier(ctx:ctx(), binary(), binary()) ->
  {ok, cfapi_feature_config:cfapi_feature_config(), cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_feature_config_by_identifier(Ctx, Identifier, Environment) ->
  get_feature_config_by_identifier(Ctx, Identifier, Environment, #{}).

-spec get_feature_config_by_identifier(ctx:ctx(), binary(), binary(), maps:map()) ->
  {ok, cfapi_feature_config:cfapi_feature_config(), cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_feature_config_by_identifier(Ctx, Identifier, Environment, Optional) ->
  _OptionalParams = maps:get(params, Optional, #{}),
  Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),
  Method = get,
  Path = [<<"/client/env/", Environment/binary, "/feature-configs/", Identifier, "">>],
  QS = lists:flatten([]) ++ cfapi_utils:optional_params([cluster], _OptionalParams),
  Headers = [],
  Body1 = [],
  ContentTypeHeader = cfapi_utils:select_header_content_type([]),
  Opts = maps:get(hackney_opts, Optional, []),
  cfapi_utils:request(Ctx, Method, [Path], QS, ContentTypeHeader ++ Headers, Body1, Opts, Cfg).

%% @doc Retrieve a segment by identifier
%% Used to retrieve a segment for a certain account id by identifier

-spec get_segment_by_identifier(ctx:ctx(), binary(), binary()) ->
  {ok, cfapi_segment:cfapi_segment(), cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_segment_by_identifier(Ctx, Identifier, Environment) ->
  get_segment_by_identifier(Ctx, Identifier, Environment, #{}).

-spec get_segment_by_identifier(ctx:ctx(), binary(), binary(), maps:map()) ->
  {ok, cfapi_segment:cfapi_segment(), cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
get_segment_by_identifier(Ctx, Identifier, Environment, Optional) ->
  _OptionalParams = maps:get(params, Optional, #{}),
  Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),
  Method = get,
  Path = [<<"/client/env/", Environment/binary, "/target-segments/", Identifier, "">>],
  QS = lists:flatten([]) ++ cfapi_utils:optional_params([cluster], _OptionalParams),
  Headers = [],
  Body1 = [],
  ContentTypeHeader = cfapi_utils:select_header_content_type([]),
  Opts = maps:get(hackney_opts, Optional, []),
  cfapi_utils:request(Ctx, Method, [Path], QS, ContentTypeHeader ++ Headers, Body1, Opts, Cfg).

%% @doc Stream endpoint.
%%

-spec stream(ctx:ctx(), binary()) ->
  {ok, [], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
stream(Ctx, APIKey) -> stream(Ctx, APIKey, #{}).
-spec stream(ctx:ctx(), binary(), maps:map()) ->
  {ok, [], cfapi_utils:response_info()}
  | {ok, hackney:client_ref()}
  | {error, term(), cfapi_utils:response_info()}.
stream(Ctx, APIKey, Optional) ->
  _OptionalParams = maps:get(params, Optional, #{}),
  Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),
  Method = get,
  Path = [<<"/stream">>],
  QS = lists:flatten([]) ++ cfapi_utils:optional_params([cluster], _OptionalParams),
  Headers = [{<<"API-Key">>, APIKey}] ++ cfapi_utils:optional_params([], _OptionalParams),
  Body1 = [],
  ContentTypeHeader = cfapi_utils:select_header_content_type([]),
  Opts = maps:get(hackney_opts, Optional, []),
  cfapi_utils:request(Ctx, Method, [Path], QS, ContentTypeHeader ++ Headers, Body1, Opts, Cfg).
