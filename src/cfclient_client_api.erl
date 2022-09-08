-module(cfclient_client_api).

-export([authenticate/2, authenticate/3,
         get_all_segments/2, get_all_segments/3,
         get_evaluation_by_identifier/4, get_evaluation_by_identifier/5,
         get_evaluations/3, get_evaluations/4,
         get_feature_config/2, get_feature_config/3,
         get_feature_config_by_identifier/3, get_feature_config_by_identifier/4,
         get_segment_by_identifier/3, get_segment_by_identifier/4,
         stream/2, stream/3]).

-define(BASE_URL, <<"/api/1.0">>).

%% @doc Authenticate with the admin server.
%% Used to retrieve all target segments for certain account id.
-spec authenticate(ctx:ctx()) -> {ok, cfclient_authentication_response:cfclient_authentication_response(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
authenticate(Ctx) ->
    authenticate(Ctx, #{}).

-spec authenticate(ctx:ctx(), maps:map()) -> {ok, cfclient_authentication_response:cfclient_authentication_response(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
authenticate(Ctx, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = post,
    Path = [<<"/client/auth">>],
    QS = [],
    Headers = [],
    Body1 = CfclientAuthenticationRequest,
    ContentTypeHeader = cfclient_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    cfclient_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Retrieve all segments.
%% Used to retrieve all segments for certain account id.
-spec get_all_segments(ctx:ctx(), binary()) -> {ok, [cfclient_segment:cfclient_segment()], cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_all_segments(Ctx, EnvironmentUUID) ->
    get_all_segments(Ctx, EnvironmentUUID, #{}).

-spec get_all_segments(ctx:ctx(), binary(), maps:map()) -> {ok, [cfclient_segment:cfclient_segment()], cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_all_segments(Ctx, EnvironmentUUID, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/client/env/", EnvironmentUUID, "/target-segments">>],
    QS = lists:flatten([])++cfclient_utils:optional_params(['cluster'], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = cfclient_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    cfclient_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Get feature evaluations for target
%% 
-spec get_evaluation_by_identifier(ctx:ctx(), binary(), binary(), binary()) -> {ok, cfclient_evaluation:cfclient_evaluation(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_evaluation_by_identifier(Ctx, EnvironmentUUID, Feature, Target) ->
    get_evaluation_by_identifier(Ctx, EnvironmentUUID, Feature, Target, #{}).

-spec get_evaluation_by_identifier(ctx:ctx(), binary(), binary(), binary(), maps:map()) -> {ok, cfclient_evaluation:cfclient_evaluation(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_evaluation_by_identifier(Ctx, EnvironmentUUID, Feature, Target, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/client/env/", EnvironmentUUID, "/target/", Target, "/evaluations/", Feature, "">>],
    QS = lists:flatten([])++cfclient_utils:optional_params(['cluster'], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = cfclient_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    cfclient_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Get feature evaluations for target
%% 
-spec get_evaluations(ctx:ctx(), binary(), binary()) -> {ok, cfclient_get_evaluations_200_response:cfclient_get_evaluations_200_response(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_evaluations(Ctx, EnvironmentUUID, Target) ->
    get_evaluations(Ctx, EnvironmentUUID, Target, #{}).

-spec get_evaluations(ctx:ctx(), binary(), binary(), maps:map()) -> {ok, cfclient_get_evaluations_200_response:cfclient_get_evaluations_200_response(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_evaluations(Ctx, EnvironmentUUID, Target, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/client/env/", EnvironmentUUID, "/target/", Target, "/evaluations">>],
    QS = lists:flatten([])++cfclient_utils:optional_params(['cluster'], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = cfclient_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    cfclient_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Get all feature flags activations
%% All feature flags with activations in project environment
-spec get_feature_config(ctx:ctx(), binary()) -> {ok, [cfclient_feature_config:cfclient_feature_config()], cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_feature_config(Ctx, EnvironmentUUID) ->
    get_feature_config(Ctx, EnvironmentUUID, #{}).

-spec get_feature_config(ctx:ctx(), binary(), maps:map()) -> {ok, [cfclient_feature_config:cfclient_feature_config()], cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_feature_config(Ctx, EnvironmentUUID, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/client/env/", EnvironmentUUID, "/feature-configs">>],
    QS = lists:flatten([])++cfclient_utils:optional_params(['cluster'], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = cfclient_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    cfclient_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Get feature config
%% 
-spec get_feature_config_by_identifier(ctx:ctx(), binary(), binary()) -> {ok, cfclient_feature_config:cfclient_feature_config(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_feature_config_by_identifier(Ctx, Identifier, EnvironmentUUID) ->
    get_feature_config_by_identifier(Ctx, Identifier, EnvironmentUUID, #{}).

-spec get_feature_config_by_identifier(ctx:ctx(), binary(), binary(), maps:map()) -> {ok, cfclient_feature_config:cfclient_feature_config(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_feature_config_by_identifier(Ctx, Identifier, EnvironmentUUID, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/client/env/", EnvironmentUUID, "/feature-configs/", Identifier, "">>],
    QS = lists:flatten([])++cfclient_utils:optional_params(['cluster'], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = cfclient_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    cfclient_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Retrieve a segment by identifier
%% Used to retrieve a segment for a certain account id by identifier
-spec get_segment_by_identifier(ctx:ctx(), binary(), binary()) -> {ok, cfclient_segment:cfclient_segment(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_segment_by_identifier(Ctx, Identifier, EnvironmentUUID) ->
    get_segment_by_identifier(Ctx, Identifier, EnvironmentUUID, #{}).

-spec get_segment_by_identifier(ctx:ctx(), binary(), binary(), maps:map()) -> {ok, cfclient_segment:cfclient_segment(), cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
get_segment_by_identifier(Ctx, Identifier, EnvironmentUUID, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/client/env/", EnvironmentUUID, "/target-segments/", Identifier, "">>],
    QS = lists:flatten([])++cfclient_utils:optional_params(['cluster'], _OptionalParams),
    Headers = [],
    Body1 = [],
    ContentTypeHeader = cfclient_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    cfclient_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).

%% @doc Stream endpoint.
%% 
-spec stream(ctx:ctx(), binary()) -> {ok, [], cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
stream(Ctx, APIKey) ->
    stream(Ctx, APIKey, #{}).

-spec stream(ctx:ctx(), binary(), maps:map()) -> {ok, [], cfclient_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), cfclient_utils:response_info()}.
stream(Ctx, APIKey, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = [<<"/stream">>],
    QS = lists:flatten([])++cfclient_utils:optional_params(['cluster'], _OptionalParams),
    Headers = [  {<<"API-Key">>, APIKey}]++cfclient_utils:optional_params([], _OptionalParams),
    Body1 = [],
    ContentTypeHeader = cfclient_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    cfclient_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


