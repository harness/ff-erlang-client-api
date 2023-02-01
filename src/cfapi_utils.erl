-module(cfapi_utils).

-export([request/8, select_header_content_type/1, optional_params/2]).

-type response_info() :: #{status := integer(), headers := list()}.

-export_type([response_info/0]).

request(_Ctx, Method, Path, QS, Headers, Body, Opts, Cfg) ->
  {Headers1, QS1} = update_params_with_auth(Cfg, Headers, QS),
  Host = maps:get(host, Cfg, "localhost:8001"),
  Url = hackney_url:make_url(Host, Path, QS1),
  ConfigHackneyOpts = maps:get(hackney_opts, Cfg, []),
  Body1 =
    case lists:keyfind(<<"Content-Type">>, 1, Headers1) of
      {_, <<"application/json", _/binary>>} -> jsx:encode(Body);
      _ -> Body
    end,
  case hackney:request(Method, Url, Headers1, Body1, Opts ++ ConfigHackneyOpts) of
    {ok, ClientRef} ->
      %% return value if Opts includes `async`
      {ok, ClientRef};

    {ok, Status, RespHeaders, ClientRef} when Status >= 200, Status =< 299 ->
      {ok, ResponseBody} = hackney:body(ClientRef),
      Resp = decode_response(RespHeaders, ResponseBody),
      {ok, Resp, #{status => Status, headers => RespHeaders}};

    {ok, Status, RespHeaders, ClientRef} when Status >= 300 ->
      {ok, ResponseBody} = hackney:body(ClientRef),
      Resp = decode_response(RespHeaders, ResponseBody),
      {error, Resp, #{status => Status, headers => RespHeaders}}
  end.


decode_response(Headers, Body) ->
  case lists:keyfind(<<"Content-Type">>, 1, Headers) of
    {_, <<"application/json", _/binary>>} -> jsx:decode(Body, [return_maps, {labels, atom}]);
    %% TODO: yml, protobuf, user defined function
    _ -> Body
  end.


-spec optional_params([atom()], map()) -> [{atom(), term()}].
optional_params(Keys, Params) -> maps:to_list(maps:with(Keys, Params)).

-spec select_header_content_type([binary()]) -> [{Name :: binary(), Value :: binary()}].
select_header_content_type([]) -> [];

select_header_content_type(ContentTypes) ->
  case
  lists:member(<<"application/json">>, ContentTypes) orelse lists:member(<<"*/*">>, ContentTypes) of
    true -> [{<<"Content-Type">>, <<"application/json">>}];
    false -> [{<<"Content-Type">>, hd(ContentTypes)}]
  end.


auth_with_prefix(#{api_key_prefix := Prefixes}, Key, Token) when is_map(Prefixes) ->
  case maps:find(Key, Prefixes) of
    {ok, Prefix} -> <<Prefix/binary, " ", Token/binary>>;
    error -> Token
  end;

auth_with_prefix(_Cfg, _Key, Token) -> Token.


update_params_with_auth(Cfg, Headers, QS) ->
  AuthSettings = maps:get(auth, Cfg, #{}),
  Auths =
    #{
      'ApiKeyAuth' => #{type => apiKey, key => <<"api-key">>, in => header},
      'BearerAuth' => #{type => http, key => <<"Authorization">>, in => header}
    },
  maps:fold(
    fun
      (AuthName, #{type := _Type, in := In, key := Key}, {HeadersAcc, QSAcc}) ->
        case maps:get(AuthName, AuthSettings, undefined) of
          undefined -> {HeadersAcc, QSAcc};

          Value ->
            case In of
              header -> {[{Key, auth_with_prefix(Cfg, Key, Value)} | HeadersAcc], QSAcc};
              query -> {HeadersAcc, [{Key, auth_with_prefix(Cfg, Key, Value)} | QSAcc]}
            end
        end
    end,
    {Headers, QS},
    Auths
  ).
