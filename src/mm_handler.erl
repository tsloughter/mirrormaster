-module(mm_handler).

%% -behaviour(elli_handler).

-export([handle/2,
         handle_event/3]).

-include("mirrormaster.hrl").
-include_lib("elli/include/elli.hrl").
-include_lib("kernel/include/logger.hrl").

-type args() :: #{repo_name := binary(),
                  repos := [hex_core:config()],
                  package_dir := file:name_all(),
                  private_key := binary()}.

-spec handle(Req, Args) -> Result when
      Req    :: elli:req(),
      Args   :: [args()],
      Result :: elli_handler:result().
handle(Req, [Map]) ->
    handle(Req#req.method, elli_request:path(Req), Req, Map).

-spec handle(Method, Path, Req, Args) -> elli_handler:result() when
      Method :: elli:http_method(),
      Path   :: [binary()],
      Req    :: elli:req(),
      Args   :: args().
handle('GET', [<<"packages">>, Name], _Req, #{repo_name := RepoName,
                                              repos := [HexConfig | _],
                                              private_key := PrivateKey}) ->
    ?LOG_DEBUG("request=/packages/:name name=~s", [Name]),
    Releases =
        case mm_packages:by_name(Name) of
            [] ->
                {ok, {200, _, R}} =
                    hex_repo:get_package(HexConfig, Name),
                _ = mm_packages:store(Name, R),
                R;
            Packages ->
                mm_packages:to_core_maps(Packages)
        end,

    Package = hex_registry:encode_package(#{repository => RepoName,
                                            name => Name,
                                            releases => Releases}),
    SignedPackage = hex_registry:sign_protobuf(Package, PrivateKey),

    {ok, [{<<"Content-Type">>, <<"application/octet-stream">>}], zlib:gzip(SignedPackage)};
handle('GET', [<<"tarballs">>, Filename], _Req, #{repos := [HexConfig | _],
                                                  package_dir := PackageDir}) ->
    ?LOG_DEBUG("request=/tarballs/:filename filename=~s", [Filename]),
    FullPath = filename:join(PackageDir, Filename),
    case filelib:is_file(FullPath) of
        true ->
            ok;
        false ->
            [Name, Version] = string:split(filename:basename(Filename, ".tar"), "-"),
            ?LOG_DEBUG("fetch=begin name=~s version=~s", [Name, Version]),
            {ok, {200, _, Tarball}} =
                hex_repo:get_tarball(HexConfig, Name, Version),
            file:write_file(FullPath, Tarball),
            ?LOG_DEBUG("fetch=complete path=~s", [FullPath])
    end,

    Size = elli_util:file_size(FullPath),
    {ok, [{<<"Content-Length">>, Size}], {file, FullPath}}.

-spec handle_event(Event, Args, Config) -> ok when
      Event  :: elli:event(),
      Args   :: [args()],
      Config :: [tuple()].
handle_event(_, _, _) ->
    ok.
