-module(mm_handler).

%% -behaviour(elli_handler).

-export([handle/2,
         handle_event/3]).

-include("mirrormaster.hrl").
-include_lib("elli/include/elli.hrl").

-type args() :: #{package_dir := file:name_all()}.

-spec handle(Req, Args) -> Result when
      Req    :: elli:req(),
      Args   :: args(),
      Result :: elli_handler:result().
handle(Req, Args) ->
    handle(Req#req.method, elli_request:path(Req), Req, Args).

-spec handle(Method, Path, Req, Args) -> elli_handler:result() when
      Method :: elli:http_method(),
      Path   :: [binary()],
      Req    :: elli:req(),
      Args   :: args().
handle('GET', [<<"packages">>, Name], _Req, #{repo_name := RepoName,
                                              repos := [HexConfig | _],
                                              private_key := PrivateKey}) ->
    Releases =
        case mm_packages:by_name(Name) of
            [] ->
                {ok, {200, _, R}} =
                    hex_repo:get_package(HexConfig, Name),
                _ = mm_packages:store(Name, R),
                R;
            Packages ->
                [#{version => Version,
                   checksum => Checksum,
                   dependencies => Dependencies} || #package{version=Version,
                                                             checksum=Checksum,
                                                             dependencies=Dependencies} <- Packages]
        end,

    Package = hex_registry:encode_package(#{repository => RepoName,
                                            name => Name,
                                            releases => Releases}),
    SignedPackage = hex_registry:sign_protobuf(Package, PrivateKey),

    {ok, [{<<"Content-Type">>, <<"application/octet-stream">>}], zlib:gzip(SignedPackage)};
handle('GET', [<<"tarballs">>, Filename], _Req, #{repos := [HexConfig | _],
                                                  package_dir := PackageDir}) ->
    FullPath = filename:join(PackageDir, Filename),
    case filelib:is_file(FullPath) of
        true ->
            ok;
        false ->
            [Name, Version] = string:split(filename:basename(Filename, ".tar"), "-"),
            {ok, {200, _, Tarball}} =
                hex_repo:get_tarball(HexConfig, Name, Version),
            file:write_file(FullPath, Tarball)
    end,

    Size = elli_util:file_size(FullPath),
    {ok, [{<<"Content-Length">>, Size}], {file, FullPath}}.

-spec handle_event(Event, Args, Config) -> ok when
      Event  :: elli:event(),
      Args   :: args(),
      Config :: [tuple()].
handle_event(_, _, _) ->
    ok.
