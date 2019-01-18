%%%-------------------------------------------------------------------
%% @doc mirrormaster top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(mirrormaster_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    {ok, PackageDir} = application:get_env(mirrormaster, package_dir),
    filelib:ensure_dir(filename:join(PackageDir, "subdir")),

    {ok, PrivateKeyFile} = application:get_env(mirrormaster, private_key_file),
    {ok, PrivateKey} = file:read_file(to_filename(PrivateKeyFile)),

    {ok, Name} = application:get_env(mirrormaster, repo_name),

    Repos = application:get_env(mirrormaster, repos, [hex_core:default_config()]),

    ElliOpts = [{callback, mm_handler},
                {callback_args, #{repo_name   => Name,
                                  repos       => Repos,
                                  package_dir => PackageDir,
                                  private_key => PrivateKey}},
                {port, 3000}],
    ChildSpecs = [#{id => mm_http,
                    start => {elli, start_link, [ElliOpts]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [elli]}],

    {ok, {SupFlags, ChildSpecs}}.

%%

to_filename({priv, Filename}) ->
    Priv = code:priv_dir(mirrormaster),
    filename:join(Priv, Filename);
to_filename(Filename) ->
    Filename.
