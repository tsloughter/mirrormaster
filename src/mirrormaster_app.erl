%%%-------------------------------------------------------------------
%% @doc mirrormaster public API
%% @end
%%%-------------------------------------------------------------------
-module(mirrormaster_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("mirrormaster.hrl").

start(_StartType, _StartArgs) ->
    ok = ensure_mnesia(),
    mirrormaster_sup:start_link().

stop(_State) ->
    ok.

%%

ensure_mnesia() ->
    Node = node(),
    case mnesia:create_schema([Node]) of
        ok ->
            mnesia:start(),
            {atomic, ok} =
                mnesia:create_table(package, [{type, ordered_set},
                                              {disc_copies, [Node]},
                                              {attributes, record_info(fields, package)}]),
            ok;
        {error, {Node, {already_exists, Node}}} ->
            mnesia:start();
        Error={error, _}->
            Error
    end.
