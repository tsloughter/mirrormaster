-module(mm_packages).

-export([by_name/1,
         store/2,
         to_core_maps/1]).

-include("mirrormaster.hrl").

by_name(Name) ->
    mnesia:dirty_select(package, [{#package{key={Name, '_'},
                                            _='_'}, [], ['$_']}]).

store(Name, Releases) when is_list(Releases) ->
    [store(Name, Release) || Release <- Releases];
store(Name, Release=#{version := Version,
                      checksum := Checksum,
                      dependencies := Dependencies}) ->
    Retired = maps:get(retired, Release, undefined),
    {atomic, ok} =
        mnesia:transaction(fun mnesia:write/1, [#package{key={Name, Version},
                                                         version=Version,
                                                         checksum=Checksum,
                                                         dependencies=Dependencies,
                                                         retired=Retired}]).

to_core_maps(Packages) ->
    [#{version => Version,
       checksum => Checksum,
       dependencies => Dependencies,
       retired => Retired} || #package{version=Version,
                                       checksum=Checksum,
                                       dependencies=Dependencies,
                                       retired=Retired} <- Packages].
