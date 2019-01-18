%%
-record(package, {key :: {unicode:unicode_binary(), binary() | '_'},
                  version :: binary() | '_',
                  checksum :: binary() | '_',
                  dependencies :: [dependency()] | '_',
                  retired :: retirement_status() | '_'}).
-type package() :: #package{}.

-type dependency() :: #{package := unicode:unicode_binary(),
                        requirement := unicode:unicode_binary(),
                        optional := boolean(),
                        app := unicode:unicode_binary(),
                        repository := unicode:unicode_binary()}.

-type retirement_reason() :: 'RETIRED_OTHER' |
                             'RETIRED_INVALID' |
                             'RETIRED_SECURITY' |
                             'RETIRED_DEPRECATED' |
                             'RETIRED_RENAMED'.

-type retirement_status() :: #{reason := retirement_reason(),
                               message := unicode:unicode_binary()} | undefined.
