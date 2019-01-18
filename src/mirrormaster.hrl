%%
-record(package, {key :: {unicode:unicode_binary(), ec_semver:semver()},
                  version :: ec_semver:version_string(),
                  checksum :: binary(),
                  dependencies :: [dependency()],
                  retired :: retirement_status()}).
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
