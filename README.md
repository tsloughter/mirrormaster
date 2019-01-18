mirrormaster
=====

Mirrormaster works as a [Hex](https://hex.pm) repository with the additional ability to fetch packages from other repositories.

Packages requested that are not found in the repository are then looked for in each repository in the `mirrormaster` `repos` configuration, in the order they are listed, the same way [rebar3](https://rebar3.org) handles multiple repositories. By default `repos` is a single element, the public repo at [hex.pm](https://hex.pm).

### Todo

* Support package publishing
* Replace mnesia
* Iterate over list of repos (currently just uses the first one)
* So much more... barely anything is done
