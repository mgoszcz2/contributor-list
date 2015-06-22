# Contributor list

A small Haskell script that generates JSON containing all of organization's
contributors and issues, to be loaded by a Github Pages script.

### Running

```sh
$ sudo apt-get install haskell-platform git
$ git clone https://github.com/mgoszcz2/contributor-list.git
$ cd contributor-list
# Likely needed to use sandboxes on Ubuntu's haskell-platform
$ cabal update && cabal install cabal-install
$ cabal sandbox init
# This will take some time - https://xkcd.com/303/
$ cabal install --only-dependencies
$ cabal run -- evercam
```

Running with an OAuth key is recommended, because of Github's API
[limits](https://developer.github.com/v3/#rate-limiting) (See below for `--key` option).

You can generate a personal access token [here](https://github.com/settings/tokens).
You shoud just select the `public_repo` scope (unless you want everyone to know about your private repos).

### Documentation

```
contributor-list - Get issues and cotributors of a Github entity

Usage: contributor-list [-q|--quiet] [-u|--user] [-p|--pretty]
                        [-a|--key OAUTHKEY] [-f|--file FILE] ENTITY

Available options:
  -h,--help                Show this help text
  -q,--quiet               Enable quiet mode
  -u,--user                Get user data
  -p,--pretty              Pretty print json
  -a,--key OAUTHKEY        Github OAuth key
  -f,--file FILE           Output file name (default ENTITY.json)
  ENTITY                   Entity (organization/user) name
```
