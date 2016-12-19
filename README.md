# elm-lang.de

## Setup

The following prerequisites need to be installed:

* [PostgreSQL](https://www.postgresql.org/download/) (Database)
* [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) (Haskell build tool)
* [Yarn](https://yarnpkg.com/en/docs/install) (front end package manager).
* [entr](http://entrproject.org) (a file watcher, this tool is optional but recommended for development)


```bash
$ # Create the database schema
$ bin/db-recreate.sh
$ # Download the proper GHC for the project and install it into an isolated location
$ stack setup
$ # Build the Haskell back end
$ stack build
$ # Download front end dependencies and build the Elm sources
$ yarn install
```

## Development

During development you will probably want to have `bin/watch-all.sh` running all the time. It builds the back end and the front end and watches all relevant files for changes. If a back end source file changes, the back end is rebuild and restarted automatically. If a front end source file changes, the front end is rebuild and a browser reload is triggered.

The webpack dev server runs on [`localhost:8080`](http://localhost:8080). Back end requests are proxied from localhost:8080/api to the back end by the webpack dev server. If for some reason you want to access the back end separately without relying on the webpack dev server proxy you can do so at [`localhost:8000`](http://localhost:8000).

### Other useful scripts and commands

* `bin/backend-build-run.sh`: Builds and starts the back end without watching for file changes.
* `bin/watch-backend.sh`: Build and run the back end, watch for back end source file changes and rebuild and restart the back end when a file changes. Does not build or start the front end.
* `npm run build`: Production build for the front end sources.
* `npm start`: Start the webpack-dev server, that is, build the front end and watch for file changes. Does not build or start the back end.
* `bin/db-recreate.sh`: Recreate the db schema. This deletes all data. This can also be used to create the initial db schema before starting the back end the first time on a new host.
* `bin/db-new-migration.sh`: Create a new dbmigrations file in backend/migrations.
* `bin/db-upgrade.sh`: Executes all outstanding database migrations. You usually do not need this script, since the back end does the same on startup.

The webpack dev server runs on [`localhost:8080`](http://localhost:8080)

## Production

TODO

## Used tools

* [elm-webpack-loader](https://github.com/rtfeldman/elm-webpack-loader)
* [elm-format](https://github.com/avh4/elm-format)

