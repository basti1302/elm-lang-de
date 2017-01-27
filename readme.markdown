# elm-lang.de

## What Is This?

This is the code that runs [elm-lang.de](https://elm-lang.de). The site elm-lang.de is a community hub for Elm developers in Germany and German speaking countries. It publishes news, events and articles related to the programming language [Elm](http://elm-lang.org/). Also, there is a developer directory where developers can create a profile for themselves and check who else works with Elm in their area. You can use the directory to connect with the community.

## Contributing

Contributions to elm-lang.de are most welcome. There's still a lot of work to do and features to build. So if you want to get your hands dirty, this section is for you.

Contributions can come in many forms. You can create issues if you notice any bugs or problems, that already helps a lot. If you feel like digging in to the code and change things, create a pull request for that. However, for bigger things it is probably a good idea to have a bit of discussion first. GitHub issues are one good way for that. Sending an [email](mailto:bastian.krol@codecentric.de) is fine, too.

The remainder of this section explains how to work with the code base.

### Setup

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

If you want to sign in via GitHub in you local development environment you also
need to follow the instructions given in .github-secret.template.

If you would like to have some test data you can run

```
psql -d elmlangde < backend/sql/profiles.sql
```

so the list of developer profiles is not empty.

### Development Setup

During development you will probably want to have `bin/watch-all.sh` running all the time. It builds the back end and the front end and watches all relevant files for changes. If a back end source file changes, the back end is rebuild and restarted automatically. If a front end source file changes, the front end is rebuild and a browser reload is triggered.

The webpack dev server runs on [`localhost:7000`](http://localhost:7000). Back end requests are proxied from localhost:7000/api to the back end by the webpack dev server. If for some reason you want to access the back end separately without relying on the webpack dev server proxy you can do so at [`localhost:8000`](http://localhost:8000).

### Other useful shell scripts and commands

* `bin/backend-build-run.sh`: Builds and starts the back end without watching for file changes.
* `bin/watch-backend.sh`: Build and run the back end, watch for back end source file changes and rebuild and restart the back end when a file changes. Does not build or start the front end.
* `npm run build`: Production build for the front end sources.
* `npm start`: Start the webpack-dev server, that is, build the front end and watch for file changes. Does not build or start the back end.
* `bin/db-recreate.sh`: Recreate the db schema. This deletes all data. This can also be used to create the initial db schema before starting the back end the first time on a new host.
* `bin/db-new-migration.sh`: Create a new dbmigrations file in backend/migrations.
* `bin/db-upgrade.sh`: Executes all outstanding database migrations. You usually do not need this script, since the back end does the same on startup.

The webpack dev server runs on [`localhost:7000`](http://localhost:7000)

### Configuration

Some operational aspects can be configured. All configuration is done solely through environment variables.

#### Database Related Environment Variables

Variable | Required  |  Default Value | Comment
---------|-----------|----------------|--------
`PGHOST` |        no | `localhost`    | The host name or IP of the PostgreSQL server
`PGDB`   |        no | `elmlangde`    | The name database of the PostgreSQL database
`PGUSER` |        no | `elmlangde`    | The PostgreSQL user
`PGPASS` |        no | `elmlangde`    | The PostgreSQL password

The provided default values are okay for development on your local machine. They must not be used in production (see below).

#### Web App Related Environment Variables

Variable                | Required  |  Default Value | Comment
------------------------|-----------|----------------|--------
HOST                    |        no | HostIPv4 | The default value is https://hackage.haskell.org/package/warp-1.3.7/docs/Network-Wai-Handler-Warp.html#v:HostIPv4, which means "any IPv4 host". Override it with `127.0.0.1` to only accept connections from localhost (might be suitable for development). The default value is okay for production.
PORT                    |        no | `8000`| The port on which the app listens to incoming connections. The default value is okay for production, assuming there is an Nginx for TLS termination in front of the app.
GITHUB_CLIENT_ID        |        no | - | The client ID required for GitHub OAuth. Without a client ID, signing up and signing in via GitHub will not be available. |
GITHUB_CLIENT_SECRET    |        no | - | The client secret required for GitHub OAuth. Without a client secret, signing up and signing in via GitHub will not be available. |
GITHUB_REDIRECT_URL     |        no | https://elm-lang.de/oauth/github | The redirect URL required for GitHub OAuth. It must match the URL the browser uses to connect to the app. The default value is only correct for production.
SECURE_COOKIES_DISABLED |        no | False | If the cookies set by the app have the secure flag set. The setting must only be used in development, so cookies work without HTTPS.
DEVELOPMENT_MODE        |        no | False | If this flag is set, static front end assets (like HTML, JS, CSS and images) will be served from `/frontend` instead of `/dist`.


### Production

TODO This section needs more detailed instructions.

* Run `npm run build` to make webpack build the productin front end assets in `/dist`.
* ...

