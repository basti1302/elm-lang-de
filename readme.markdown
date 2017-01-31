# elm-lang.de

This is the code that runs [elm-lang.de](https://elm-lang.de). The site elm-lang.de is a community hub for Elm developers in Germany and German speaking countries. It publishes news, events and articles related to the programming language [Elm](http://elm-lang.org/). Also, there is a developer directory where developers can create a profile for themselves and check who else works with Elm in their area. You can use the directory to connect with the community.

# Contributing

Contributions to elm-lang.de are most welcome. There's still a lot of work to do and features to build. So if you want to get your hands dirty, this section is for you.

Contributions can come in many forms. You can create issues if you notice any bugs or problems, that already helps a lot. If you feel like digging in to the code and change things, create a pull request for that. However, for bigger things it is probably a good idea to have a bit of discussion first. GitHub issues are one good way for that. Sending an [email](mailto:bastian.krol@codecentric.de) is fine, too.

The remainder of this section explains how to work with the code base.

# Development Setup

The following prerequisites need to be installed:

* [PostgreSQL](https://www.postgresql.org/download/) (Database)
* [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) (Haskell build tool)
* [Yarn](https://yarnpkg.com/en/docs/install) (front end package manager).
* [Docker](https://www.docker.com/) (not required for development, but to build new versions for production)
* [Docker Compose](https://docs.docker.com/compose/install/) (not required for development, but to build new versions for production)
* [entr](http://entrproject.org) (a file watcher, this tool is optional but recommended for development)

```bash
$ # Download the proper GHC for the project and install it into an isolated location
$ stack setup
$ # Install the dbmigrations executable for PostgreSQL (to manage the db schema)
$ stack install dbmigrations-postgresql
$ # Create the database schema
$ bin/db-recreate.sh
$ # Build the Haskell back end
$ stack build
$ # Download front end dependencies and build the Elm sources
$ yarn install
```

If you want to sign in via GitHub in you local development environment you also
need to follow the instructions given in .github-secret.template.

If you would like to have some test data you can run

```
psql -d elmlangde < backend/sql/test_data_profiles.sql
```

so the list of developer profiles is not empty.

# Development Workflow

During development you will probably want to have `bin/watch-all.sh` running all the time. It builds the back end and the front end and watches all relevant files for changes. If a back end source file changes, the back end is rebuild and restarted automatically. If a front end source file changes, the front end is rebuild and a browser reload is triggered.

The webpack dev server runs on [`localhost:7000`](http://localhost:7000). Back end requests are proxied from localhost:7000/api to the back end by the webpack dev server. If for some reason you want to access the back end separately without relying on the webpack dev server proxy you can do so at [`localhost:8000`](http://localhost:8000).

## Other Useful Shell Scripts and Commands

* `bin/backend-build-run.sh`: Builds and starts the back end without watching for file changes.
* `bin/watch-backend.sh`: Build and run the back end, watch for back end source file changes and rebuild and restart the back end when a file changes. Does not build or start the front end.
* `npm run build`: Production build for the front end sources.
* `npm start`: Start the webpack-dev server, that is, build the front end and watch for file changes. Does not build or start the back end.
* `bin/db-recreate.sh`: Recreate the db schema. This deletes all data. This can also be used to create the initial db schema before starting the back end the first time on a new host.
* `bin/db-new-migration.sh`: Create a new dbmigrations file in backend/migrations.
* `bin/db-upgrade.sh`: Executes all outstanding database migrations. You usually do not need this script, since the back end does the same on startup.

The webpack dev server runs on [`localhost:7000`](http://localhost:7000)

## Configuration

Some operational aspects can be configured. All configuration is done solely through environment variables.

### Database Related Environment Variables

Variable | Required  |  Default Value | Comment
---------|-----------|----------------|--------
`PGHOST` |        no | `localhost`    | The host name or IP of the PostgreSQL server
`PGDB`   |        no | `elmlangde`    | The name database of the PostgreSQL database
`PGUSER` |        no | `elmlangde`    | The PostgreSQL user
`PGPASS` |        no | `elmlangde`    | The PostgreSQL password

The provided default values are okay for development on your local machine. They must not be used in production (see below).

### Web App Related Environment Variables

Variable                | Required  |  Default Value | Comment
------------------------|-----------|----------------|--------
HOST                    |        no | HostIPv4 | The default value is https://hackage.haskell.org/package/warp-1.3.7/docs/Network-Wai-Handler-Warp.html#v:HostIPv4, which means "any IPv4 host". Override it with `127.0.0.1` to only accept connections from localhost (might be suitable for development). The default value is okay for production.
PORT                    |        no | `8000`| The port on which the app listens to incoming connections. The default value is okay for production, assuming there is an Nginx for TLS termination in front of the app.
GITHUB_CLIENT_ID        |        no | - | The client ID required for GitHub OAuth. Without a client ID, signing up and signing in via GitHub will not be available. |
GITHUB_CLIENT_SECRET    |        no | - | The client secret required for GitHub OAuth. Without a client secret, signing up and signing in via GitHub will not be available. |
GITHUB_REDIRECT_URL     |        no | https://elm-lang.de/oauth/github | The redirect URL required for GitHub OAuth. It must match the URL the browser uses to connect to the app. The default value is only correct for production.
SECURE_COOKIES_DISABLED |        no | False | If the cookies set by the app have the secure flag set. The setting must only be used in development, so cookies work without HTTPS.
DEVELOPMENT_MODE        |        no | False | If this flag is set, static front end assets (like HTML, JS, CSS and images) will be served from `/frontend` instead of `/dist`.


# Operation

elm-lang.de's production setup uses three Docker containers, elmlangde-postgres (the database), elmlangde-nginx (Nginx, for SSL-Termination) and last but not least elmlangde-app, the Haskell/Servant app which also includes the static front end assets including the compiled Elm code.

The containers have Dockerfiles in their respective directory under `docker/`. There is also a docker-compose.yml that describes the connections between the containers.

## Working with the Docker Containers

* Run `bin/container-rebuild-all.sh` to create (or recreate) all three containers.
* If you only want to recreate the app container and leave the Nginx and the PostgreSQL containers untouched, use `bin/container-rebuild-app.sh`.
* If you only need to recreate the nginx container, use `bin/container-rebuild-nginx.sh`.

This will create the Docker containers locally on your system. When the containers have been build (and tested) and you want to deploy them, they need to be pushed to the registry. Once a new version of a container is in the registry, it can be pulled on the target system and started there, see below.

For the nginx container there is one caveat: The initial deploy requires a rather ugly workaround on the very first deployment to production, to get the letsencrypt certs workflow right. It is documented in `docker/nginx/Dockerfile` in the comment beginning with "Install letsencrypt cert".

When one or more of the containers have been created (or recreated) its time to push them to the Docker registry. Right now, they live in Bastian's account at:

* https://cloud.docker.com/app/basti1302/repository/docker/basti1302/elmlangde-postgres
* https://cloud.docker.com/app/basti1302/repository/docker/basti1302/elmlangde-nginx
* https://cloud.docker.com/app/basti1302/repository/docker/basti1302/elmlangde-app

To be able to push the containers to the registry for the first time, a repository has to be created using the Dockerhub web UI. To actually push the containers, use the script `bin/container-push-all.sh` or these commands:

* `docker push basti1302/elmlangde-postgres`
* `docker push basti1302/elmlangde-nginx`
* `docker push basti1302/elmlangde-app`

To prepare a target system for the first deployment, you need to:

* Create a user `elmlangde` and grant it sudo privileges (see https://www.digitalocean.com/community/tutorials/initial-server-setup-with-ubuntu-16-04).
* `git clone` this repository there (prefered location: `/opt/elm-lang-de`).
* You might want to `chown -R` the repo to user `elmlangde`
* Run the script `/opt/elm-lang-de/docker/prepare-target-system.sh` *on the target machine*. This will install Docker and Docker Compose.
* Add the following line to `/etc/rc.local`: `/opt/elm-lang-de/bin/remote/start.sh`
* Copy the file `docker/postgres/postgres-secrets.env` from your *local* machine (where `elmlangde-postgres` has been build) to `/opt/elm-lang-de/docker/postgres/` on the target machine.
* Create a file `docker/app/app-secrets.env` in `/opt/elm-lang-de/docker/app/` on the target machine. You can use <project-root>/docker/app/app-secrets.env.template as a template. Enter the client ID and client secret for GitHub OAuth there. See [GitHub's authentication guide](https://developer.github.com/guides/basics-of-authentication/#registering-your-app) and [GitHub's OAuth apps settings page](https://github.com/settings/developers) for more information. Never add the client secret to version control or make it public by other means!
* Start the Docker containers via `/opt/elm-lang-de/bin/remote/start.sh`. This will pull the Docker containers and start them. If you get an error message that you could not connect to the Docker daemon
    * Make sure Docker is running via `systemctl status docker`,
    * Make sure you have added your username to the docker group by executing `groups`, the output should list `docker` among other groups. If not, add your username now with
        * `usermod -aG docker $(whoami)` or
        * `usermod -aG docker elmlangde`
    * Log out and log in again (this might be necessary for the group change to take effect).

The script `bin/remote/deploy.sh` is a convenience script that is intended to be executed on production, it will pull the latest app container and deploy it. It will not touch the postgres or nginx containers. `bin/deploy-app-prod.sh` is a convenience script intended to run locally that rebuilds the app container, pushes it to the registry and immediately deploys it.

Note: `/opt/elm-lang-de/bin/remote/start.sh` just starts the containers with the `docker-compose` executable. It will pull the containers that do not exist on the system but it will not update them if they are there. For updates, you'll need to pull the latest container versions explicitly. You can pull all Docker containers with `/opt/elm-lang-de/bin/container-pull-all.sh` or pull individual containers with one of the following commands:
* `docker pull basti1302/elmlangde-postgres`
* `docker pull basti1302/elmlangde-nginx`
* `docker pull basti1302/elmlangde-app`
