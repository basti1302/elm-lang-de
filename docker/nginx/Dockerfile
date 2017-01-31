FROM nginx

# Create strong Diffie-Hellman Group (much secure, y'know?)
RUN openssl dhparam -out /etc/ssl/certs/dhparam.pem 2048

RUN echo "deb http://ftp.debian.org/debian jessie-backports main" >> /etc/apt/sources.list

# Set commodity shell aliases
RUN echo "alias ls='ls --color=auto'" >> /root/.bashrc
RUN echo "alias ll='ls -alF'" >> /root/.bashrc

# install basic tools
RUN apt-get update && apt-get install -y \
  less \
  vim \
  cron

# create a document root for the site
RUN mkdir -p /var/www/elm-lang.de/html
RUN chown -R nginx:nginx /var/www/elm-lang.de

# install certbot
RUN apt-get update && apt-get install -y certbot -t jessie-backports

# add letsencrypt account information (/etc/letsencrypt/accounts/...)
ADD docker/nginx/letsencrypt /etc/letsencrypt

# Install letsencrypt cert. For now, this is a manual step to be executed once
# on the running container when it is run in production to obtain cert files
# from letsencrypt, then download the cert files and recreate the Docker image
# again with these certs installed. This is really ugly, but at least it's only
# a one time thing that only needs to be done once per domain. In detail:
#  1) Edit docker/nginx/nginx.conf and comment out the whole server block for
#     https (that is, the server block that has "listen 443 ssl ..." in it.
#     Do not commit this change, this is only a temporary change.
#  2) Build the basti1302/elmlangde-nginx image with the modified settings and
#     push it to the Docker registry. We do not care about the other images for
#     now.
#  3) On the production host, pull the basti1302/elmlangde-nginx image from the
#     Docker registry.
#  4) Start the elmlangde-nginx container on the production host.
#     Access http://elm-lang.de/.well-known and
#     http://www.elm-lang.de.well-known in a browser and make sure they yield a
#     response (404, but that's okay) from your nginx.
#     We now have an nginx that serves http and in particular, serves all URLs
#     beginning with /.well-known from the file system. This is what the certbot
#     tool needs to verify that we actually own the domain for which we will
#     request certificates in a minute.
#  5) Enter the elmlangde-nginx container:
#     docker exec -it elmlangde-nginx bash
#  6) run the following command:
#     certbot certonly --webroot -w /var/www/elm-lang.de/html -d elm-lang.de -d www.elm-lang.de
#  7) Answer certbot's questions to the best of your knowledge :-)
#  8) Now the cert files should be created in /etc/letsencrypt
#  9) tar the /etc/letsencrypt folder:
#     cd /etc  && tar -czf letsencrypt.tar.gz letsencrypt
#     (copying it as a tar makes sure symbolic links stay intact)
# 10) Leave the nginx container and docker cp the tar to the Docker host:
#     docker cp elmlangde-nginx:/letsencrypt.tar.gz .
# 11) Leave the target system and scp the tar to your local system.
# 12) Untar the file and put the content into
#     <project-root>/docker/nginx/letsencrypt
#     This content must not be added to git!
#     After this, there should be directories like
#     <project-root>/docker/nginx/letsencrypt/accounts and
#     <project-root>/docker/nginx/letsencrypt/keys and so on...
# 13) Delete the tar locally and on the Docker host.
# 14) Revert your local changes to docker/nginx/nginx.conf from step (1).
# 15) Recreate the nginx Docker image, push it and deploy it to production.
# 16) You now should have https on production with a valid cert and automatic
#     cert renewal.

ADD docker/nginx/nginx.conf /etc/nginx/nginx.conf

# Install cronjob for automatic renewal, remove non-functioning cronjob from
# certbot package (see docker/nginx/crontab for details).
ADD docker/nginx/crontab /etc/cron.d/certbot-renew-cron
RUN chmod 0644 /etc/cron.d/certbot-renew-cron \
 && touch /var/log/cron.log \
 && rm -f /etc/cron.d/certbot

