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
# from letsencrypt, then download the cert files and recreate the container
# again with these certs installed. In detail:
#  1) put letsencrypt `accounts` folder into
#     <project-root>/docker/nginx/letsencrypt
#  2) create the nginx container
#  3) start nginx container on prod host
#  4) enter nginx container on prod:
#     docker exec -it elmlangde-nginx bash
#  5) run the following two commands:
#     certbot certonly --webroot -w /var/www/elm-lang.de/html -d elm-lang.de -d www.elm-lang.de
#  6) certs should be created in /etc/letsencrypt
#  7) leave the nginx container
#  8) tar the /etc/letsencrypt folder,
#     docker cp the tar to the Docker host and
#     scp it to your local system.
#     (copying it as a tar makes sure symbolic links stay intact)
#  9) put the newly generated stuff (that is, the certs) into
#     <project-root>/docker/nginx/letsencrypt
# 10) recreate the container.

ADD docker/nginx/nginx.conf /etc/nginx/nginx.conf

# Install cronjob for automatic renewal, remove non-functioning cronjob from
# certbot package (see docker/nginx/crontab for details).
ADD docker/nginx/crontab /etc/cron.d/certbot-renew-cron
RUN chmod 0644 /etc/cron.d/certbot-renew-cron \
 && touch /var/log/cron.log \
 && rm -f /etc/cron.d/certbot

