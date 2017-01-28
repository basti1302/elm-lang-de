FROM library/postgres:9.5.4

# Set commodity shell aliases
RUN echo "alias ls='ls --color=auto'" >> /root/.bashrc
RUN echo "alias ll='ls -alF'" >> /root/.bashrc

# install basic tools
RUN apt-get update && apt-get install -y \
  less \
  vim

# add initial sql file
ADD backend/sql/init_db.sql /docker-entrypoint-initdb.d/

