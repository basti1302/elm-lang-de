#!/usr/bin/env bash
set -xe

# A script to prepare a target system for deployment. This is intended for
# Ubuntu 16.04. Don't run this locally, it will likely mess up your system :-)

# This script requires sudo/root permissions.

# TODO Creating the elmlangde user and granting him sudo rights is not part
# of this script, this needs to be done before.
# See https://www.digitalocean.com/community/tutorials/initial-server-setup-with-ubuntu-16-04

# Install some basic packages
apt-get update
sudo apt-get install curl linux-image-extra-$(uname -r) linux-image-extra-virtual

ufw allow OpenSSH
sudo ufw allow http
sudo ufw allow https
ufw enable

# Install Docker
apt-get update
apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" | sudo tee /etc/apt/sources.list.d/docker.list
apt-get update
apt-get install -y docker-engine
systemctl status docker
usermod -aG docker $(whoami)

# Install Docker Compose
curl -L "https://github.com/docker/compose/releases/download/1.10.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

echo "!Remember to edit /etc/rc.local to have containers start up at machine startup, see readme.markdown!"

