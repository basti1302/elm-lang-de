#!/usr/bin/env bash
set -e
echo "$(date) new certificates have been issued" >> /var/log/cron.log
service nginx reload >> /var/log/cron.log 2>&1
echo "$(date) nginx reload command has been triggerd" >> /var/log/cron.log

