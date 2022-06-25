#!/bin/sh
cd /srv/www/taksworks
/root/taksworks
sed -i 's/(c)/©/g' /srv/www/taksworks/index.html
