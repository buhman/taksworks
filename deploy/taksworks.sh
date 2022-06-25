#!/bin/sh
cd /srv/www/taksworks
/root/taksworks

find /srv/www/taksworks -name '*.html' -exec sed -i 's/(c)/©/g' {} \;
