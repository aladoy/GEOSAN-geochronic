#!/bin/bash


#!/bin/sh
DATABASE='geosan'
USERNAME='aladoy'
HOSTNAME='localhost'

psql -h $HOSTNAME -U $USERNAME $DATABASE << EOF
CREATE SCHEMA if not exists geochronic;
EOF



