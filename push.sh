#!/bin/sh
hugo -D
aws s3 sync public/ s3://tanakahx.com --delete --storage-class=REDUCED_REDUNDANCY 
