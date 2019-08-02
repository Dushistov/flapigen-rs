#!/bin/sh

rsync --exclude=.git/ --exclude=.git* -avu --delete book/ $1
