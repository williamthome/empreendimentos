#!/bin/bash

set -e

erl -make
erl -eval "app:main()"

exit 0
