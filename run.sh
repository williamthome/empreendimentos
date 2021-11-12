#!/bin/bash

set -e

erl -make
erl -eval "io:format(\"\")" -eval "app:start()"

exit 0
