#!/bin/bash

set -e

erl -make
erl -eval "io:format(\"\")" -eval "app:iniciar()"

exit 0
