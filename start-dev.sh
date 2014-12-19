#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname ticktick_dev \
    -s ticktick \
    -s reloader
