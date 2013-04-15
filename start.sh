#!/bin/sh
cd `dirname $0`
# use this set of options when run on production node
opts="-boot start_sasl -s reloader -s urlfetch -noshell"
# use this set of options when run interactively (erlang will provide a shell)
opts="-boot start_sasl -s reloader -s urlfetch"
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s urlfetch
