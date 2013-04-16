urlfetch
========

urlfetch is a fork of original urlfetch project [1]. It was re-written using
webmachine [2] to avoid socket implementation between server and a client.

Asynchronous URL Fetch Service
==============================

This project aims at providing a URL Fetch service for web application
developers to make concurrent asynchronous HTTP requests. The service is
entirely written in Erlang and so, it leverages the robust and scalable
Erlang/OTP platform.

Copyright and License
---------------------

This software is released under the Apache License, Version 2.0. You may obtain
a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

The `urlfetch_async.erl, urlfetch_cache.erl, utlfetch_http.erl,
urlfetch_uuid.erl` were part of original urlfetch project, see [1], and further
extended and adopted for this implementation.

Requirements
------------

In order to run the URL Fetch service, Erlang/OTP R14A or higher is required.
Rebar [3] build tool to build urlfetch.

Credits
-------

Original idea, design and implementation belongs to Tobias Rodaebel, see
Copyright and License notice above.

The code has been extended by Valentin Kuznetsov to cover the following topics:
- replace `urlfetch_cache` implementation with `gen_server` behavior
- add support of X509 authentication with remove services
- replace original packaging with rebar (ability to create release distribution)
- drop off socket implementation in favor of standard HTTP access
  - code has been re-written to use webmachine as HTTP server back-end

Installation
------------

1. run make
2. start.sh

you may tweak start.sh options for your server needs.

References
----------

1. https://code.google.com/p/urlfetch/
2. https://github.com/basho/webmachine/wiki
3. https://github.com/basho/rebar
