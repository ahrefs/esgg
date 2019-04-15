# ElasticSearch Guided (code) Generator

[![Build Status](https://travis-ci.org/ahrefs/esgg.svg?branch=master)](https://travis-ci.org/ahrefs/esgg)

## Development

Install dependencies with `opam install --deps-only .`

Buld with `make`

## Variables

Syntax for variables in template json files is as follows:

  - `$var` for regular required variable
  - `$var?` for optional variable (minimal surrounding scope is conditionally expunged)
  - full form `$(var:hint)` where `hint` can be either `list` or `list?` currently

## Elasticsearch features

TODO document what is supported

Some notes follow:

### [filters aggregation](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filters-aggregation.html)

  - [x] named
  - [x] anonymous
  - [x] dynamic (i.e. a variable)
  - [x] partial dynamic (i.e. containing variables)
  - [x] other_bucket and other_bucket_key
  - [ ] other_bucket with anonymous filters (ignored, user is responsible to treat last element of result specially)

Dynamic (defined at runtime) filters are supported, as follows `{ "filters": { "filters": $x } }`.
In this case corresponding part of output will be quite untyped. `$x` is assumed to be a dictionary and result will be represented with
dictionaries. For anonymous filters (ie array of filters) use `$(x:list)`.

### date_histogram aggregation

`key_as_string` is returned in output only when `format` is
[explicitly specified](https://www.elastic.co/guide/en/elasticsearch/reference/6.3/search-aggregations-bucket-datehistogram-aggregation.html#_keys),
to discourage fragile code.

### [script](https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-scripting-using.html)

Scripts are opaque, ie no type information is extracted and result is json.

## Tests

`make test` runs regression tests in [test/](test/) verifying
that input and output atd generated from query stays unchanged.
Once there is an expected change in generated query - it should be committed.
Tests are easy to add and fast to run.

TODO tests to verify that:

  * code generated for query application of input variables does actually compile and produce correct query when run
  * atd description of output (generated from query) can indeed unserialize ES output from that actual query

## Conditions

Copyright (c) 2018 Ahrefs <github@ahrefs.com>

This project is distributed under the terms of GPL Version 2. See LICENSE file for full license text.

NB the output of esgg, i.e. the generated code, is all yours of course :)

----
2019-04-01
