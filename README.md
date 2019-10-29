# ElasticSearch Guided (code) Generator

[![Build Status](https://travis-ci.org/ahrefs/esgg.svg?branch=master)](https://travis-ci.org/ahrefs/esgg)

## Development

Install dependencies with `opam install --deps-only .`

Build with `make`

## Mapping

`esgg` takes as an input an ES [mapping](https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping.html) (schema description)
and actual query (with syntax for variables, described below). Often additional information is needed to map ES fields into proper OCaml types,
this is achieved by attaching `_meta` annotation object to the affected field (ES only supports `_meta` at root level, so these annotations make it
impossible to store extended mapping back into ES which is a pity), as follows:

```json
    "counts": {
      "_meta": {
        "optional": true
      },
      "properties": {
        "hash": {
          "type": "long",
          "_meta": { "repr": "int64" }
        },
        "value": {
          "type": "long"
        }
      }
    },
```

Supported `_meta` attributes:

* `{"list":true}` - property is an array (mapped to `list`)
* `{"list":"sometimes"}` - property is either an array or single element (mapped to json with custom ocaml module wrap that will need to be provided in scope)
* `{"optional":<true|false>}` - property may be missing (mapped to `option`)
* `{"ignore":true}` - skip property altogether
* `{"fields_default_optional":true}` - any subfield may be missing (can be overriden by per-field `optional:false`)
* `{"repr":"int64"}` - override ES `type`, currently the only possible value is `"int64"` to ensure no bits are lost (by default `long` is mapped to OCaml `int`)

### Host types mapping

Generated code allows to use application types for any fields. This is achieved by referencing specific type for each field in generated
code, instead of the primitive type from the mapping, allowing consumer of the code to map it onto custom type etc. For example the field
`hash` in example above will have type `Counts.Hash.t` in generated code. In order to compile the generated code this type must be present
in scope and mapped to something useful. Default mapping (which just maps everything to corresponding primitive types) can be generated
with `esgg reflect <mapping name> <mapping.json>`, e.g.:

```bash
esgg reflect hello_world src/mappings/hello_world.json >> src/mapping.ml
```

will generate the following, which should be edited manually as needed, e.g. by making `Hash` a module with an abstract type

```ocaml
  module Counts = struct
    module Hash = Id_(Int64_)
    module Value = Id_(Long_)
  end

```

## Variables

Syntax for variables in template json files is as follows:

  - `$var` for regular required variable
  - `$var?` for optional variable (minimal surrounding scope is conditionally expunged)
  - full form `$(var:hint)` where `hint` can be either `list` or `list?` currently

## Elasticsearch features

TODO document what is supported

Some notes follow:

### [aggregations](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations.html)

The following aggregation types are supported:

#### [filters](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filters-aggregation.html)

  - [x] named
  - [x] anonymous
  - [x] dynamic (i.e. a variable)
  - [x] partial dynamic (i.e. containing variables)
  - [x] other_bucket and other_bucket_key
  - [ ] other_bucket with anonymous filters (ignored, user is responsible to treat last element of result specially)

Dynamic (defined at runtime) filters are supported, as follows `{ "filters": { "filters": $x } }`.
In this case corresponding part of output will be quite untyped. `$x` is assumed to be a dictionary and result will be represented with
dictionaries. For anonymous filters (ie array of filters) use `$(x:list)`.

#### date_histogram

`key_as_string` is returned in output only when `format` is
[explicitly specified](https://www.elastic.co/guide/en/elasticsearch/reference/6.3/search-aggregations-bucket-datehistogram-aggregation.html#_keys),
to discourage fragile code.

#### [range](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-range-aggregation.html)

  Keyed aggregation expects explicit `key` for each range. `from`/`to` fields in response are not extracted.

#### [date_range](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-daterange-aggregation.html)

  same as for range aggregation

#### others

  - min
  - max
  - avg
  - sum
  - value_count
  - cardinality
  - terms
  - significant_terms
  - histogram
  - top_hits
  - nested
  - reverse_nested

#### dynamic

  Specifying aggregation as variable (`$var`) will lead to an untyped json in place of aggregation output, this can be used as temporary
  workaround for unsupported aggregation types or for truly dynamic usecase (aggregation built at run-time).

### [script](https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-scripting-using.html)

Scripts are opaque, ie no type information is extracted and result is json.

### [source filtering](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-request-source-filtering.html)

- [x] exclude and include
- [ ] wildcards
- [x] dynamic (i.e. a variable) NB not implemented for get and mget

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
2019-10-29
