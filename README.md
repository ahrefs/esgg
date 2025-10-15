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

- `{"list":true}` - property is an array (mapped to `list`)
- `{"list":"sometimes"}` - property is either an array or single element (mapped to json with custom ocaml module wrap that will need to be provided in scope)
- `{"optional":<true|false>}` - property may be missing (mapped to `option`)
- `{"ignore":true}` - skip property altogether
- `{"fields_default_optional":true}` - any subfield may be missing (can be overriden by per-field `optional:false`)
- `{"repr":"int64"}` - override ES `type`, currently the only possible value is `"int64"` to ensure no bits are lost (by default `long` is mapped to OCaml `int`)

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

## Configuration via `_esgg`

The `_esgg` field can be added to query templates to configure code generation behavior. This field is automatically filtered out before sending queries to Elasticsearch.

Supported configuration options:

- `{"matched_queries": true}` - Include `matched_queries` field in output types even when `_name` is not explicitly present in the query template. This is useful when `_name` is defined inside query variables.

Example:

```json
{
  "_esgg": {
    "matched_queries": true
  },
  "query": $query,
  "size": 10
}
```

## Reusing shared ATD definitions

To reuse shared definitions using the `-shared <file.atd>` option, the `atd` file must have the `<esgg from="...">` annotation at the top of the file.
The value of the annotation must correspond to the OCaml module containing the shared definitions.

Example:

```
# file.atd

<esgg from="Your_ocaml_module_name">

...atd type definitions...
```

## Elasticsearch features

TODO document what is supported

Some notes follow:

### [aggregations](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations.html)

The following aggregation types are supported:

#### Metric Aggregations

- [x] [avg](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-avg-aggregation.html) - Average of numeric values
- [x] [sum](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-sum-aggregation.html) - Sum of numeric values
- [x] [min](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-min-aggregation.html) - Minimum value
- [x] [max](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-max-aggregation.html) - Maximum value
- [x] [value_count](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-valuecount-aggregation.html) - Count of values
- [x] [cardinality](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-cardinality-aggregation.html) - Approximate count of distinct values
- [x] [top_hits](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-top-hits-aggregation.html) - Top matching documents per bucket
- [ ] [stats](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-stats-aggregation.html) - Basic statistics (count, min, max, avg, sum)
- [ ] [extended_stats](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-extendedstats-aggregation.html) - Extended statistics (variance, std deviation, etc.)
- [ ] [percentiles](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-percentile-aggregation.html) - Percentile calculations
- [ ] [percentile_ranks](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-percentile-rank-aggregation.html) - Percentile ranks
- [ ] [median_absolute_deviation](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-median-absolute-deviation-aggregation.html) - MAD calculation
- [ ] [weighted_avg](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-weight-avg-aggregation.html) - Weighted average
- [ ] [geo_bounds](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-geobounds-aggregation.html) - Geographic bounding box
- [ ] [scripted_metric](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-scripted-metric-aggregation.html) - Custom scripted metrics

#### Bucket Aggregations

- [x] [terms](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-terms-aggregation.html) - Buckets by field values
- [x] [histogram](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-histogram-aggregation.html) - Fixed-size numeric interval buckets
- [x] [date_histogram](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-datehistogram-aggregation.html) - Fixed calendar/time interval buckets
- [x] [range](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-range-aggregation.html) - Numeric range buckets
- [x] [date_range](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-daterange-aggregation.html) - Date range buckets
- [x] [filter](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filter-aggregation.html) - Single filter bucket
- [x] [filters](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filters-aggregation.html) - Multiple filter buckets
- [x] [nested](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-nested-aggregation.html) - Nested document aggregation
- [x] [reverse_nested](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-reverse-nested-aggregation.html) - Reverse nested aggregation
- [x] [significant_terms](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-significantterms-aggregation.html) - Significant terms analysis
- [x] [significant_text](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-significanttext-aggregation.html) - Significant text analysis
- [ ] [auto_date_histogram](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-autodatehistogram-aggregation.html) - Automatic date histogram interval
- [ ] [ip_range](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-iprange-aggregation.html) - IP address range buckets
- [ ] [geo_distance](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-geodistance-aggregation.html) - Geographic distance buckets
- [ ] [geohash_grid](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-geohashgrid-aggregation.html) - Geohash grid buckets
- [ ] [geotile_grid](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-geotilegrid-aggregation.html) - Geotile grid buckets
- [ ] [geohex_grid](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-geohexgrid-aggregation.html) - Geohex grid buckets
- [ ] [global](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-global-aggregation.html) - Global aggregation (all documents)
- [ ] [missing](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-missing-aggregation.html) - Missing field values
- [ ] [children](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-children-aggregation.html) - Child documents aggregation
- [ ] [parent](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-parent-aggregation.html) - Parent documents aggregation
- [ ] [sampler](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-sampler-aggregation.html) - Sampler aggregation
- [ ] [diversified_sampler](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-diversified-sampler-aggregation.html) - Diversified sampler
- [ ] [composite](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-composite-aggregation.html) - Composite aggregation for pagination
- [ ] [multi_terms](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-multi-terms-aggregation.html) - Multi-field terms aggregation
- [ ] [adjacency_matrix](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-adjacency-matrix-aggregation.html) - Adjacency matrix
- [ ] [categorize_text](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-categorize-text-aggregation.html) - Text categorization
- [ ] [frequent_item_sets](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-frequent-item-sets-aggregation.html) - Frequent item sets
- [ ] [random_sampler](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-random-sampler-aggregation.html) - Random sampling

#### Pipeline Aggregations

- [x] [cumulative_sum](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-cumulative-sum-aggregation.html) - Cumulative sum across buckets
- [x] [bucket_sort](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-bucket-sort-aggregation.html) - Sort and limit buckets
- [ ] [avg_bucket](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-avg-bucket-aggregation.html) - Average of bucket values
- [ ] [max_bucket](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-max-bucket-aggregation.html) - Maximum bucket value
- [ ] [min_bucket](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-min-bucket-aggregation.html) - Minimum bucket value
- [ ] [sum_bucket](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-sum-bucket-aggregation.html) - Sum of bucket values
- [ ] [stats_bucket](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-stats-bucket-aggregation.html) - Statistics on bucket values
- [ ] [extended_stats_bucket](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-extended-stats-bucket-aggregation.html) - Extended statistics on buckets
- [ ] [percentiles_bucket](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-percentiles-bucket-aggregation.html) - Percentiles of bucket values
- [ ] [moving_avg](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-movavg-aggregation.html) - Moving average (deprecated)
- [ ] [moving_fn](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-movfn-aggregation.html) - Moving function
- [ ] [derivative](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-derivative-aggregation.html) - Derivative calculation
- [ ] [bucket_script](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-bucket-script-aggregation.html) - Custom bucket calculations
- [ ] [bucket_selector](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-bucket-selector-aggregation.html) - Filter buckets by script
- [ ] [serial_diff](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-pipeline-serialdiff-aggregation.html) - Serial differencing

#### Matrix Aggregations

- [ ] [matrix_stats](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-matrix-stats-aggregation.html) - Matrix statistics

---

#### Aggregation-specific notes

##### [filters](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filters-aggregation.html)

- [x] named
- [x] anonymous
- [x] dynamic (i.e. a variable)
- [x] partial dynamic (i.e. containing variables)
- [x] other_bucket and other_bucket_key
- [ ] other_bucket with anonymous filters (ignored, user is responsible to treat last element of result specially)

Dynamic (defined at runtime) filters are supported, as follows `{ "filters": { "filters": $x } }`.
In this case corresponding part of output will be quite untyped. `$x` is assumed to be a dictionary and result will be represented with
dictionaries. For anonymous filters (ie array of filters) use `$(x:list)`.

##### date_histogram

`key_as_string` is returned in output only when `format` is
[explicitly specified](https://www.elastic.co/guide/en/elasticsearch/reference/6.3/search-aggregations-bucket-datehistogram-aggregation.html#_keys),
to discourage fragile code.

##### range

Keyed aggregation expects explicit `key` for each range. `from`/`to` fields in response are not extracted.

##### date_range

Same as for range aggregation.

##### dynamic

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

- code generated for query application of input variables does actually compile and produce correct query when run
- atd description of output (generated from query) can indeed unserialize ES output from that actual query

## Conditions

Copyright (c) 2018 Ahrefs <github@ahrefs.com>

This project is distributed under the terms of GPL Version 2. See LICENSE file for full license text.

NB the output of esgg, i.e. the generated code, is all yours of course :)

---
