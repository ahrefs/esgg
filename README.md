ElasticSearch Guided (code) Generator
=====================================

Tests
-----

`make check` runs tests in [check/](check/) verifying
that atd description of output (generated from query) can indeed unserialize ES output from that actual query.
Tests take somewhat long time because multiple separate invocations of ocamlbuild for each test, this could be improved.
Also adding new tests is currently a bit too manual.

`make test` runs regression tests in [test/](test/) verifying
that input and output atd generated from query stays unchanged.
Once there is an expected change in generated query - it should be committed.
Tests are easy to add and fast to run.

One thing that is currently not covered by tests is that code generated for query application of input variables
does actually compile and produce correct query when run.
