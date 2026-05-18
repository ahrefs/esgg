(* Types that appear in every Elasticsearch response (currently just _shards
   and its sub-types). Without this, every generated query's atd re-declares
   these as fresh types, so callers cannot write a single typed helper that
   operates on shard info across queries. This module bundles them so
   [Atdgen.make_abstract] can replace the inline declarations with an
   [<ocaml from="Esgg_shared">] equation in every generated atd. *)

let content = {|<esgg from="Esgg_shared">

type reason = { type_ <json name="type">: string; ?reason: string nullable }

type failure = {
  reason: reason;
  ?index: string nullable;
  ?node: string nullable;
  ?shard: int nullable;
  ?status: string nullable
}

type _shards = {
  total: int;
  successful: int;
  skipped: int;
  failed: int;
  ?failures: failure list nullable
}
|}
