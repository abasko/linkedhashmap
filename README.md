Persistent LinkedHashMap
========================

Haskell implementation of Java LinkedHashMap.

Underlying HashMap is based on Data.HashMap.Strict.

Two different implementations are based on Data.Sequence and Data.IntMap.Strict to keep keys in
the order of insertion.

Criterion report: https://cdn.rawgit.com/abasko/linkedhashmap/68de7332e80db673e024906321ad2e14c2baa4d1/benchmarks/report.html

