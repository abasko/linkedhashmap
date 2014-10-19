Persistent LinkedHashMap
========================

Haskell implementation of Java LinkedHashMap.

Underlying HashMap is based on Data.HashMap.Strict.

Two different implementations are based on Data.Sequence and Data.IntMap.Strict to keep keys in
the order of insertion.

Criterion report: https://cdn.rawgit.com/abasko/linkedhashmap/32739ce33711d12777b24221c311b3d8030f53a4/benchmarks/report.html

