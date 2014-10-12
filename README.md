Persistent LinkedHashMap
========================

Haskell implementation of Java (Concurrent)LinkedHashMap.

Underlying HashMap is based on Data.HashMap.Strict.

Two different implementations are based on Data.Sequence and Data.IntMap.Strict to keep the keys in
the order of insertion.

Criterion report: (https://github.com/abasko/linkedhashmap/blob/master/benchmarks/report.html)