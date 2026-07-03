# floatml - IEEE-754 floats for OCaml

Add `Float16`, `Float32`, `Float64`, and `Float128` types to OCaml, with
full support for arithmetic, comparisons, and conversions to bit representation.

It also provides `AnyFloat`, which hides the specific float type but will error if mismatched sizes are used together.

This project aims at being a reference implementation of IEEE-754 floating point
types in OCaml, with a focus on correctness and completeness.

> [!CAUTION]
> This library is (i) in early development and (ii) entirely written through LLMs, as I lack the time and expertise to implement it myself. Use at your own risk!
> For accurate `Float64`, prefer OCaml's built-in `float` type. For other types, consider using 

The library is developed for use inside [Soteria](https://github.com/soteria-tools/soteria), where we need concrete reductions for floating point values, that are sound with respect to [IEEE-754 semantics](https://ieeexplore.ieee.org/document/8766229/) (and SMT-lib's [`FloatingPoint`](https://smt-lib.org/theories-FloatingPoint.shtml)).
