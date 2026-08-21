# Four-Point Compton Enumeration

Symbolic tree-level minimal-coupling amplitudes generated from `enumerate_four_point_compton.wl`.

| Theory | Enumerated external-state components |
| --- | ---: |
| QED | 14 |
| YangMills | 56 |
| Gravity | 55 |

Leg order: `(massiveIn, bosonPlus, bosonMinus, massiveOut)`.
The output includes QED spins 0, 1/2, 1; Yang--Mills spins 0, 1/2, 1; and gravity spins 0, 1/2, 1, 3/2, 2.
The Yang--Mills example uses a two-dimensional representation and enumerates all four matter color components.

Load the complete symbolic result in Mathematica with:

```wl
results = Get["all_local_compton_amplitudes.wl"];
```