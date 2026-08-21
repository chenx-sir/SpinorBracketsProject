# Four-Point Compton Scattering

Reproducible Wolfram Language enumeration of tree-level, minimal-coupling four-point Compton amplitudes with two massive matter legs and two massless boson legs. The calculation uses massive spinor-helicity variables and outputs both a display-oriented Markdown report and the exact machine-readable Wolfram Language association.

## Contents

- `enumerate_four_point_compton.wl`: standalone enumerator for QED, Yang--Mills, and gravity.
- `run_default_enumeration.wl`: default symbolic run and report generator.
- `all_local_compton_amplitudes.md`: checked-in display report with all enumerated components.
- `all_local_compton_amplitudes.wl`: checked-in exact symbolic results for Mathematica/Wolfram Language.
- `../MixedSpinorBrackets.wl`: spinor-bracket implementation supplied by the parent project.

## Reproduce

Clone the parent project, then run the default configuration:

```sh
git clone https://github.com/chenx-sir/SpinorBracketsProject.git
cd SpinorBracketsProject/Campton_Scattering
wolframscript -file run_default_enumeration.wl
```

The run overwrites these generated artifacts:

- `all_local_compton_amplitudes.wl`
- `all_local_compton_amplitudes.md`
- `all_local_compton_amplitudes_summary.md`

The default configuration enumerates QED matter spins \(S=0,\frac12,1\), Yang--Mills matter spins \(S=0,\frac12,1\) in a two-dimensional example representation, and gravity matter spins \(S=0,\frac12,1,\frac32,2\). It produces 14 QED, 56 Yang--Mills, and 55 gravity external-state components.

## Formula and conventions

For leg order \((1^S,2^{+h},3^{-h},4^S)\), the report implements

\[
\mathcal{M}_4=
\frac{X^{2h-2S}\prod_{r=1}^{2S}N_{I_rJ_r}}
     {(s-m^2)(u-m^2)}K,
\quad
X=\langle3|p_1-p_4|2],
\quad
N_{IJ}=\langle4^J3\rangle[1^I2]+\langle1^I3\rangle[4^J2].
\]

Here \(h=1\) for QED/Yang--Mills and \(h=2\) for gravity. The detailed report records the theory-dependent kernels, color components, massive SU(2) little-group components, and all resulting symbolic expressions.

## Requirements

- Wolfram Mathematica or Wolfram Engine, providing `wolframscript`.

No numerical kinematic point is needed for the checked-in symbolic results. To evaluate at a numerical spinor-helicity point, pass compatible `spinorData` to `EnumerateFourPointCompton` as documented in `enumerate_four_point_compton.wl`.
