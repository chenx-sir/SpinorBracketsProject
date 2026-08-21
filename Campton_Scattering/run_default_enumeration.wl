scriptDirectory = DirectoryName[$InputFileName];

Get[FileNameJoin[{scriptDirectory, "enumerate_four_point_compton.wl"}]];

results = EnumerateFourPointCompton[exampleConfiguration];
summary = ComptonEnumerationSummary[results];

resultsFile = FileNameJoin[{scriptDirectory, "all_local_compton_amplitudes.wl"}];
summaryFile = FileNameJoin[{scriptDirectory, "all_local_compton_amplitudes_summary.md"}];
detailedMarkdownFile = FileNameJoin[{scriptDirectory, "all_local_compton_amplitudes.md"}];

Export[
    resultsFile,
    ToString[results, InputForm, PageWidth -> Infinity],
    "Text"
];

summaryLines = Join[
    {
        "# Four-Point Compton Enumeration",
        "",
        "Symbolic tree-level minimal-coupling amplitudes generated from `enumerate_four_point_compton.wl`.",
        "",
        "| Theory | Enumerated external-state components |",
        "| --- | ---: |"
    },
    ("| " <> ToString[#[[1]]] <> " | " <> ToString[#[[2]]] <> " |" &) /@
        Normal[summary],
    {
        "",
        "Leg order: `(massiveIn, bosonPlus, bosonMinus, massiveOut)`.",
        "The output includes QED spins 0, 1/2, 1; Yang--Mills spins 0, 1/2, 1; and gravity spins 0, 1/2, 1, 3/2, 2.",
        "The Yang--Mills example uses a two-dimensional representation and enumerates all four matter color components.",
        "",
        "Load the complete symbolic result in Mathematica with:",
        "",
        "```wl",
        "results = Get[\"all_local_compton_amplitudes.wl\"];",
        "```"
    }
];
Export[summaryFile, StringRiffle[summaryLines, "\n"], "Text"];

(* Render every symbolic component in a readable, Mathematica-reusable report. *)
ClearAll[
    comptonMarkdownCode, comptonMarkdownIndices, comptonMarkdownRecord,
    comptonMarkdownSpinSection, comptonMarkdownTheorySection,
    comptonLatexAtom, comptonAmplitudeLatex, comptonLatexIndexList
];

comptonMarkdownCode[expr_] := ToString[expr, InputForm, PageWidth -> Infinity];

(* Render project-specific spinor objects as conventional amplitude notation. *)
comptonLatexAtom[
    MixedAngle[MassiveLeg[i_, ii_], MasslessLeg[j_]]
] := "\\langle " <> ToString[i] <> "^{" <> ToString[ii] <> "} " <>
    ToString[j] <> " \\rangle";
comptonLatexAtom[
    MixedAngle[MasslessLeg[i_], MassiveLeg[j_, jj_]]
] := "\\langle " <> ToString[i] <> " " <> ToString[j] <> "^{" <>
    ToString[jj] <> "} \\rangle";
comptonLatexAtom[
    MixedSquare[MassiveLeg[i_, ii_], MasslessLeg[j_]]
] := "[" <> ToString[i] <> "^{" <> ToString[ii] <> "} " <>
    ToString[j] <> "]";
comptonLatexAtom[
    MixedSquare[MasslessLeg[i_], MassiveLeg[j_, jj_]]
] := "[" <> ToString[i] <> " " <> ToString[j] <> "^{" <>
    ToString[jj] <> "}]";
comptonLatexAtom[
    MixedChain[
        MasslessLeg[left_],
        {MassiveSpinorBrackets`mp[first_] - MassiveSpinorBrackets`mp[second_]},
        MasslessLeg[right_]
    ]
] := "\\langle " <> ToString[left] <> "\\,|\\,p_{" <>
    ToString[first] <> "}-p_{" <> ToString[second] <> "}\\,|\\," <>
    ToString[right] <> "]";
comptonLatexAtom[expr_] := "\\operatorname{unrendered}" <>
    "\\left(" <> comptonMarkdownCode[expr] <> "\\right)";

(* Let TeXForm arrange ordinary products, powers, and rational functions. *)
comptonAmplitudeLatex[expr_] := Module[
    {atoms, markers, markerTex, renderedAtoms, tex},
    atoms = DeleteDuplicates @ Cases[
        expr, _MixedAngle | _MixedSquare | _MixedChain, Infinity
    ];
    If[atoms === {}, Return[ToString[TeXForm[expr]]]];
    markers = Symbol["ComptonTexMarker" <> ToString[#]] & /@
        Range[Length[atoms]];
    markerTex = ToString[TeXForm[#]] & /@ markers;
    renderedAtoms = comptonLatexAtom /@ atoms;
    tex = ToString[TeXForm[expr /. Thread[atoms -> markers]]];
    Fold[
        StringReplace[#1, #2[[1]] -> #2[[2]]] &,
        tex,
        Transpose[{markerTex, renderedAtoms}]
    ]
];

comptonLatexIndexList[indices_List] := StringRiffle[ToString /@ indices, ","];
comptonMarkdownIndices[Automatic] := "$S=0$ (no massive little-group index)";
comptonMarkdownIndices[{inIndices_List, outIndices_List}] :=
    "$I=(" <> comptonLatexIndexList[inIndices] <> "),\\quad J=(" <>
        comptonLatexIndexList[outIndices] <> ")$";
comptonMarkdownIndices[indices_] := "$" <> comptonMarkdownCode[indices] <> "$";

comptonMarkdownRecord[record_Association, number_Integer] := Module[
    {indices, colorLine, amplitude},
    indices = comptonMarkdownIndices[record["LittleGroupIndices"]];
    colorLine = If[
        KeyExistsQ[record, "MatterIndices"],
        "- Matter color component: $(i,j)=(" <>
            comptonLatexIndexList[record["MatterIndices"]] <> ")$",
        Nothing
    ];
    amplitude = comptonAmplitudeLatex[record["Amplitude"]];
    Join[
        {
            "#### Component " <> ToString[number],
            "",
            "- Massive SU(2) little-group indices: " <> indices
        },
        If[colorLine === Nothing, {}, {colorLine}],
        {
            "- Symbolic amplitude:",
            "",
            "$$",
            amplitude,
            "$$",
            ""
        }
    ]
];

comptonMarkdownSpinSection[spinLabel_String, records_List] := Module[
    {nonzero, componentBlocks},
    nonzero = Count[records, record_Association /; record["Amplitude"] =!= 0];
    componentBlocks = Flatten[
        MapIndexed[comptonMarkdownRecord[#1, First[#2]] &, records],
        1
    ];
    Join[
        {
            "### Massive matter spin `S = " <> spinLabel <> "`",
            "",
            "Enumerated components: " <> ToString[Length[records]] <>
                "; nonzero for the configured symbolic couplings and color generators: " <>
                ToString[nonzero] <> ".",
            ""
        },
        componentBlocks
    ]
];

comptonMarkdownTheorySection[theory_String, spinAssociation_Association] := Join[
    {
        "## " <> theory,
        "",
        If[
            theory === "YangMills",
            "The configured matter representation is two-dimensional, with $T^a=\\sigma_x$ and $T^b=\\sigma_z$. All four $(i,j)$ matrix components are listed; a vanishing entry reflects this particular choice of generators.",
            ""
        ],
        ""
    },
    Flatten[
        (comptonMarkdownSpinSection[#[[1]], #[[2]]] &) /@ Normal[spinAssociation],
        1
    ]
];

detailedMarkdownLines = Join[
    {
        "# Four-Point Compton Amplitudes: Enumerated Symbolic Results",
        "",
        "This report is generated by `run_default_enumeration.wl` from the exact Wolfram Language association in `all_local_compton_amplitudes.wl`. The amplitudes below are displayed in conventional spinor-helicity notation; no numerical spinors were supplied for this run.",
        "",
        "## Scope and conventions",
        "",
        "- Tree-level, four-point, minimal-coupling Compton amplitudes with two equal-mass matter legs and two massless boson legs.",
        "- External-leg order: `(1^S, 2^{+h}, 3^{-h}, 4^S)` = `(massiveIn, bosonPlus, bosonMinus, massiveOut)`. Here `h=1` for QED and Yang--Mills, and `h=2` for gravity.",
        "- Mandelstam inputs are `{s,u,m}`; the report uses `t = 2 m^2 - s - u`. The propagator factors are `(s-m^2)(u-m^2)`.",
        "- A massless leg is denoted by its label, while `i^I` denotes massive leg `i` with SU(2) little-group index `I=1,2`. Thus `\\langle 3|p_1-p_4|2]` is the mixed momentum chain.",
        "- For matter spin `S`, each massive leg carries a symmetric rank-`2S` SU(2) tensor. The traversal lists `(2S+1)^2` independent in/out index components.",
        "- The underlying closed form is:",
        "",
        "$$\\mathcal{M}_4=\\frac{X^{2h-2S}\\prod_{r=1}^{2S}N_{I_rJ_r}}{(s-m^2)(u-m^2)}\\,K,\\qquad X=\\langle3|p_1-p_4|2],\\qquad N_{IJ}=\\langle4^J3\\rangle[1^I2]+\\langle1^I3\\rangle[4^J2].$$",
        "",
        "- The theory kernels are:",
        "",
        "$$K_{\\rm QED}=e^2q^2,\\qquad K_{\\rm YM}=\\frac{g^2}{t}\\left[(u-m^2)(T^aT^b)_{ij}+(s-m^2)(T^bT^a)_{ij}\\right],\\qquad K_{\\rm Gravity}=-\\frac{\\kappa^2}{t}.$$",
        "- Particle identity is encoded only through the permitted matter spin, mass, charge, and (for Yang--Mills) representation. For example, electrons and muons use the same QED `S=1/2` form with their own `m` and `q`.",
        "",
        "## Enumeration summary",
        "",
        "| Theory | Matter spins included | Enumerated external-state components |",
        "| --- | --- | ---: |",
        "| QED | `0, 1/2, 1` | 14 |",
        "| Yang--Mills | `0, 1/2, 1` | 56 |",
        "| Gravity | `0, 1/2, 1, 3/2, 2` | 55 |",
        "",
        "The Yang--Mills total includes all four matrix components `(i,j)` of the configured two-dimensional matter representation. For direct Mathematica evaluation, use the machine-readable file `all_local_compton_amplitudes.wl` after loading `MixedSpinorBrackets.wl`.",
        "",
        "## Component Results",
        ""
    },
    Flatten[
        (comptonMarkdownTheorySection[#[[1]], #[[2]]] &) /@ Normal[results],
        1
    ]
];
Export[detailedMarkdownFile, StringRiffle[detailedMarkdownLines, "\n"], "Text"];

Print["Wrote: ", resultsFile];
Print["Wrote: ", summaryFile];
Print["Wrote: ", detailedMarkdownFile];
Print[summary];
