(*
  独立的四点 Compton 振幅遍历器。

  加载 SpinorBracketsProject 中已经实现的 UnifiedComptonAmplitude，
  遍历 QED、Yang--Mills、Gravity 的允许 spin、massive little-group
  分量，以及 Yang--Mills 的 matter color 分量。
*)

packageCandidates = ExpandFileName /@ {
    FileNameJoin[
        {
            DirectoryName[$InputFileName], "..", "MixedSpinorBrackets.wl"
        }
    ],
    FileNameJoin[
        {
            DirectoryName[$InputFileName], "SpinorBracketsProject",
            "MixedSpinorBrackets.wl"
        }
    ],
    FileNameJoin[
        {
            DirectoryName[$InputFileName], "..", "SpinorBracketsProject",
            "MixedSpinorBrackets.wl"
        }
    ]
};
packageFile = SelectFirst[packageCandidates, FileExistsQ, Missing["NotFound"]];

If[
    MissingQ[packageFile],
    Print["Cannot find MixedSpinorBrackets.wl. Checked: ", packageCandidates];
    Abort[]
];
Get[packageFile];

ClearAll[
    EnumerateFourPointCompton, ComptonEnumerationSummary,
    comptonAllowedSpins, comptonLittleGroupAssignments,
    comptonColorAssignments, comptonEvaluate, comptonRecords,
    comptonTheoryRecords
];

comptonAllowedSpins["QED" | "YangMills"] := {0, 1/2, 1};
comptonAllowedSpins["Gravity"] := {0, 1/2, 1, 3/2, 2};

(* 对给定 massive spin 遍历所有独立的 SU(2) little-group 分量。 *)
comptonLittleGroupAssignments[0] := {Automatic};
comptonLittleGroupAssignments[spin_] := Module[{rank = 2 spin, states},
    If[!IntegerQ[rank] || rank < 0, Return[{}]];
    (* 对称 rank 阶 SU(2) 张量只有 rank+1 个独立分量。 *)
    states = Table[
        Join[ConstantArray[1, rank - numberOfTwos], ConstantArray[2, numberOfTwos]],
        {numberOfTwos, 0, rank}
    ];
    ({#[[1]], #[[2]]} & /@ Tuples[states, 2])
];

(*
  Yang--Mills 中没有指定 MatterIndices 时，遍历生成元矩阵表示空间的
  全部 (i,j) 分量；指定后只计算该分量。
*)
comptonColorAssignments["YangMills", internal_Association] := Module[
    {ta, tb, matterIndices, dimension, internalWithoutIndices},
    ta = Lookup[internal, "Ta", Missing["Ta"]];
    tb = Lookup[internal, "Tb", Missing["Tb"]];
    matterIndices = Lookup[internal, "MatterIndices", Automatic];
    If[
        MissingQ[ta] || MissingQ[tb] || !MatrixQ[ta] || !MatrixQ[tb] ||
            Dimensions[ta] != Dimensions[tb] ||
            Length[Dimensions[ta]] != 2 || Dimensions[ta][[1]] != Dimensions[ta][[2]],
        Return[$Failed]
    ];
    If[matterIndices =!= Automatic, Return[{internal}]];
    dimension = Dimensions[ta][[1]];
    internalWithoutIndices = KeyDrop[internal, "MatterIndices"];
    (Join[internalWithoutIndices, <|"MatterIndices" -> #|>] &) /@
        Tuples[Range[dimension], 2]
];
comptonColorAssignments["YangMills", _] := $Failed;
comptonColorAssignments[_, _] := {Automatic};

(* Optional spinorData changes the returned amplitude from symbolic to numeric. *)
comptonEvaluate[amplitude_, Automatic] := amplitude;
comptonEvaluate[amplitude_, spinorData_] :=
    MixedSpinorBrackets`MixedSpinorEvaluate[amplitude, spinorData];

(* Generate records for one theory and one allowed massive spin. *)
comptonRecords[
    theory_, spin_, legs_, invariants_, couplings_, internal_, spinorData_
] := Module[{littleGroupAssignments, colorAssignments},
    littleGroupAssignments = comptonLittleGroupAssignments[spin];
    colorAssignments = comptonColorAssignments[theory, internal];
    If[colorAssignments === $Failed, Return[$Failed]];
    Flatten[
        Function[colorAssignment,
            Function[littleGroupIndices,
                Module[{amplitude, record},
                    amplitude = MixedSpinorBrackets`UnifiedComptonAmplitude[
                        theory, spin, legs, invariants, couplings,
                        littleGroupIndices, colorAssignment
                    ];
                    record = <|
                        "Spin" -> spin,
                        "LittleGroupIndices" -> littleGroupIndices,
                        "Amplitude" -> comptonEvaluate[amplitude, spinorData]
                    |>;
                    If[
                        theory === "YangMills",
                        Join[
                            record,
                            <|
                                "MatterIndices" ->
                                    Lookup[colorAssignment, "MatterIndices"]
                            |>
                        ],
                        record
                    ]
                ]
            ] /@ littleGroupAssignments
        ] /@ colorAssignments,
        1
    ]
];

(* Read one theory configuration and group its records by massive spin. *)
comptonTheoryRecords[
    theory_, theoryConfiguration_Association, legs_, invariants_, spinorData_
] := Module[{couplings, spins, internal},
    couplings = Lookup[theoryConfiguration, "Couplings", Missing["Couplings"]];
    spins = Lookup[theoryConfiguration, "Spins", Automatic];
    internal = Lookup[theoryConfiguration, "Internal", Automatic];
    If[!AssociationQ[couplings], Return[$Failed]];
    If[spins === Automatic, spins = comptonAllowedSpins[theory]];
    If[
        !ListQ[spins] || !SubsetQ[comptonAllowedSpins[theory], spins],
        Return[$Failed]
    ];
    AssociationThread[
        ToString[#, InputForm] & /@ spins,
        comptonRecords[
            theory, #, legs, invariants, couplings, internal, spinorData
        ] & /@ spins
    ]
];

(*
  Main entry point.

  Required configuration shape:

  <|
      "Legs" -> {1, 2, 3, 4},
      "Invariants" -> {s, u, m},
      "Theories" -> <|
          "QED" -> <|"Couplings" -> <|"e" -> e, "Charge" -> q|>|>,
          "YangMills" -> <|
              "Couplings" -> <|"g" -> g|>,
              "Internal" -> <|"Ta" -> ta, "Tb" -> tb|>
          |>,
          "Gravity" -> <|"Couplings" -> <|"kappa" -> kappa|>|>
      |>
  |>

  Add "Spins" -> {...} in a theory entry to restrict the traversal.
  Supply spinorData as a second argument to evaluate every record numerically.
*)
EnumerateFourPointCompton[configuration_Association, spinorData_: Automatic] := Module[
    {legs, invariants, theories, theoryNames},
    legs = Lookup[configuration, "Legs", Missing["Legs"]];
    invariants = Lookup[configuration, "Invariants", Missing["Invariants"]];
    theories = Lookup[configuration, "Theories", Missing["Theories"]];
    If[
        !ListQ[legs] || Length[legs] != 4 ||
            !MatchQ[invariants, {_, _, _}] || !AssociationQ[theories],
        Return[$Failed]
    ];
    theoryNames = Keys[theories];
    If[
        Length[Complement[theoryNames, {"QED", "YangMills", "Gravity"}]] != 0,
        Return[$Failed]
    ];
    Association @@ Map[
        Function[rule,
            rule[[1]] -> comptonTheoryRecords[
                rule[[1]], rule[[2]], legs, invariants, spinorData
            ]
        ],
        Normal[theories]
    ]
];

(* Count the returned external-state components for each selected theory. *)
ComptonEnumerationSummary[result_Association] := Association @@ Map[
    Function[rule,
        rule[[1]] -> Total[Length /@ Values[rule[[2]]]]
    ],
    Normal[result]
];

(* Example: symbolic traversal of every locally valid case. *)
exampleConfiguration = <|
    "Legs" -> {1, 2, 3, 4},
    "Invariants" -> {s, u, m},
    "Theories" -> <|
        "QED" -> <|"Couplings" -> <|"e" -> e, "Charge" -> q|>|>,
        "YangMills" -> <|
            "Couplings" -> <|"g" -> g|>,
            "Internal" -> <|
                "Ta" -> {{0, 1}, {1, 0}},
                "Tb" -> {{1, 0}, {0, -1}}
            |>
        |>,
        "Gravity" -> <|"Couplings" -> <|"kappa" -> kappa|>|>
    |>
|>;

(*
  symbolicResults = EnumerateFourPointCompton[exampleConfiguration];
  ComptonEnumerationSummary[symbolicResults]

  For numerical evaluation, use:
  numericResults = EnumerateFourPointCompton[exampleConfiguration, spinorData];
*)
