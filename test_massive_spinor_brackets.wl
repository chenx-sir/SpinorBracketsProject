Get[FileNameJoin[{DirectoryName[$InputFileName], "MassiveSpinorBrackets.wl"}]];

data = <|"Lambda" -> {
    {{1, 0}, {0, 1}},
    {{1, 1}, {2, 3}}
  }, "LambdaTilde" -> {
    {{2, 0}, {0, 2}},
    {{1, 2}, {3, 5}}
  }|>;

expandedAngleChain = ToExpression[
  "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`mla[1, 1] ** " <>
    "MassiveSpinorBrackets`mp[2] ** " <>
    "MassiveSpinorBrackets`mra[3, 2]]"
];
expandedSquareChain = ToExpression[
  "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`mls[1, 1] ** " <>
    "MassiveSpinorBrackets`mp[2] ** " <>
    "MassiveSpinorBrackets`mrs[3, 2]]"
];
expandedMixedAngleSquareChain = ToExpression[
  "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`mla[1, 1] ** " <>
    "MassiveSpinorBrackets`mp[2] ** " <>
    "MassiveSpinorBrackets`mrs[3, 2]]"
];
expandedMixedSquareAngleChain = ToExpression[
  "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`mls[1, 1] ** " <>
    "MassiveSpinorBrackets`mp[2] ** " <>
    "MassiveSpinorBrackets`mra[3, 2]]"
];
expectedAngleChain = ToExpression[
  "MassiveSpinorBrackets`mab[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`msb[2, 1, 3, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`msb[2, 2, 3, 2]"
];
expectedSquareChain = ToExpression[
  "MassiveSpinorBrackets`msb[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`mab[2, 1, 3, 2] + " <>
    "MassiveSpinorBrackets`msb[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`mab[2, 2, 3, 2]"
];
expandedTwoMomentumChain = ToExpression[
  "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`mla[1, 1] ** " <>
    "MassiveSpinorBrackets`mp[2] ** " <>
    "MassiveSpinorBrackets`mp[3] ** " <>
    "MassiveSpinorBrackets`mrs[4, 2]]"
];
expectedTwoMomentumChain = ToExpression[
  "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`masb[1, 1, " <>
    "MassiveSpinorBrackets`mp[2], " <>
    "MassiveSpinorBrackets`mp[3], 4, 2]]"
];

tests = {
  VerificationTest[
    expandedAngleChain,
    expectedAngleChain,
    TestID -> "angle-angle massive chain contraction"
  ],
  VerificationTest[
    expandedSquareChain,
    expectedSquareChain,
    TestID -> "square-square massive chain contraction"
  ],
  VerificationTest[
    expandedMixedAngleSquareChain,
    expectedAngleChain,
    TestID -> "mixed angle-square massive chain contraction"
  ],
  VerificationTest[
    expandedMixedSquareAngleChain,
    expectedSquareChain,
    TestID -> "mixed square-angle massive chain contraction"
  ],
  VerificationTest[
    expandedTwoMomentumChain,
    expectedTwoMomentumChain,
    TestID -> "massive mixed chain expansion"
  ],
  VerificationTest[MassiveSpinorEvaluate[mab[1, 1, 1, 1], data], 0,
    TestID -> "same massive leg angle antisymmetry"],
  VerificationTest[MassiveSpinorEvaluate[mm[1], data], 4,
    TestID -> "mass squared from determinant"],
  VerificationTest[MassiveSpinorEvaluate[mp[1], data], {{2, 0}, {0, 2}},
    TestID -> "massive momentum decomposition"],
  VerificationTest[MassiveSpinorEvaluate[mm[1, 2], data], 51,
    TestID -> "momentum sum invariant"]
};

report = TestReport[tests];
Print["Succeeded: ", Length[report["TestsSucceeded"]]];
Print["Failed: ", Total[Length /@ Values[report["TestsFailed"]]]];
If[Length[report["TestsFailed"]] =!= 0, Exit[1]];
