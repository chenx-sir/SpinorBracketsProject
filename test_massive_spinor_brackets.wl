Get[FileNameJoin[{DirectoryName[$InputFileName], "MassiveSpinorBrackets.wl"}]];

data = <|"Lambda" -> {
    {{1, 0}, {0, 1}},
    {{1, 1}, {2, 3}}
  }, "LambdaTilde" -> {
    {{2, 0}, {0, 2}},
    {{1, 2}, {3, 5}}
  }|>;
spinorData = {data["Lambda"], data["LambdaTilde"]};

expandedAngleChain = ToExpression[
    "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`mab[1, 1, " <>
    "MassiveSpinorBrackets`mp[2], " <>
    "MassiveSpinorBrackets`mp[3], " <>
    "4, 2]]"
];
expandedSquareChain = ToExpression[
    "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`msb[1, 1, " <>
    "MassiveSpinorBrackets`mp[2], " <>
    "MassiveSpinorBrackets`mp[3], " <>
    "4, 2]]"
];
expandedMixedAngleSquareChain = ToExpression[
    "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`masb[1, 1, " <>
    "MassiveSpinorBrackets`mp[2], " <>
    "3, 2]]"
];
expandedMixedSquareAngleChain = ToExpression[
    "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`msab[1, 1, " <>
    "MassiveSpinorBrackets`mp[2], " <>
    "3, 2]]"
];
expectedMixedAngleSquareChain = ToExpression[
  "MassiveSpinorBrackets`mab[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`msb[2, 1, 3, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`msb[2, 2, 3, 2]"
];
expectedMixedSquareAngleChain = ToExpression[
  "MassiveSpinorBrackets`msb[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`mab[2, 1, 3, 2] + " <>
    "MassiveSpinorBrackets`msb[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`mab[2, 2, 3, 2]"
];
expectedAngleChain = ToExpression[
  "MassiveSpinorBrackets`mab[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`msb[2, 1, 3, 1] " <>
    "MassiveSpinorBrackets`mab[3, 1, 4, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`msb[2, 1, 3, 2] " <>
    "MassiveSpinorBrackets`mab[3, 2, 4, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`msb[2, 2, 3, 1] " <>
    "MassiveSpinorBrackets`mab[3, 1, 4, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`msb[2, 2, 3, 2] " <>
    "MassiveSpinorBrackets`mab[3, 2, 4, 2]"
];
expectedSquareChain = ToExpression[
  "MassiveSpinorBrackets`msb[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`mab[2, 1, 3, 1] " <>
    "MassiveSpinorBrackets`msb[3, 1, 4, 2] + " <>
    "MassiveSpinorBrackets`msb[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`mab[2, 1, 3, 2] " <>
    "MassiveSpinorBrackets`msb[3, 2, 4, 2] + " <>
    "MassiveSpinorBrackets`msb[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`mab[2, 2, 3, 1] " <>
    "MassiveSpinorBrackets`msb[3, 1, 4, 2] + " <>
    "MassiveSpinorBrackets`msb[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`mab[2, 2, 3, 2] " <>
    "MassiveSpinorBrackets`msb[3, 2, 4, 2]"
];
expandedTwoMomentumChain = ToExpression[
    "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`mab[1, 1, " <>
    "MassiveSpinorBrackets`mp[2], " <>
    "MassiveSpinorBrackets`mp[3], " <>
    "4, 2]]"
];
expectedTwoMomentumChain = ToExpression[
  "MassiveSpinorBrackets`mab[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`msb[2, 1, 3, 1] " <>
    "MassiveSpinorBrackets`mab[3, 1, 4, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`msb[2, 1, 3, 2] " <>
    "MassiveSpinorBrackets`mab[3, 2, 4, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`msb[2, 2, 3, 1] " <>
    "MassiveSpinorBrackets`mab[3, 1, 4, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`msb[2, 2, 3, 2] " <>
    "MassiveSpinorBrackets`mab[3, 2, 4, 2]"
];
expandedMomentumSum = ToExpression[
  "MassiveSpinorBrackets`MassiveSpinorExpand[" <>
    "MassiveSpinorBrackets`masb[1, 1, " <>
    "MassiveSpinorBrackets`mp[2, 3], 4, 2]]"
];
expectedMomentumSum = ToExpression[
  "MassiveSpinorBrackets`mab[1, 1, 2, 1] " <>
    "MassiveSpinorBrackets`msb[2, 1, 4, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 2, 2] " <>
    "MassiveSpinorBrackets`msb[2, 2, 4, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 3, 1] " <>
    "MassiveSpinorBrackets`msb[3, 1, 4, 2] + " <>
    "MassiveSpinorBrackets`mab[1, 1, 3, 2] " <>
    "MassiveSpinorBrackets`msb[3, 2, 4, 2]"
];
invalidMixedZeroMomentum = ToExpression[
  "MassiveSpinorBrackets`mla[1, 1] ** " <>
    "MassiveSpinorBrackets`mrs[3, 2]"
];
invalidAngleOddChain = ToExpression[
  "MassiveSpinorBrackets`mla[1, 1] ** " <>
    "MassiveSpinorBrackets`mp[2] ** " <>
    "MassiveSpinorBrackets`mra[3, 2]"
];

mabBoxes = ToBoxes[
  MassiveSpinorBrackets`mab[1, 1, 2, 2],
  StandardForm
];
msbBoxes = ToBoxes[
  MassiveSpinorBrackets`msb[1, 1, 2, 2],
  StandardForm
];
masbBoxes = ToBoxes[
  MassiveSpinorBrackets`masb[
    1,
    1,
    MassiveSpinorBrackets`mp[3],
    2,
    2
  ],
  StandardForm
];
mraBoxes = ToBoxes[
  MassiveSpinorBrackets`mra[1, 1],
  StandardForm
];
mrsBoxes = ToBoxes[
  MassiveSpinorBrackets`mrs[1, 1],
  StandardForm
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
    expectedMixedAngleSquareChain,
    TestID -> "mixed angle-square massive chain contraction"
  ],
  VerificationTest[
    expandedMixedSquareAngleChain,
    expectedMixedSquareAngleChain,
    TestID -> "mixed square-angle massive chain contraction"
  ],
  VerificationTest[
    expandedTwoMomentumChain,
    expectedTwoMomentumChain,
    TestID -> "massive mixed chain expansion"
  ],
  VerificationTest[
    expandedMomentumSum,
    expectedMomentumSum,
    TestID -> "massive momentum sum expansion"
  ],
  VerificationTest[
    Head[invalidMixedZeroMomentum],
    System`NonCommutativeMultiply,
    TestID -> "invalid zero-momentum mixed chain remains open"
  ],
  VerificationTest[
    Head[invalidAngleOddChain],
    System`NonCommutativeMultiply,
    TestID -> "invalid odd angle chain remains open"
  ],
  VerificationTest[
    MatchQ[
      mabBoxes,
      InterpretationBox[
        RowBox[
          {
            "\[LeftAngleBracket]",
            SuperscriptBox[_, _],
            "\[ThinSpace]",
            SuperscriptBox[_, _],
            "\[RightAngleBracket]"
          }
        ],
        _
      ]
    ],
    True,
    TestID -> "massive angle bracket boxes"
  ],
  VerificationTest[
    MatchQ[
      msbBoxes,
      InterpretationBox[
        RowBox[
          {
            "[",
            SubscriptBox[_, _],
            "\[ThinSpace]",
            SubscriptBox[_, _],
            "]"
          }
        ],
        _
      ]
    ],
    True,
    TestID -> "massive square bracket boxes"
  ],
  VerificationTest[
    MatchQ[
      masbBoxes,
      InterpretationBox[
        RowBox[
          {
            "\[LeftAngleBracket]",
            SuperscriptBox[_, _],
            _,
            _,
            _,
            SubscriptBox[_, _],
            "]"
          }
        ],
        _
      ]
    ],
    True,
    TestID -> "massive mixed bracket boxes"
  ],
  VerificationTest[
    MatchQ[
      mraBoxes,
      InterpretationBox[
        RowBox[{"|", SuperscriptBox[_, _], "\[RightAngleBracket]"}],
        _
      ]
    ],
    True,
    TestID -> "massive angle ket boxes"
  ],
  VerificationTest[
    MatchQ[
      mrsBoxes,
      InterpretationBox[
        RowBox[{"|", SubscriptBox[_, _], "]"}],
        _
      ]
    ],
    True,
    TestID -> "massive square ket boxes"
  ],
  VerificationTest[
    Head[
      MassiveSpinorBrackets`MassiveSpinorForm[
        MassiveSpinorBrackets`mab[1, 1, 2, 2]
      ]
    ],
    TraditionalForm,
    TestID -> "massive spinor form interface"
  ],
  VerificationTest[
    {
      MassiveEpsilon[1, 1],
      MassiveEpsilon[1, 2],
      MassiveEpsilon[2, 1],
      MassiveEpsilon[2, 2]
    },
    {0, 1, -1, 0},
    TestID -> "SU(2) epsilon convention"
  ],
  VerificationTest[
    MassiveSpinorEvaluate[mab[1, 1, 1, 1], spinorData],
    0,
    TestID -> "list interface bracket evaluation"
  ],
  VerificationTest[
    MassiveSpinorEvaluate[mm[1], spinorData],
    4,
    TestID -> "mass squared from determinant"
  ],
  VerificationTest[
    MassiveSpinorEvaluate[mp[1], spinorData],
    {{2, 0}, {0, 2}},
    TestID -> "massive momentum decomposition"
  ],
  VerificationTest[
    MassiveSpinorEvaluate[mm[1, 2], spinorData],
    51,
    TestID -> "momentum sum invariant"
  ],
  VerificationTest[
    MassiveSpinorEvaluate[mm[1], data],
    4,
    TestID -> "association evaluation compatibility"
  ],
  VerificationTest[
    MassiveKinematicsCheck[spinorData],
    <|
      "ValidDimensions" -> True,
      "OnShell" -> Missing["MassesNotProvided"],
      "MassSquared" -> {4, -1},
      "TotalMomentum" -> {{9, 12}, {10, 19}},
      "MomentumConserving" -> False
    |>,
    TestID -> "list interface kinematics check"
  ],
  VerificationTest[
    MassiveKinematicsCheck[data],
    MassiveKinematicsCheck[spinorData],
    TestID -> "association kinematics compatibility"
  ],
  VerificationTest[
    MassiveKinematicsCheck[
      {
        {{{1, 0}, {0, 1}}},
        {{{1, 0}, {0, 1}}}
      },
      {1}
    ]["OnShell"],
    True,
    TestID -> "given mass shell check"
  ],
  VerificationTest[
    Quiet[
      MassiveSpinorEvaluate[mm[1], {{{1, 0}}, {{1, 0}}}],
      MassiveSpinorEvaluate::data
    ],
    $Failed,
    TestID -> "invalid evaluation data"
  ],
  VerificationTest[
    MassiveKinematicsCheck[{{{1, 0}}, {{1, 0}}}],
    <|
      "ValidDimensions" -> False,
      "OnShell" -> False,
      "MomentumConserving" -> False
    |>,
    TestID -> "invalid kinematics data"
  ]
};

report = TestReport[tests];
Print["Succeeded: ", Length[report["TestsSucceeded"]]];
Print["Failed: ", Total[Length /@ Values[report["TestsFailed"]]]];
If[Length[report["TestsFailed"]] =!= 0, Exit[1]];
