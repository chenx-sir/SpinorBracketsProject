(* Sample tests for SpinorExpand in SpinorBrackets.wl *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "SpinorBrackets.wl"}]];

printCase[name_String, input_, expected_] := Module[{actual},
  actual = SpinorExpand[input];
  Print["--- ", name, " ---"];
  Print["Input:    ", ToString[input, InputForm]];
  Print["Expected: ", ToString[expected, InputForm]];
  Print["Actual:   ", ToString[actual, InputForm]];
  Print["Pass:     ", actual === expected];
];

tests = {
  VerificationTest[
    SpinorExpand[ab[1, p[2], 3]],
    ab[1, 2] sab[2, 3],
    TestID -> "angle-angle chain with one momentum"
  ],
  VerificationTest[
    SpinorExpand[asb[1, p[2], 3]],
    ab[1, 2] sb[2, 3],
    TestID -> "angle-square chain with one momentum"
  ],
  VerificationTest[
    SpinorExpand[sb[1, p[2], 3]],
    sb[1, 2] asb[2, 3],
    TestID -> "square-square chain with one momentum"
  ],
  VerificationTest[
    SpinorExpand[sab[1, p[2], 3]],
    sb[1, 2] ab[2, 3],
    TestID -> "square-angle chain with one momentum"
  ],
  VerificationTest[
    SpinorExpand[ab[1, p[2], p[3], 4]],
    ab[1, 2] sb[2, 3] ab[3, 4],
    TestID -> "angle-angle chain with two momenta"
  ],
  VerificationTest[
    SpinorExpand[asb[1, p[2, 3], 4]],
    ab[1, 2] sb[2, 4] + ab[1, 3] sb[3, 4],
    TestID -> "momentum sum expansion"
  ],
  VerificationTest[
    SpinorExpand[ab[1, x, 3]],
    ab[1, x, 3],
    TestID -> "non-momentum middle object is unchanged"
  ],
  VerificationTest[
    la[1] ** ra[2] ** la[1] ** ra[2],
    ab[1, 2]^2,
    TestID -> "adjacent angle contractions"
  ]
};

Print["SpinorExpand samples"]; 
Print["==================="]; 

printCase[
  "1. ab with one momentum",
  ab[1, p[2], 3],
  ab[1, 2] sab[2, 3]
];

printCase[
  "2. asb with one momentum",
  asb[1, p[2], 3],
  ab[1, 2] sb[2, 3]
];

printCase[
  "3. ab with two momenta",
  ab[1, p[2], p[3], 4],
  ab[1, 2] sb[2, 3] ab[3, 4]
];

printCase[
  "4. p[2,3] means p[2] + p[3]",
  asb[1, p[2, 3], 4],
  ab[1, 2] sb[2, 4] + ab[1, 3] sb[3, 4]
];

Print[""];
Print["5. Adjacent spinors contract pairwise"];
Print["Input:  ", ToString[la[1] ** ra[2] ** la[1] ** ra[2], InputForm]];
Print["Output: ", ToString[la[1] ** ra[2] ** la[1] ** ra[2], InputForm]];

Print[""];
Print["6. A repeated label gives ab[1,1] = 0"];
Print["Input:  ", ToString[
  SpinorExpand[la[1] ** p[1] ** ra[2] ** la[1] ** ra[2]],
  InputForm
]];
Print["Output: ", ToString[
  SpinorExpand[la[1] ** p[1] ** ra[2] ** la[1] ** ra[2]],
  InputForm
]];

Print[""]; 
Print["Verification summary"]; 
report = TestReport[tests];
Print["Succeeded: ", Length[report["TestsSucceeded"]]];
Print["Failed:    ", Total[Length /@ Values[report["TestsFailed"]]]];
