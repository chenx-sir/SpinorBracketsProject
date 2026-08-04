Get[FileNameJoin[{DirectoryName[$InputFileName], "SpinorBrackets.wl"}]];

tests = {
  VerificationTest[la[1] ** ra[2], ab[1, 2], TestID -> "angle contraction"],
  VerificationTest[ls[1] ** rs[2], sb[1, 2], TestID -> "square contraction"],
  VerificationTest[la[1] ** p[2, 3] ** rs[4], asb[1, p[2, 3], 4],
    TestID -> "mixed chain contraction"],
  VerificationTest[ls[1] ** p[2] ** ra[3], sab[1, p[2], 3],
    TestID -> "conjugate mixed chain contraction"],
  VerificationTest[la[1] ** p[2] ** ra[3], ab[1, p[2], 3],
    TestID -> "open angle-angle chain contraction"],
  VerificationTest[ls[1] ** p[2] ** rs[3], sb[1, p[2], 3],
    TestID -> "open square-square chain contraction"],
  VerificationTest[la[1] ** x ** ra[3], ab[1, x, 3],
    TestID -> "arbitrary angle chain contraction"],
  VerificationTest[ls[1] ** x ** rs[3], sb[1, x, 3],
    TestID -> "arbitrary square chain contraction"],
  VerificationTest[la[1] ** ra[2] ** la[1] ** ra[2], ab[1, 2]^2,
    TestID -> "adjacent angle contractions"],
  VerificationTest[
    SpinorExpand[la[1] ** p[1] ** ra[2] ** la[1] ** ra[2]],
    0,
    TestID -> "zero momentum-chain factor"
  ],
  VerificationTest[
    SpinorExpand[asb[1, p[2, 3], 4]],
    ab[1, 2] sb[2, 4] + ab[1, 3] sb[3, 4],
    TestID -> "one momentum expansion"
  ],
  VerificationTest[
    SpinorExpand[ab[1, p[2], 3]],
    ab[1, 2] (ls[2] ** ra[3]),
    TestID -> "open angle-chain expansion"
  ],
  VerificationTest[
    SpinorExpand[sb[1, p[2], 3]],
    sb[1, 2] (la[2] ** rs[3]),
    TestID -> "open square-chain expansion"
  ],
  VerificationTest[
    SpinorExpand[ab[1, p[2], p[3], 4]],
    ab[1, 2] sb[2, 3] ab[3, 4],
    TestID -> "two momentum expansion"
  ],
  VerificationTest[SpinorCanonicalize[ab[3, 1] sb[4, 2]],
    ab[1, 3] sb[2, 4], TestID -> "double antisymmetry"],
  VerificationTest[MandelstamExpand[s[1, 2]],
    ab[1, 2] sb[1, 2], TestID -> "Mandelstam convention"],
  VerificationTest[
    MomentumConserve[asb[1, p[2, 4], 3], 4],
    -asb[1, p[1, 3], 3],
    TestID -> "momentum conservation"
  ],
  VerificationTest[MomentumConserve[p[2, 4], 4], -p[1, 3],
    TestID -> "standalone momentum conservation"],
  VerificationTest[
    SpinorCanonicalize[SchoutenIdentity[Angle, 1, 2, 3, 4] /.
      SchoutenRule[Angle, 1, 2, 3, 4]],
    0,
    TestID -> "Schouten directed rule"
  ],
  VerificationTest[
    SpinorCanonicalize[BCFWShift[ab[2, 3], {1, 2}, z]],
    ab[2, 3] + z ab[1, 3],
    TestID -> "BCFW angle shift"
  ],
  VerificationTest[
    SpinorCanonicalize[BCFWShift[sb[1, 3], {1, 2}, z]],
    sb[1, 3] - z sb[2, 3],
    TestID -> "BCFW square shift"
  ],
  VerificationTest[ParityConjugate[asb[1, p[2], 3]],
    sab[1, p[2], 3], TestID -> "parity conjugation"],
  VerificationTest[
    SpinorEvaluate[ab[1, 2] sb[2, 1],
      {{{1, 0}, {0, 1}}, {{1, 0}, {0, 1}}}],
    -1,
    TestID -> "numeric bracket evaluation"
  ],
  VerificationTest[
    SpinorEvaluate[s[1, 2],
      {{{1, 0}, {0, 1}}, {{1, 0}, {0, 1}}}],
    1,
    TestID -> "numeric Mandelstam convention"
  ],
  VerificationTest[
    SpinorEvaluate[p[1, 2],
      {{{1, 0}, {0, 1}}, {{1, 0}, {0, 1}}}],
    IdentityMatrix[2],
    TestID -> "numeric momentum sum"
  ],
  VerificationTest[
    MatchQ[ToBoxes[ab[1, 2], StandardForm],
      InterpretationBox[RowBox[{"\[LeftAngleBracket]", ___,
        "\[RightAngleBracket]"}], _ab]],
    True,
    TestID -> "automatic angle-bracket display"
  ],
  VerificationTest[
    MatchQ[ToBoxes[sb[1, 2], StandardForm],
      InterpretationBox[RowBox[{"[", ___, "]"}], _sb]],
    True,
    TestID -> "automatic square-bracket display"
  ],
  VerificationTest[
    ToExpression[ToBoxes[ab[1, 2], StandardForm], StandardForm],
    ab[1, 2],
    TestID -> "formatted bracket preserves expression"
  ]
};

report = TestReport[tests];
successCount = Length[report["TestsSucceeded"]];
failureCount = Total[Length /@ Values[report["TestsFailed"]]];
Print["Succeeded: ", successCount];
Print["Failed: ", failureCount];
If[failureCount =!= 0, Exit[1]];
