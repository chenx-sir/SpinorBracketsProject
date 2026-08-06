BeginPackage["MassiveSpinorBrackets`"];

mab::usage = "mab[i,I,j,J] 表示 <i^I j^J>。";
msb::usage = "msb[i,I,j,J] 表示 [i_I j_J]。";
masb::usage = "masb[i,I,middle...,j,J] 表示 <i^I|...|j_J]。";
msab::usage = "msab[i,I,middle...,j,J] 表示 [i_I|...|j^J>。";
mra::usage = "mra[i,I] 表示 |i^I>；mla[i,I] 表示 <i^I|。";
mrs::usage = "mrs[i,I] 表示 |i_I]；mls[i,I] 表示 [i_I|。";
mp::usage = "mp[i] 表示有质量动量 p_i = |i^I>[i_I|；mp[i,j,...] 表示动量和。";
mm::usage = "mm[i] 是由 det(p_i) 定义的质量平方；mm[i,j,...] 是动量和的平方。";
MassiveSpinorExpand::usage = "MassiveSpinorExpand[expr] 将有质量 spinor 链中的动量和展开为二旋量括号的乘积之和。";
MassiveSpinorEvaluate::usage = "MassiveSpinorEvaluate[expr,{lambdas,lambdaTildes}] 使用给定的有质量二维 spinor 数据求值。";
MassiveKinematicsCheck::usage = "MassiveKinematicsCheck[{lambdas,lambdaTildes}] 检查有质量 spinor 数据的维数、质量平方和总动量守恒。";

Begin["`Private`"];

ClearAll[momentumSequenceQ];
momentumSequenceQ[items_List] :=
    MatchQ[items, {MassiveSpinorBrackets`mp[___] ...}];

ClearAll[spinorEndpointQ, spinorChainMiddleQ];
spinorEndpointQ[expr_] :=
    MatchQ[
        expr,
        MassiveSpinorBrackets`mla[_, _] |
        MassiveSpinorBrackets`mra[_, _] |
        MassiveSpinorBrackets`mls[_, _] |
        MassiveSpinorBrackets`mrs[_, _]
    ];
spinorChainMiddleQ[items_List] :=
    !AnyTrue[items, spinorEndpointQ];

ClearAll[scalarBracketQ, orderedProduct];
scalarBracketQ[expr_] :=
    MatchQ[
        expr,
        MassiveSpinorBrackets`mab[___] |
        MassiveSpinorBrackets`msb[___] |
        MassiveSpinorBrackets`masb[___] |
        MassiveSpinorBrackets`msab[___]
    ];
orderedProduct[items_List] := Which[
    AnyTrue[items, SameQ[#, 0] &],
        0,
    Length[items] == 0,
        1,
    Length[items] == 1,
        First[items],
    And @@ (scalarBracketQ /@ items),
        Times @@ items,
    True,
        NonCommutativeMultiply @@ items
];

mab[i_, ii_, j_, jj_] /;
    SameQ[i, j] && SameQ[ii, jj] := 0;
msb[i_, ii_, j_, jj_] /;
    SameQ[i, j] && SameQ[ii, jj] := 0;

mla /: HoldPattern[
    NonCommutativeMultiply[
        pre___,
        mla[i_, ii_],
        mra[j_, jj_],
        post___
    ]
] :=
    orderedProduct[
        {
            pre,
            mab[i, ii, j, jj],
            post
        }
    ];
mls /: HoldPattern[
    NonCommutativeMultiply[
        pre___,
        mls[i_, ii_],
        mrs[j_, jj_],
        post___
    ]
] :=
    orderedProduct[
        {
            pre,
            msb[i, ii, j, jj],
            post
        }
    ];
mla /: HoldPattern[
    NonCommutativeMultiply[
        pre___,
        mla[i_, ii_],
        mrs[j_, jj_],
        post___
    ]
] :=
    orderedProduct[
        {
            pre,
            masb[i, ii, j, jj],
            post
        }
    ];
mls /: HoldPattern[
    NonCommutativeMultiply[
        pre___,
        mls[i_, ii_],
        mra[j_, jj_],
        post___
    ]
] :=
    orderedProduct[
        {
            pre,
            msab[i, ii, j, jj],
            post
        }
    ];

mla /: HoldPattern[
    NonCommutativeMultiply[
        mla[i_, ii_],
        middle___,
        mra[j_, jj_]
    ]
] /; spinorChainMiddleQ[{middle}] :=
    mab[i, ii, middle, j, jj];
mls /: HoldPattern[
    NonCommutativeMultiply[
        mls[i_, ii_],
        middle___,
        mrs[j_, jj_]
    ]
] /; spinorChainMiddleQ[{middle}] :=
    msb[i, ii, middle, j, jj];
mla /: HoldPattern[
    NonCommutativeMultiply[
        mla[i_, ii_],
        middle___,
        mrs[j_, jj_]
    ]
] /; spinorChainMiddleQ[{middle}] :=
    masb[i, ii, middle, j, jj];
mls /: HoldPattern[
    NonCommutativeMultiply[
        mls[i_, ii_],
        middle___,
        mra[j_, jj_]
    ]
] /; spinorChainMiddleQ[{middle}] :=
    msab[i, ii, middle, j, jj];

ClearAll[angle, square, chainFactor, openFactor, expandChain];
chainFactor[angle, {{i_, ii_}, {j_, jj_}}] :=
    mab[i, ii, j, jj];
chainFactor[square, {{i_, ii_}, {j_, jj_}}] :=
    msb[i, ii, j, jj];

openFactor[angle, {{i_, ii_}, {j_, jj_}}] :=
    mla[i, ii] ** mrs[j, jj];
openFactor[square, {{i_, ii_}, {j_, jj_}}] :=
    mls[i, ii] ** mra[j, jj];

(* Each inserted massive momentum introduces one summed SU(2) index. *)
expandChain[start_, end_, {i_, ii_}, momenta_List, {j_, jj_}] := Module[
    {
        choices,
        terms,
        littleGroupTerms,
        m = Length[momenta],
        otherType,
        lastType,
        pairList
    },
    choices = List @@@ momenta;
    If[AnyTrue[choices, EmptyQ], Return[0]];
    terms = Tuples[choices];
    littleGroupTerms = Tuples[Range[2], m];
    otherType = If[start === angle, square, angle];
    Total[
        Function[labels,
            Total[
                Function[littleGroupIndices,
                    pairList = Partition[
                        Join[
                            {{i, ii}},
                            Transpose[{labels, littleGroupIndices}],
                            {{j, jj}}
                        ],
                        2,
                        1
                    ];
                    lastType = If[OddQ[m], otherType, start];
                    Times[
                        Sequence @@ MapIndexed[
                            chainFactor[
                                If[OddQ[First[#2]], start, otherType],
                                #1
                            ] &,
                            Most[pairList]
                        ],
                        If[
                            lastType === end,
                            chainFactor[lastType, Last[pairList]],
                            openFactor[lastType, Last[pairList]]
                        ]
                    ]
                ] /@ littleGroupTerms
            ]
        ] /@ terms
    ]
];

MassiveSpinorExpand[expr_] := Expand[
    expr /. {
        HoldPattern[mab[i_, ii_, middle___, j_, jj_]] /;
            Length[{middle}] > 0 && momentumSequenceQ[{middle}] :>
          expandChain[angle, angle, {i, ii}, {middle}, {j, jj}],
        HoldPattern[msb[i_, ii_, middle___, j_, jj_]] /;
            Length[{middle}] > 0 && momentumSequenceQ[{middle}] :>
          expandChain[square, square, {i, ii}, {middle}, {j, jj}],
        HoldPattern[masb[i_, ii_, middle___, j_, jj_]] /;
            Length[{middle}] > 0 && momentumSequenceQ[{middle}] :>
          expandChain[angle, square, {i, ii}, {middle}, {j, jj}],
        HoldPattern[msab[i_, ii_, middle___, j_, jj_]] /;
            Length[{middle}] > 0 && momentumSequenceQ[{middle}] :>
          expandChain[square, angle, {i, ii}, {middle}, {j, jj}]
    }
] /. HoldPattern[NonCommutativeMultiply[___, 0, ___]] :> 0;

ClearAll[det2, outer2, massiveMomentum];
det2[u_List, v_List] /; Length[u] == Length[v] == 2 :=
    Det[{u, v}];
outer2[u_List, v_List] /; Length[u] == Length[v] == 2 :=
    Outer[Times, u, v];
massiveMomentum[lambdas_List, lambdaTildes_List, i_Integer] :=
    Total[MapThread[outer2, {lambdas[[i]], lambdaTildes[[i]]}]];

MassiveSpinorEvaluate::data = "需要两组长度相同的有质量二维 spinor 列表，但收到的是 `1`。";
MassiveSpinorEvaluate[
    expr_,
    {lambdas_List, lambdaTildes_List}
] := Module[
    {n = Length[lambdas], validDimensions, expanded},
    validDimensions =
        Length[lambdaTildes] == n &&
        And @@ (
            MatchQ[#, {{_, _}, {_, _}}] & /@
                Join[lambdas, lambdaTildes]
        );
    If[
        !validDimensions,
        Message[
            MassiveSpinorEvaluate::data,
            {lambdas, lambdaTildes}
        ];
        Return[$Failed]
    ];
    expanded = MassiveSpinorExpand[expr];
    expanded /. {
        HoldPattern[mab[i_Integer, ii_Integer, j_Integer, jj_Integer]] /;
            1 <= i <= n && 1 <= j <= n &&
                1 <= ii <= 2 && 1 <= jj <= 2 :>
          det2[lambdas[[i, ii]], lambdas[[j, jj]]],
        HoldPattern[msb[i_Integer, ii_Integer, j_Integer, jj_Integer]] /;
            1 <= i <= n && 1 <= j <= n &&
                1 <= ii <= 2 && 1 <= jj <= 2 :>
          det2[lambdaTildes[[i, ii]], lambdaTildes[[j, jj]]],
        HoldPattern[mp[labels___Integer]] /;
            And @@ Thread[1 <= {labels} <= n] :>
          Total[
              massiveMomentum[lambdas, lambdaTildes, #] & /@ {labels}
          ],
        HoldPattern[mm[labels__Integer]] /;
            And @@ Thread[1 <= {labels} <= n] :>
          Det[
              Total[
                  massiveMomentum[lambdas, lambdaTildes, #] & /@
                      {labels}
              ]
          ],
        HoldPattern[mra[i_Integer, ii_Integer]] /;
            1 <= i <= n && 1 <= ii <= 2 :>
          lambdas[[i, ii]],
        HoldPattern[mla[i_Integer, ii_Integer]] /;
            1 <= i <= n && 1 <= ii <= 2 :>
          lambdas[[i, ii]],
        HoldPattern[mrs[i_Integer, ii_Integer]] /;
            1 <= i <= n && 1 <= ii <= 2 :>
          lambdaTildes[[i, ii]],
        HoldPattern[mls[i_Integer, ii_Integer]] /;
            1 <= i <= n && 1 <= ii <= 2 :>
          lambdaTildes[[i, ii]]
    }
];

MassiveSpinorEvaluate[expr_, data_Association] := With[
    {
        lambdas = Lookup[data, "Lambda", Missing["NotAvailable"]],
        lambdaTildes =
            Lookup[data, "LambdaTilde", Missing["NotAvailable"]]
    },
    If[
        ListQ[lambdas] && ListQ[lambdaTildes],
        MassiveSpinorEvaluate[expr, {lambdas, lambdaTildes}],
        Message[MassiveSpinorEvaluate::data, data];
        $Failed
    ]
];

MassiveKinematicsCheck[
    {lambdas_List, lambdaTildes_List}
] := Module[
    {sameLength, twoLittleGroupSpinors, momenta, massSquared, total},
    sameLength = Length[lambdas] == Length[lambdaTildes];
    twoLittleGroupSpinors =
        And @@ (
            MatchQ[#, {{_, _}, {_, _}}] & /@
                Join[lambdas, lambdaTildes]
        );
    If[
        !sameLength || !twoLittleGroupSpinors,
        Return[
            <|
                "ValidDimensions" -> False,
                "OnShell" -> False,
                "MomentumConserving" -> False
            |>
        ]
    ];
    momenta = Table[
        massiveMomentum[lambdas, lambdaTildes, i],
        {i, Length[lambdas]}
    ];
    massSquared = Simplify[Det /@ momenta];
    total = Total[momenta];
    <|
        "ValidDimensions" -> True,
        "OnShell" ->
            And @@ MapThread[
                TrueQ[Simplify[Det[#1] == #2]] &,
                {momenta, massSquared}
            ],
        "MassSquared" -> massSquared,
        "TotalMomentum" -> Simplify[total],
        "MomentumConserving" ->
            And @@ (TrueQ[Simplify[# == 0]] & /@ Flatten[total])
    |>
];

MassiveKinematicsCheck[data_Association] := With[
    {
        lambdas = Lookup[data, "Lambda", Missing["NotAvailable"]],
        lambdaTildes =
            Lookup[data, "LambdaTilde", Missing["NotAvailable"]]
    },
    If[
        ListQ[lambdas] && ListQ[lambdaTildes],
        MassiveKinematicsCheck[{lambdas, lambdaTildes}],
        <|
            "ValidDimensions" -> False,
            "OnShell" -> False,
            "MomentumConserving" -> False
        |>
    ]
];

End[];

Protect[mab, msb, masb, msab, mra, mla, mrs, mls, mp, mm];
EndPackage[];
