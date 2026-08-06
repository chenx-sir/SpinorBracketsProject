BeginPackage["MassiveSpinorBrackets`"];

mab::usage = "mab[i,I,j,J] 表示 <i^I j^J>。";
msb::usage = "msb[i,I,j,J] 表示 [i_I j_J]。";
masb::usage = "masb[i,I,middle...,j,J] 表示 <i^I|...|j_J]。";
msab::usage = "msab[i,I,middle...,j,J] 表示 [i_I|...|j^J>。";
mra::usage = "mra[i,I] 表示 |i^I>；mla[i,I] 表示 <i^I|。";
mrs::usage = "mrs[i,I] 表示 |i_I]；mls[i,I] 表示 [i_I|。";
mp::usage = "mp[i] 表示有质量动量 p_i = |i^I>[i_I|；mp[i,j,...] 表示动量和。";
mm::usage = "mm[i] 是由 det(p_i) 定义的质量平方；mm[i,j,...] 是动量和的平方。";
MassiveSpinorExpand::usage = "展开含 mp 的有质量 spinor 链。";
MassiveSpinorEvaluate::usage = "MassiveSpinorEvaluate[expr,data] 用有质量 spinor 数据求值。";
MassiveKinematicsCheck::usage = "检查有质量 spinor 数据的维数、质量壳和总动量守恒。";

Begin["`Private`"];

ClearAll[scalarQ, orderedProduct];
scalarQ[x_] :=
    MatchQ[
        x,
        MassiveSpinorBrackets`mab[___] |
        MassiveSpinorBrackets`msb[___] |
        MassiveSpinorBrackets`masb[___] |
        MassiveSpinorBrackets`msab[___] |
        MassiveSpinorBrackets`mm[___]
    ];
orderedProduct[x_List] := Which[
    AnyTrue[x, SameQ[#, 0] &], 0,
    x === {}, 1,
    Length[x] == 1, First[x],
    And @@ (scalarQ /@ x), Times @@ x,
    True, NonCommutativeMultiply @@ x
];

ClearAll[massiveSpinorEndpointQ, massiveChainMiddleQ];
massiveSpinorEndpointQ[expr_] :=
    MatchQ[
        expr,
        MassiveSpinorBrackets`mla[_, _] |
        MassiveSpinorBrackets`mra[_, _] |
        MassiveSpinorBrackets`mls[_, _] |
        MassiveSpinorBrackets`mrs[_, _]
    ];
massiveChainMiddleQ[items_List] :=
    Length[items] > 0 && !AnyTrue[items, massiveSpinorEndpointQ];

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
] /; massiveChainMiddleQ[{middle}] :=
    mab[i, ii, middle, j, jj];
mls /: HoldPattern[
    NonCommutativeMultiply[
        mls[i_, ii_],
        middle___,
        mrs[j_, jj_]
    ]
] /; massiveChainMiddleQ[{middle}] :=
    msb[i, ii, middle, j, jj];
mla /: HoldPattern[
    NonCommutativeMultiply[
        mla[i_, ii_],
        middle___,
        mrs[j_, jj_]
    ]
] /; massiveChainMiddleQ[{middle}] :=
    masb[i, ii, middle, j, jj];
mls /: HoldPattern[
    NonCommutativeMultiply[
        mls[i_, ii_],
        middle___,
        mra[j_, jj_]
    ]
] /; massiveChainMiddleQ[{middle}] :=
    msab[i, ii, middle, j, jj];

ClearAll[momentumTermQ, expandOneMomentum];
momentumTermQ[x_] := MatchQ[x, mp[_Integer]];

(* The first implementation expands one momentum insertion at a time.
   Momentum sums are distributed before this rule is applied. *)
MassiveSpinorExpand[expr_] := FixedPoint[
    Expand[# /. {
        HoldPattern[masb[i_, I_, mp[k_], j_, J_]] :>
            Sum[mab[i, I, k, K] msb[k, K, j, J], {K, 1, 2}],
        HoldPattern[msab[i_, I_, mp[k_], j_, J_]] :>
            Sum[msb[i, I, k, K] mab[k, K, j, J], {K, 1, 2}],
        HoldPattern[mab[i_, I_, mp[k_], j_, J_]] :>
            Sum[mab[i, I, k, K] msb[k, K, j, J], {K, 1, 2}],
        HoldPattern[msb[i_, I_, mp[k_], j_, J_]] :>
            Sum[msb[i, I, k, K] mab[k, K, j, J], {K, 1, 2}],
        HoldPattern[mp[labels__Integer]] :> Total[mp /@ {labels}]
    }] &, expr];

MassiveSpinorEvaluate[expr_, data_Association] := Module[
    {
        lam = Lookup[data, "Lambda"],
        tlam = Lookup[data, "LambdaTilde"],
        n,
        valid,
        pmat
    },
    valid = ArrayQ[lam, 3] && ArrayQ[tlam, 3] &&
        Dimensions[lam][[2 ;;]] === {2, 2} &&
        Dimensions[tlam][[2 ;;]] === {2, 2} &&
        Length[lam] == Length[tlam];
    If[!valid, Return[$Failed]];
    n = Length[lam];
    pmat[i_Integer] :=
        Sum[
            Outer[Times, lam[[i, I]], tlam[[i, I]]],
            {I, 1, 2}
        ];
    Expand[expr /. {
        HoldPattern[mab[i_Integer, I_Integer, j_Integer, J_Integer]] :>
            Det[{lam[[i, I]], lam[[j, J]]}],
        HoldPattern[msb[i_Integer, I_Integer, j_Integer, J_Integer]] :>
            Det[{tlam[[i, I]], tlam[[j, J]]}],
        HoldPattern[mp[labels__Integer]] :> Total[pmat /@ {labels}],
        HoldPattern[mm[labels__Integer]] :> Det[Total[pmat /@ {labels}]],
        HoldPattern[mra[i_Integer, I_Integer]] :> lam[[i, I]],
        HoldPattern[mla[i_Integer, I_Integer]] :> lam[[i, I]],
        HoldPattern[mrs[i_Integer, I_Integer]] :> tlam[[i, I]],
        HoldPattern[mls[i_Integer, I_Integer]] :> tlam[[i, I]]
    }]
];

MassiveKinematicsCheck[data_Association] := Module[
    {
        lam = Lookup[data, "Lambda"],
        tlam = Lookup[data, "LambdaTilde"],
        momenta,
        total
    },
    If[!ArrayQ[lam, 3] || !ArrayQ[tlam, 3] ||
        Dimensions[lam][[2 ;;]] =!= {2, 2} ||
        Dimensions[tlam][[2 ;;]] =!= {2, 2} ||
        Length[lam] =!= Length[tlam],
       Return[<|"ValidDimensions" -> False|>]
    ];
    momenta = Table[
        Sum[
            Outer[Times, lam[[i, I]], tlam[[i, I]]],
            {I, 1, 2}
        ],
        {i, Length[lam]}
    ];
    total = Total[momenta];
    <|
        "ValidDimensions" -> True,
        "MassSquared" -> (Det /@ momenta),
        "TotalMomentum" -> total,
        "MomentumConserving" ->
            TrueQ[FullSimplify[total == ConstantArray[0, {2, 2}]]]
    |>
];

End[];

Protect[mab, msb, masb, msab, mra, mla, mrs, mls, mp, mm];
EndPackage[];
