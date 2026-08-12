BeginPackage["MixedSpinorBrackets`"];

Unprotect[
    MasslessLeg, MassiveLeg, MixedAngle, MixedSquare, MixedChain,
    MixedSpinorExpand, MixedSpinorEvaluate, MixedKinematicsCheck,
    ComptonAmplitude
];

Needs["SpinorBrackets`"];
Needs["MassiveSpinorBrackets`"];

MasslessLeg::usage = "MasslessLeg[i] labels a massless external leg.";
MassiveLeg::usage = "MassiveLeg[i,I] labels a massive external leg and its SU(2) index.";
MixedAngle::usage = "MixedAngle[leg1,leg2] represents a mixed angle bracket.";
MixedSquare::usage = "MixedSquare[leg1,leg2] represents a mixed square bracket.";
MixedChain::usage = "MixedChain[MasslessLeg[a],{P1,...},MasslessLeg[b]] represents <a|P1...|b].";
MixedSpinorExpand::usage = "MixedSpinorExpand[expr] expands mixed chains with massive momenta.";
MixedSpinorEvaluate::usage = "MixedSpinorEvaluate[expr,data] evaluates mixed, massless, and massive spinor expressions.";
MixedKinematicsCheck::usage = "MixedKinematicsCheck[data] delegates massless and massive checks.";
ComptonAmplitude::usage = "ComptonAmplitude[spin,legs,invariants,coupling,indices] returns a stripped Compton amplitude.";

Begin["`Private`"];

ClearAll[masslessLegQ, massiveLegQ];
masslessLegQ[expr_] := MatchQ[expr, MasslessLeg[_]];
massiveLegQ[expr_] := MatchQ[expr, MassiveLeg[_, _]];

ClearAll[determinant2, epsilon2];
determinant2[u_List, v_List] /; Length[u] == Length[v] == 2 := Det[{u, v}];
epsilon2 = {{0, 1}, {-1, 0}};

ClearAll[normalizeSpinorData, mixedDataParts];
normalizeSpinorData[data : {_, _}] := <|"Lambda" -> data[[1]], "LambdaTilde" -> data[[2]]|>;
normalizeSpinorData[data_Association] := data;
normalizeSpinorData[_] := $Failed;

mixedDataParts[data_Association] := Module[{massless, massive},
    massless = normalizeSpinorData[Lookup[data, "Massless", Missing["MasslessData"]]];
    massive = normalizeSpinorData[Lookup[data, "Massive", Missing["MassiveData"]]];
    If[AssociationQ[massless] && AssociationQ[massive], {massless, massive}, $Failed]
];
mixedDataParts[_] := $Failed;

ClearAll[masslessSpinor, massiveSpinor];
masslessSpinor[data_Association, key_String, i_Integer] := Lookup[data, key][[i]];
massiveSpinor[data_Association, key_String, i_Integer, ii_Integer] := Lookup[data, key][[i, ii]];

ClearAll[delegateMasslessEvaluate, delegateMassiveEvaluate];
delegateMasslessEvaluate[expr_, data_Association] :=
    SpinorBrackets`SpinorEvaluate[
        expr,
        {Lookup[data, "Lambda"], Lookup[data, "LambdaTilde"]}
    ];
delegateMassiveEvaluate[expr_, data_Association] :=
    MassiveSpinorBrackets`MassiveSpinorEvaluate[expr, data];

ClearAll[mixedAngleValue, mixedSquareValue];
mixedAngleValue[MasslessLeg[a_], MassiveLeg[i_, ii_], masslessData_Association, massiveData_Association] :=
    determinant2[masslessSpinor[masslessData, "Lambda", a], massiveSpinor[massiveData, "Lambda", i, ii]];
mixedAngleValue[MassiveLeg[i_, ii_], MasslessLeg[a_], masslessData_Association, massiveData_Association] :=
    determinant2[massiveSpinor[massiveData, "Lambda", i, ii], masslessSpinor[masslessData, "Lambda", a]];
mixedSquareValue[MassiveLeg[i_, ii_], MasslessLeg[a_], masslessData_Association, massiveData_Association] :=
    determinant2[massiveSpinor[massiveData, "LambdaTilde", i, ii], masslessSpinor[masslessData, "LambdaTilde", a]];
mixedSquareValue[MasslessLeg[a_], MassiveLeg[i_, ii_], masslessData_Association, massiveData_Association] :=
    determinant2[masslessSpinor[masslessData, "LambdaTilde", a], massiveSpinor[massiveData, "LambdaTilde", i, ii]];

ClearAll[mixedMatrixValue];
mixedMatrixValue[MassiveSpinorBrackets`mp[labels___Integer], masslessData_Association, massiveData_Association] :=
    MassiveSpinorBrackets`MassiveSpinorEvaluate[MassiveSpinorBrackets`mp[labels], massiveData];
mixedMatrixValue[SpinorBrackets`p[labels___Integer], masslessData_Association, massiveData_Association] :=
    delegateMasslessEvaluate[SpinorBrackets`p[labels], masslessData];
mixedMatrixValue[expr_Plus, masslessData_Association, massiveData_Association] :=
    Total[mixedMatrixValue[#, masslessData, massiveData] & /@ List @@ expr];
mixedMatrixValue[
    c_ * MassiveSpinorBrackets`mp[labels___Integer],
    masslessData_Association,
    massiveData_Association
] := c * mixedMatrixValue[
    MassiveSpinorBrackets`mp[labels], masslessData, massiveData
];

ClearAll[mixedChainValue];
mixedChainValue[MasslessLeg[a_], middle_List, MasslessLeg[b_], masslessData_Association, massiveData_Association] /; Length[middle] > 0 := Module[
    {matrices, product, left, right},
    matrices = mixedMatrixValue[#, masslessData, massiveData] & /@ middle;
    If[!And @@ (MatrixQ[#] && Dimensions[#] === {2, 2} & /@ matrices), Return[$Failed]];
    product = Dot @@ matrices;
    left = masslessSpinor[masslessData, "Lambda", a];
    right = masslessSpinor[masslessData, "LambdaTilde", b];
    left . epsilon2 . product . epsilon2 . right
];

ClearAll[mixedSpinorExpandOne];
mixedSpinorExpandOne[MixedChain[MasslessLeg[a_], {MassiveSpinorBrackets`mp[i_]}, MasslessLeg[b_]]] :=
    Sum[MixedAngle[MasslessLeg[a], MassiveLeg[i, ii]] * MixedSquare[MassiveLeg[i, ii], MasslessLeg[b]], {ii, 1, 2}];

mixedSpinorExpandOne[
    MixedChain[
        MasslessLeg[a_],
        {SpinorBrackets`p[i_]},
        MasslessLeg[b_]
    ]
] :=
    SpinorBrackets`ab[a, i] SpinorBrackets`sb[i, b];

MixedSpinorExpand[expr_] := Expand[expr /. {
    HoldPattern[
        MixedChain[
            MasslessLeg[a_],
            {MassiveSpinorBrackets`mp[i_]},
            MasslessLeg[b_]
        ]
    ] :>
        mixedSpinorExpandOne[
            MixedChain[
                MasslessLeg[a],
                {MassiveSpinorBrackets`mp[i]},
                MasslessLeg[b]
            ]
        ],
    HoldPattern[
        MixedChain[
            MasslessLeg[a_],
            {SpinorBrackets`p[i_]},
            MasslessLeg[b_]
        ]
    ] :>
        mixedSpinorExpandOne[
            MixedChain[
                MasslessLeg[a],
                {SpinorBrackets`p[i]},
                MasslessLeg[b]
            ]
        ]
}];

MixedSpinorEvaluate::data =
    "需要包含 Massless 和 Massive 两部分的 spinor 数据，但收到的是 `1`。";
MixedSpinorEvaluate[expr_, data_Association] := Module[
    {parts, masslessData, massiveData},
    parts = mixedDataParts[data];
    If[parts === $Failed,
        Message[MixedSpinorEvaluate::data, data];
        Return[$Failed]
    ];
    {masslessData, massiveData} = parts;
    Expand[MixedSpinorExpand[expr] /. {
        HoldPattern[MixedAngle[first_, second_]] /;
            masslessLegQ[first] && massiveLegQ[second] :>
                mixedAngleValue[first, second, masslessData, massiveData],
        HoldPattern[MixedAngle[first_, second_]] /;
            massiveLegQ[first] && masslessLegQ[second] :>
                mixedAngleValue[first, second, masslessData, massiveData],
        HoldPattern[MixedSquare[first_, second_]] /;
            masslessLegQ[first] && massiveLegQ[second] :>
                mixedSquareValue[first, second, masslessData, massiveData],
        HoldPattern[MixedSquare[first_, second_]] /;
            massiveLegQ[first] && masslessLegQ[second] :>
                mixedSquareValue[first, second, masslessData, massiveData],
        HoldPattern[MixedChain[MasslessLeg[a_], middle_List, MasslessLeg[b_]]] :>
            mixedChainValue[
                MasslessLeg[a], middle, MasslessLeg[b],
                masslessData, massiveData
            ],
        HoldPattern[SpinorBrackets`ab[args___]] :>
            delegateMasslessEvaluate[SpinorBrackets`ab[args], masslessData],
        HoldPattern[SpinorBrackets`sb[args___]] :>
            delegateMasslessEvaluate[SpinorBrackets`sb[args], masslessData],
        HoldPattern[SpinorBrackets`p[args___]] :>
            delegateMasslessEvaluate[SpinorBrackets`p[args], masslessData],
        HoldPattern[MassiveSpinorBrackets`mab[args___]] :>
            delegateMassiveEvaluate[MassiveSpinorBrackets`mab[args], massiveData],
        HoldPattern[MassiveSpinorBrackets`msb[args___]] :>
            delegateMassiveEvaluate[MassiveSpinorBrackets`msb[args], massiveData],
        HoldPattern[MassiveSpinorBrackets`mp[args___]] :>
            delegateMassiveEvaluate[MassiveSpinorBrackets`mp[args], massiveData]
    }]
];
MixedSpinorEvaluate[expr_, data : {_, _}] :=
    MixedSpinorEvaluate[
        expr,
        <|"Massless" -> data[[1]], "Massive" -> data[[2]]|>
    ];

MixedKinematicsCheck[data_Association] := Module[{parts},
    parts = mixedDataParts[data];
    If[parts === $Failed,
        Message[MixedSpinorEvaluate::data, data];
        Return[$Failed]
    ];
    <|
        "Massless" ->
            SpinorBrackets`SpinorKinematicsCheck[parts[[1]]],
        "Massive" ->
            MassiveSpinorBrackets`MassiveKinematicsCheck[parts[[2]]]
    |>
];
MixedKinematicsCheck[data : {_, _}] :=
    MixedKinematicsCheck[
        <|"Massless" -> data[[1]], "Massive" -> data[[2]]|>
    ];

ClearAll[comptonY, comptonX];
comptonY[massiveIn_, photonMinus_, photonPlus_, massiveOut_, inIndex_, outIndex_] :=
    MixedAngle[MasslessLeg[photonMinus], MassiveLeg[massiveIn, inIndex]] *
    MixedSquare[MassiveLeg[massiveOut, outIndex], MasslessLeg[photonPlus]] +
    MixedAngle[MasslessLeg[photonMinus], MassiveLeg[massiveOut, outIndex]] *
    MixedSquare[MassiveLeg[massiveIn, inIndex], MasslessLeg[photonPlus]];
comptonX[massiveIn_, photonMinus_, photonPlus_, massiveOut_] :=
    MixedChain[
        MasslessLeg[photonMinus],
        {
            MassiveSpinorBrackets`mp[massiveIn] -
                MassiveSpinorBrackets`mp[massiveOut]
        },
        MasslessLeg[photonPlus]
    ];

ComptonAmplitude[
    0,
    {massiveIn_, photonMinus_, photonPlus_, massiveOut_},
    {s_, u_, mass_},
    coupling_
] := coupling^2 comptonX[
    massiveIn, photonMinus, photonPlus, massiveOut
]^2 / ((s - mass^2) (u - mass^2));

ComptonAmplitude[
    1/2,
    {massiveIn_, photonMinus_, photonPlus_, massiveOut_},
    {s_, u_, mass_},
    coupling_,
    {inIndex_, outIndex_}
] := coupling^2 comptonX[
    massiveIn, photonMinus, photonPlus, massiveOut
] * comptonY[
    massiveIn, photonMinus, photonPlus, massiveOut,
    inIndex, outIndex
] / ((s - mass^2) (u - mass^2));

ComptonAmplitude[
    1,
    {massiveIn_, photonMinus_, photonPlus_, massiveOut_},
    {s_, u_, mass_},
    coupling_,
    {{inIndex1_, inIndex2_}, {outIndex1_, outIndex2_}}
] := coupling^2 comptonY[
    massiveIn, photonMinus, photonPlus, massiveOut,
    inIndex1, outIndex1
] * comptonY[
    massiveIn, photonMinus, photonPlus, massiveOut,
    inIndex2, outIndex2
] / ((s - mass^2) (u - mass^2));

End[];

Protect[MasslessLeg, MassiveLeg, MixedAngle, MixedSquare, MixedChain, MixedSpinorExpand, MixedSpinorEvaluate, MixedKinematicsCheck, ComptonAmplitude];
EndPackage[];
