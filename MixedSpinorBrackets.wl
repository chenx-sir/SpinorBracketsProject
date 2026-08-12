BeginPackage["MixedSpinorBrackets`"];

Unprotect[
    MasslessLeg, MassiveLeg, MixedAngle, MixedSquare, MixedChain,
    MixedSpinorExpand, MixedSpinorEvaluate, MixedKinematicsCheck,
    ComptonAmplitude, MixedSpinorForm
];

(* 自动加载同目录中的无质量和有质量 spinor 包。 *)
With[{packageDirectory = Quiet@Check[DirectoryName[$InputFileName], ""]},
    If[DirectoryQ[packageDirectory],
        Block[
            {$Path = DeleteDuplicates[Prepend[$Path, packageDirectory]]},
            Needs["SpinorBrackets`"];
            Needs["MassiveSpinorBrackets`"]
        ],
        Needs["SpinorBrackets`"];
        Needs["MassiveSpinorBrackets`"]
    ]
];

MasslessLeg::usage = "MasslessLeg[i] 表示编号为 i 的无质量外腿。";
MassiveLeg::usage = "MassiveLeg[i, I] 表示编号为 i 且带有 SU(2) 指标 I 的有质量外腿。";
MixedAngle::usage = "MixedAngle[leg1, leg2] 表示混合角括号。";
MixedSquare::usage = "MixedSquare[leg1, leg2] 表示混合方括号。";
MixedChain::usage = "MixedChain[MasslessLeg[a], {P1, ...}, MasslessLeg[b]] 表示 <a|P1...|b]。";
MixedSpinorExpand::usage = "MixedSpinorExpand[expr] 展开含有质量动量的混合旋量链。";
MixedSpinorEvaluate::usage = "MixedSpinorEvaluate[expr, data] 对混合、有质量和无质量旋量表达式进行数值求值。";
MixedKinematicsCheck::usage = "MixedKinematicsCheck[data] 分别调用无质量和有质量包的运动学检查。";
ComptonAmplitude::usage = "ComptonAmplitude[spin, legs, invariants, coupling, indices] 返回去除整体因子的 Compton 振幅。";
MixedSpinorForm::usage = "MixedSpinorForm[expr] 使用 TraditionalForm 显示混合有质量/无质量 spinor 表达式。";

Begin["`Private`"];

ClearAll[masslessLegQ, massiveLegQ];
masslessLegQ[expr_] := MatchQ[expr, MasslessLeg[_]];
massiveLegQ[expr_] := MatchQ[expr, MassiveLeg[_, _]];

ClearAll[determinant2, epsilon2];
determinant2[u_List, v_List] /; Length[u] == Length[v] == 2 := Det[{u, v}];
epsilon2 = {{0, 1}, {-1, 0}};

(* 将列表形式和 Association 形式的旋量数据统一起来。 *)
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

(* 数值求值直接委托给两个基础包，避免重复实现。 *)
ClearAll[delegateMasslessEvaluate, delegateMassiveEvaluate];
delegateMasslessEvaluate[expr_, data_Association] :=
    SpinorBrackets`SpinorEvaluate[
        expr,
        {Lookup[data, "Lambda"], Lookup[data, "LambdaTilde"]}
    ];
delegateMassiveEvaluate[expr_, data_Association] :=
    MassiveSpinorBrackets`MassiveSpinorEvaluate[expr, data];

(* 用二维行列式计算无质量与有质量旋量之间的混合括号。 *)
ClearAll[mixedAngleValue, mixedSquareValue];
mixedAngleValue[MasslessLeg[a_], MassiveLeg[i_, ii_], masslessData_Association, massiveData_Association] :=
    determinant2[masslessSpinor[masslessData, "Lambda", a], massiveSpinor[massiveData, "Lambda", i, ii]];
mixedAngleValue[MassiveLeg[i_, ii_], MasslessLeg[a_], masslessData_Association, massiveData_Association] :=
    determinant2[massiveSpinor[massiveData, "Lambda", i, ii], masslessSpinor[masslessData, "Lambda", a]];
mixedSquareValue[MassiveLeg[i_, ii_], MasslessLeg[a_], masslessData_Association, massiveData_Association] :=
    determinant2[massiveSpinor[massiveData, "LambdaTilde", i, ii], masslessSpinor[masslessData, "LambdaTilde", a]];
mixedSquareValue[MasslessLeg[a_], MassiveLeg[i_, ii_], masslessData_Association, massiveData_Association] :=
    determinant2[masslessSpinor[masslessData, "LambdaTilde", a], massiveSpinor[massiveData, "LambdaTilde", i, ii]];

(* 将混合链中的每个动量插入计算为二维动量矩阵。 *)
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

(* 先做矩阵乘法，再用两端的无质量旋量完成链的收缩。 *)
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

(* 对有质量动量使用 p = lambda^I tilde-lambda_I 展开 little-group 求和。 *)
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

(*----------以下代码控制混合 spinor 对象的显示格式，与计算过程无关----------*)

ClearAll[
    mixedRawCallBoxes,
    mixedInterpretedBoxes,
    mixedIndexBox,
    mixedLegBox,
    mixedMiddleBoxes,
    mixedBracketBoxes,
    mixedChainBoxes,
    mixedSpinorBoxes
];

mixedRawCallBoxes[head_String, args_List, form_] :=
    RowBox[
        {
            head,
            "[",
            RowBox[Riffle[MakeBoxes[#, form] & /@ args, ","]],
            "]"
        }
    ];

SetAttributes[mixedInterpretedBoxes, HoldRest];
mixedInterpretedBoxes[display_, expr_] :=
    InterpretationBox[display, expr];

mixedIndexBox[MasslessLeg[i_], _, form_] :=
    MakeBoxes[i, form];
mixedIndexBox[MassiveLeg[i_, ii_], position_, form_] :=
    If[
        position === Upper,
        SuperscriptBox[MakeBoxes[i, form], MakeBoxes[ii, form]],
        SubscriptBox[MakeBoxes[i, form], MakeBoxes[ii, form]]
    ];
mixedIndexBox[leg_, _, form_] := MakeBoxes[leg, form];

mixedLegBox[MasslessLeg[i_], form_] :=
    MakeBoxes[i, form];
mixedLegBox[MassiveLeg[i_, ii_], form_] :=
    SuperscriptBox[MakeBoxes[i, form], MakeBoxes[ii, form]];
mixedLegBox[leg_, form_] := MakeBoxes[leg, form];

mixedMiddleBoxes[MassiveSpinorBrackets`mp[i_], form_] :=
    SubscriptBox["p", MakeBoxes[i, form]];
mixedMiddleBoxes[SpinorBrackets`p[i_], form_] :=
    SubscriptBox["p", MakeBoxes[i, form]];
mixedMiddleBoxes[expr_Plus, form_] :=
    Module[{terms = List @@ expr, first, rest},
        first = First[terms];
        rest = Rest[terms];
        RowBox[
            Join[
                {mixedMiddleBoxes[first, form]},
                Flatten[
                    ({
                        If[
                            MatchQ[#, Times[-1, _]],
                            {"-", mixedMiddleBoxes[-#, form]},
                            {"+", mixedMiddleBoxes[#, form]}
                        ]
                    } & /@ rest)
                ]
            ]
        ]
    ];
mixedMiddleBoxes[Times[-1, expr_], form_] :=
    RowBox[{"-", mixedMiddleBoxes[expr, form]}];
mixedMiddleBoxes[Times[coefficient_, expr_], form_] :=
    RowBox[
        {
            MakeBoxes[coefficient, form],
            mixedMiddleBoxes[expr, form]
        }
    ];
mixedMiddleBoxes[expr_, form_] := MakeBoxes[expr, form];

mixedBracketBoxes[left_, right_, first_, second_, position_, form_] :=
    RowBox[
        {
            left,
            mixedIndexBox[first, position, form],
            "\[ThinSpace]",
            mixedIndexBox[second, position, form],
            right
        }
    ];

mixedChainBoxes[left_, right_, first_, middle_List, last_, form_] :=
    RowBox[
        {
            left,
            mixedLegBox[first, form],
            "|",
            RowBox[
                Riffle[
                    mixedMiddleBoxes[#, form] & /@ middle,
                    "\[CenterDot]"
                ]
            ],
            "|",
            mixedLegBox[last, form],
            right
        }
    ];

mixedSpinorBoxes[MixedAngle, {first_, second_}, form_] :=
    mixedBracketBoxes[
        "\[LeftAngleBracket]",
        "\[RightAngleBracket]",
        first,
        second,
        Upper,
        form
    ];
mixedSpinorBoxes[MixedSquare, {first_, second_}, form_] :=
    mixedBracketBoxes["[", "]", first, second, Lower, form];
mixedSpinorBoxes[
    MixedChain,
    {MasslessLeg[first_], middle_List, MasslessLeg[last_]},
    form_
] :=
    mixedChainBoxes[
        "\[LeftAngleBracket]",
        "]",
        MasslessLeg[first],
        middle,
        MasslessLeg[last],
        form
    ];
mixedSpinorBoxes[head_, args_List, form_] :=
    mixedRawCallBoxes[SymbolName[Unevaluated[head]], args, form];

MasslessLeg /: MakeBoxes[
    MasslessLeg[i_],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[MakeBoxes[i, form], MasslessLeg[i]];

MassiveLeg /: MakeBoxes[
    MassiveLeg[i_, ii_],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[
        mixedLegBox[MassiveLeg[i, ii], form],
        MassiveLeg[i, ii]
    ];

MixedAngle /: MakeBoxes[
    MixedAngle[args___],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[
        mixedSpinorBoxes[MixedAngle, {args}, form],
        MixedAngle[args]
    ];

MixedSquare /: MakeBoxes[
    MixedSquare[args___],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[
        mixedSpinorBoxes[MixedSquare, {args}, form],
        MixedSquare[args]
    ];

MixedChain /: MakeBoxes[
    MixedChain[args___],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[
        mixedSpinorBoxes[MixedChain, {args}, form],
        MixedChain[args]
    ];

MixedSpinorForm[expr_] := TraditionalForm[expr];

(* 构造 Compton 振幅中反复出现的 X 和 Y 结构。 *)
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

Protect[
    MasslessLeg, MassiveLeg, MixedAngle, MixedSquare, MixedChain,
    MixedSpinorExpand, MixedSpinorEvaluate, MixedKinematicsCheck,
    ComptonAmplitude, MixedSpinorForm
];
EndPackage[];
