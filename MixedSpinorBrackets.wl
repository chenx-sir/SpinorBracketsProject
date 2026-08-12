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
(* 判断一个腿标签是否表示无质量外腿。 *)
masslessLegQ[expr_] := MatchQ[expr, MasslessLeg[_]];
(* 判断一个腿标签是否表示有质量外腿。 *)
massiveLegQ[expr_] := MatchQ[expr, MassiveLeg[_, _]];

ClearAll[determinant2, epsilon2];
(* 计算两个二维旋量的行列式。 *)
determinant2[u_List, v_List] /; Length[u] == Length[v] == 2 := Det[{u, v}];
epsilon2 = {{0, 1}, {-1, 0}};

(* 将列表形式和 Association 形式的旋量数据统一起来。 *)
ClearAll[normalizeSpinorData, mixedDataParts];
(* 将 {Lambda, LambdaTilde} 列表转换为带键的 Association。 *)
normalizeSpinorData[data : {_, _}] := <|"Lambda" -> data[[1]], "LambdaTilde" -> data[[2]]|>;
(* 已经是 Association 的数据无需转换。 *)
normalizeSpinorData[data_Association] := data;
(* 不支持的数据格式统一标记为失败。 *)
normalizeSpinorData[_] := $Failed;

(* 从混合数据中提取并规范化无质量和有质量两部分。 *)
mixedDataParts[data_Association] := Module[{massless, massive},
    massless = normalizeSpinorData[Lookup[data, "Massless", Missing["MasslessData"]]];
    massive = normalizeSpinorData[Lookup[data, "Massive", Missing["MassiveData"]]];
    If[AssociationQ[massless] && AssociationQ[massive], {massless, massive}, $Failed]
];
(* 非 Association 输入无法拆分为无质量和有质量数据。 *)
mixedDataParts[_] := $Failed;

ClearAll[masslessSpinor, massiveSpinor];
(* 读取第 i 条无质量外腿的 Lambda 或 LambdaTilde。 *)
masslessSpinor[data_Association, key_String, i_Integer] := Lookup[data, key][[i]];
(* 读取第 i 条有质量外腿的第 ii 个 little-group 旋量。 *)
massiveSpinor[data_Association, key_String, i_Integer, ii_Integer] := Lookup[data, key][[i, ii]];

(* 数值求值直接委托给两个基础包，避免重复实现。 *)
ClearAll[delegateMasslessEvaluate, delegateMassiveEvaluate];
(* 调用无质量包的数值求值函数。 *)
delegateMasslessEvaluate[expr_, data_Association] :=
    SpinorBrackets`SpinorEvaluate[
        expr,
        {Lookup[data, "Lambda"], Lookup[data, "LambdaTilde"]}
    ];
(* 调用有质量包的数值求值函数。 *)
delegateMassiveEvaluate[expr_, data_Association] :=
    MassiveSpinorBrackets`MassiveSpinorEvaluate[expr, data];

(* 用二维行列式计算无质量与有质量旋量之间的混合括号。 *)
ClearAll[mixedAngleValue, mixedSquareValue];
(* 计算无质量角旋量与有质量角旋量的混合角括号。 *)
mixedAngleValue[MasslessLeg[a_], MassiveLeg[i_, ii_], masslessData_Association, massiveData_Association] :=
    determinant2[masslessSpinor[masslessData, "Lambda", a], massiveSpinor[massiveData, "Lambda", i, ii]];
(* 计算有质量角旋量与无质量角旋量的混合角括号。 *)
mixedAngleValue[MassiveLeg[i_, ii_], MasslessLeg[a_], masslessData_Association, massiveData_Association] :=
    determinant2[massiveSpinor[massiveData, "Lambda", i, ii], masslessSpinor[masslessData, "Lambda", a]];
(* 计算有质量与无质量角旋量的混合方括号。 *)
mixedSquareValue[MassiveLeg[i_, ii_], MasslessLeg[a_], masslessData_Association, massiveData_Association] :=
    determinant2[massiveSpinor[massiveData, "LambdaTilde", i, ii], masslessSpinor[masslessData, "LambdaTilde", a]];
(* 计算无质量与有质量方旋量的混合方括号。 *)
mixedSquareValue[MasslessLeg[a_], MassiveLeg[i_, ii_], masslessData_Association, massiveData_Association] :=
    determinant2[masslessSpinor[masslessData, "LambdaTilde", a], massiveSpinor[massiveData, "LambdaTilde", i, ii]];

(* 将混合链中的每个动量插入计算为二维动量矩阵。 *)
ClearAll[mixedMatrixValue];
(* 将有质量动量矩阵交给有质量包计算。 *)
mixedMatrixValue[MassiveSpinorBrackets`mp[labels___Integer], masslessData_Association, massiveData_Association] :=
    MassiveSpinorBrackets`MassiveSpinorEvaluate[MassiveSpinorBrackets`mp[labels], massiveData];
(* 将无质量动量矩阵交给无质量包计算。 *)
mixedMatrixValue[SpinorBrackets`p[labels___Integer], masslessData_Association, massiveData_Association] :=
    delegateMasslessEvaluate[SpinorBrackets`p[labels], masslessData];
(* 对动量和逐项计算并相加。 *)
mixedMatrixValue[expr_Plus, masslessData_Association, massiveData_Association] :=
    Total[mixedMatrixValue[#, masslessData, massiveData] & /@ List @@ expr];
(* 提取有质量动量前的标量系数。 *)
mixedMatrixValue[
    c_ * MassiveSpinorBrackets`mp[labels___Integer],
    masslessData_Association,
    massiveData_Association
] := c * mixedMatrixValue[
    MassiveSpinorBrackets`mp[labels], masslessData, massiveData
];

(* 先做矩阵乘法，再用两端的无质量旋量完成链的收缩。 *)
ClearAll[mixedChainValue];
(* 计算两端无质量旋量之间的混合矩阵链。 *)
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
(* 展开含一个有质量动量的混合链。 *)
mixedSpinorExpandOne[MixedChain[MasslessLeg[a_], {MassiveSpinorBrackets`mp[i_]}, MasslessLeg[b_]]] :=
    Sum[MixedAngle[MasslessLeg[a], MassiveLeg[i, ii]] * MixedSquare[MassiveLeg[i, ii], MasslessLeg[b]], {ii, 1, 2}];

(* 展开含一个无质量动量的混合链。 *)
mixedSpinorExpandOne[
    MixedChain[
        MasslessLeg[a_],
        {SpinorBrackets`p[i_]},
        MasslessLeg[b_]
    ]
] :=
    SpinorBrackets`ab[a, i] SpinorBrackets`sb[i, b];

(* 展开表达式中的混合动量链。 *)
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
(* 使用混合旋量数据对混合表达式以及两个基础包对象求值。 *)
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
(* 接受 {MasslessData, MassiveData} 的简写数据格式。 *)
MixedSpinorEvaluate[expr_, data : {_, _}] :=
    MixedSpinorEvaluate[
        expr,
        <|"Massless" -> data[[1]], "Massive" -> data[[2]]|>
    ];

(* 分别调用两个基础包的运动学检查。 *)
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
(* 接受列表形式混合数据的运动学检查简写。 *)
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

(* 为未识别的内部对象生成原始函数调用框。 *)
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
(* 创建可显示且仍可还原为原表达式的解释框。 *)
mixedInterpretedBoxes[display_, expr_] :=
    InterpretationBox[display, expr];

(* 无质量腿没有 little-group 指标，直接显示腿编号。 *)
mixedIndexBox[MasslessLeg[i_], _, form_] :=
    MakeBoxes[i, form];
(* 根据有质量腿的指标位置生成上标或下标框。 *)
mixedIndexBox[MassiveLeg[i_, ii_], position_, form_] :=
    If[
        position === Upper,
        SuperscriptBox[MakeBoxes[i, form], MakeBoxes[ii, form]],
        SubscriptBox[MakeBoxes[i, form], MakeBoxes[ii, form]]
    ];
(* 对未知腿标签退回普通显示。 *)
mixedIndexBox[leg_, _, form_] := MakeBoxes[leg, form];

(* 生成腿标签的显示框。 *)
mixedLegBox[MasslessLeg[i_], form_] :=
    MakeBoxes[i, form];
(* 为有质量腿标签生成带 little-group 上标的显示框。 *)
mixedLegBox[MassiveLeg[i_, ii_], form_] :=
    SuperscriptBox[MakeBoxes[i, form], MakeBoxes[ii, form]];
(* 对未知腿标签退回普通显示。 *)
mixedLegBox[leg_, form_] := MakeBoxes[leg, form];

(* 将有质量动量显示为带下标的 p。 *)
mixedMiddleBoxes[MassiveSpinorBrackets`mp[i_], form_] :=
    SubscriptBox["p", MakeBoxes[i, form]];
(* 将无质量动量显示为带下标的 p。 *)
mixedMiddleBoxes[SpinorBrackets`p[i_], form_] :=
    SubscriptBox["p", MakeBoxes[i, form]];
(* 递归生成动量和的显示框。 *)
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
(* 处理带负号的动量显示。 *)
mixedMiddleBoxes[Times[-1, expr_], form_] :=
    RowBox[{"-", mixedMiddleBoxes[expr, form]}];
mixedMiddleBoxes[Times[coefficient_, expr_], form_] :=
    RowBox[
        {
            MakeBoxes[coefficient, form],
            mixedMiddleBoxes[expr, form]
        }
    ];
(* 对未知中间对象退回普通显示。 *)
mixedMiddleBoxes[expr_, form_] := MakeBoxes[expr, form];

(* 生成混合括号的显示框。 *)
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

(* 生成混合动量链的显示框。 *)
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

(* 根据混合对象类型选择相应的显示框。 *)
mixedSpinorBoxes[MixedAngle, {first_, second_}, form_] :=
    mixedBracketBoxes[
        "\[LeftAngleBracket]",
        "\[RightAngleBracket]",
        first,
        second,
        Upper,
        form
    ];
(* 为 MixedSquare 选择方括号显示形式。 *)
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
(* 对未知混合对象退回原始函数调用显示。 *)
mixedSpinorBoxes[head_, args_List, form_] :=
    mixedRawCallBoxes[SymbolName[Unevaluated[head]], args, form];

(* 为无质量腿定义可解释的显示框。 *)
MasslessLeg /: MakeBoxes[
    MasslessLeg[i_],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[MakeBoxes[i, form], MasslessLeg[i]];

(* 为有质量腿定义带 little-group 上标的显示框。 *)
MassiveLeg /: MakeBoxes[
    MassiveLeg[i_, ii_],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[
        mixedLegBox[MassiveLeg[i, ii], form],
        MassiveLeg[i, ii]
    ];

(* 为 MixedAngle 定义可解释的显示格式。 *)
MixedAngle /: MakeBoxes[
    MixedAngle[args___],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[
        mixedSpinorBoxes[MixedAngle, {args}, form],
        MixedAngle[args]
    ];

(* 为 MixedSquare 定义可解释的显示格式。 *)
MixedSquare /: MakeBoxes[
    MixedSquare[args___],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[
        mixedSpinorBoxes[MixedSquare, {args}, form],
        MixedSquare[args]
    ];

(* 为 MixedChain 定义可解释的显示格式。 *)
MixedChain /: MakeBoxes[
    MixedChain[args___],
    form : (StandardForm | TraditionalForm)
] :=
    mixedInterpretedBoxes[
        mixedSpinorBoxes[MixedChain, {args}, form],
        MixedChain[args]
    ];

(* 将混合 spinor 表达式转换为 TraditionalForm。 *)
MixedSpinorForm[expr_] := TraditionalForm[expr];
(* 构造 Compton 振幅中重复出现的 X 和 Y 结构。 *)

ClearAll[comptonY, comptonX];
(* 构造带 little-group 指标的 Y 结构。 *)
comptonY[massiveIn_, photonMinus_, photonPlus_, massiveOut_, inIndex_, outIndex_] :=
    MixedAngle[MasslessLeg[photonMinus], MassiveLeg[massiveIn, inIndex]] *
    MixedSquare[MassiveLeg[massiveOut, outIndex], MasslessLeg[photonPlus]] +
    MixedAngle[MasslessLeg[photonMinus], MassiveLeg[massiveOut, outIndex]] *
    MixedSquare[MassiveLeg[massiveIn, inIndex], MasslessLeg[photonPlus]];
(* 构造动量链 X 结构。 *)
comptonX[massiveIn_, photonMinus_, photonPlus_, massiveOut_] :=
    MixedChain[
        MasslessLeg[photonMinus],
        {
            MassiveSpinorBrackets`mp[massiveIn] -
                MassiveSpinorBrackets`mp[massiveOut]
        },
        MasslessLeg[photonPlus]
    ];

(* 返回自旋 0 的 stripped Compton 振幅。 *)
ComptonAmplitude[
    0,
    {massiveIn_, photonMinus_, photonPlus_, massiveOut_},
    {s_, u_, mass_},
    coupling_
] := coupling^2 comptonX[
    massiveIn, photonMinus, photonPlus, massiveOut
]^2 / ((s - mass^2) (u - mass^2));

(* 返回自旋 1/2 的 stripped Compton 振幅。 *)
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

(* 返回自旋 1 的 stripped Compton 振幅。 *)
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
