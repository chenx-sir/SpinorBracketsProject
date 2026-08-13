BeginPackage["MassiveSpinorBrackets`"];

Unprotect[
    mab, msb, masb, msab, mra, mla, mrs, mls, mp, mm,
    MassiveEpsilon, MassiveSpinorExpand, MassiveSpinorEvaluate,
    MassiveKinematicsCheck, MassiveSpinorForm
];

mab::usage = "mab[i,I,j,J] 表示 <i^I j^J>。";
msb::usage = "msb[i,I,j,J] 表示 [i_I j_J]。";
masb::usage = "masb[i,I,middle...,j,J] 表示 <i^I|...|j_J]。";
msab::usage = "msab[i,I,middle...,j,J] 表示 [i_I|...|j^J>。";
mra::usage = "mra[i,I] 表示 |i^I>；mla[i,I] 表示 <i^I|。";
mrs::usage = "mrs[i,I] 表示 |i_I]；mls[i,I] 表示 [i_I|。";
mp::usage = "mp[i] 表示有质量动量 p_i = |i^I>[i_I|；mp[i,j,...] 表示动量和。";
mm::usage = "mm[i] 是由 det(p_i) 定义的质量平方；mm[i,j,...] 是动量和的平方。";
MassiveEpsilon::usage = "MassiveEpsilon[I,J] 是 SU(2) little-group 的 epsilon 张量，约定 MassiveEpsilon[1,2]=1。";
MassiveSpinorExpand::usage = "MassiveSpinorExpand[expr] 将有质量 spinor 链中的动量和展开为二旋量括号的乘积之和。";
MassiveSpinorEvaluate::usage = "MassiveSpinorEvaluate[expr,{lambdas,lambdaTildes}] 使用给定的有质量二维 spinor 数据求值。";
MassiveKinematicsCheck::usage = "MassiveKinematicsCheck[{lambdas,lambdaTildes}] 检查有质量 spinor 数据的维数、质量平方和总动量守恒；MassiveKinematicsCheck[{lambdas,lambdaTildes},masses] 另外检查给定质量的质壳条件。";
MassiveSpinorForm::usage = "MassiveSpinorForm[expr] 使用 TraditionalForm 显示有质量 spinor 表达式。";

Begin["`Private`"];

(* 返回 SU(2) little-group epsilon 张量的指定分量。 *)
MassiveEpsilon[i_Integer, j_Integer] /;
    1 <= i <= 2 && 1 <= j <= 2 :=
    {{0, 1}, {-1, 0}}[[i, j]];

ClearAll[momentumSequenceQ];
(* 判断列表中的对象是否全部为有质量动量。 *)
momentumSequenceQ[items_List] :=
    MatchQ[items, {MassiveSpinorBrackets`mp[___] ...}];

ClearAll[massiveSameChiralityMiddleQ, massiveMixedChiralityMiddleQ];
(* 判断同手征链的中间动量个数是否满足偶数条件。 *)
massiveSameChiralityMiddleQ[items_List] :=
    items === {} ||
        (momentumSequenceQ[items] && EvenQ[Length[items]]);
(* 判断混合手征链的中间动量个数是否满足奇数条件。 *)
massiveMixedChiralityMiddleQ[items_List] :=
    momentumSequenceQ[items] && OddQ[Length[items]];

ClearAll[scalarBracketQ, orderedProduct];
(* 判断对象是否为可交换的有质量标量括号。 *)
scalarBracketQ[expr_] :=
    MatchQ[
        expr,
        MassiveSpinorBrackets`mab[___] |
        MassiveSpinorBrackets`msb[___] |
        MassiveSpinorBrackets`masb[___] |
        MassiveSpinorBrackets`msab[___] |
        MassiveSpinorBrackets`mm[___]
    ];
(* 根据因子类型选择普通乘法或非交换乘法。 *)
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

(* 同一腿且 little-group 指标相同时，角括号为零。 *)
mab[i_, ii_, j_, jj_] /;
    SameQ[i, j] && SameQ[ii, jj] := 0;
(* 同一腿且 little-group 指标相同时，方括号为零。 *)
msb[i_, ii_, j_, jj_] /;
    SameQ[i, j] && SameQ[ii, jj] := 0;

(* 将相邻的 angle bra 和 angle ket 收缩为有质量角括号。 *)
mla /: HoldPattern[
    NonCommutativeMultiply[pre___, mla[i_, ii_], mra[j_, jj_], post___]
] :=
    orderedProduct[
        {
            pre,
            mab[i, ii, j, jj],
            post
        }
    ];
(* 将相邻的 square bra 和 square ket 收缩为有质量方括号。 *)
mls /: HoldPattern[
    NonCommutativeMultiply[pre___, mls[i_, ii_], mrs[j_, jj_], post___]
] :=
    orderedProduct[
        {
            pre,
            msb[i, ii, j, jj],
            post
        }
    ];

(* 将带偶数个动量插入的 angle-angle 链收缩为 mab。 *)
mla /: HoldPattern[
    NonCommutativeMultiply[mla[i_, ii_], middle___, mra[j_, jj_]]
] /; massiveSameChiralityMiddleQ[{middle}] && Length[{middle}] > 0 :=
    mab[i, ii, middle, j, jj];
(* 将带偶数个动量插入的 square-square 链收缩为 msb。 *)
mls /: HoldPattern[
    NonCommutativeMultiply[mls[i_, ii_], middle___, mrs[j_, jj_]]
] /; massiveSameChiralityMiddleQ[{middle}] && Length[{middle}] > 0 :=
    msb[i, ii, middle, j, jj];
(* 将带奇数个动量插入的 angle-square 链收缩为 masb。 *)
mla /: HoldPattern[
    NonCommutativeMultiply[mla[i_, ii_], middle___, mrs[j_, jj_]]
] /; massiveMixedChiralityMiddleQ[{middle}] :=
    masb[i, ii, middle, j, jj];
(* 将带奇数个动量插入的 square-angle 链收缩为 msab。 *)
mls /: HoldPattern[
    NonCommutativeMultiply[mls[i_, ii_], middle___, mra[j_, jj_]]
] /; massiveMixedChiralityMiddleQ[{middle}] :=
    msab[i, ii, middle, j, jj];

ClearAll[angle, square, chainFactor, expandChain];
(* 将相邻的有质量旋量标签转换成基本括号。 *)
chainFactor[angle, {{i_, ii_}, {j_, jj_}}] :=
    mab[i, ii, j, jj];
(* 将相邻的有质量方旋量标签转换成基本方括号。 *)
chainFactor[square, {{i_, ii_}, {j_, jj_}}] :=
    msb[i, ii, j, jj];

(* 展开有质量动量插入，并对每个插入求和 SU(2) little-group 指标。 *)
expandChain[start_, end_, {i_, ii_}, momenta_List, {j_, jj_}] := Module[
    {
        choices,
        terms,
        littleGroupTerms,
        m = Length[momenta],
        pairList
    },
    choices = List @@@ momenta;
    If[AnyTrue[choices, EmptyQ], Return[0]];
    terms = Tuples[choices];
    littleGroupTerms = Tuples[Range[2], m];
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
                    Times @@ MapIndexed[
                        chainFactor[
                            If[
                                OddQ[First[#2]],
                                start,
                                If[start === angle, square, angle]
                            ],
                            #1
                        ] &,
                        pairList
                    ]
                ] /@ littleGroupTerms
            ]
        ] /@ terms
    ]
];

(* 展开有质量 spinor 链中的动量和。 *)
MassiveSpinorExpand[expr_] := Expand[
    expr /. {
        HoldPattern[mab[i_, ii_, middle___, j_, jj_]] /;
            massiveSameChiralityMiddleQ[{middle}] && Length[{middle}] > 0 :>
          expandChain[angle, angle, {i, ii}, {middle}, {j, jj}],
        HoldPattern[msb[i_, ii_, middle___, j_, jj_]] /;
            massiveSameChiralityMiddleQ[{middle}] && Length[{middle}] > 0 :>
          expandChain[square, square, {i, ii}, {middle}, {j, jj}],
        HoldPattern[masb[i_, ii_, middle___, j_, jj_]] /;
            massiveMixedChiralityMiddleQ[{middle}] :>
          expandChain[angle, square, {i, ii}, {middle}, {j, jj}],
        HoldPattern[msab[i_, ii_, middle___, j_, jj_]] /;
            massiveMixedChiralityMiddleQ[{middle}] :>
          expandChain[square, angle, {i, ii}, {middle}, {j, jj}]
    }
] /. HoldPattern[NonCommutativeMultiply[___, 0, ___]] :> 0;

ClearAll[det2, outer2, raise2, massiveMomentum];
(* 计算两个二维旋量的行列式。 *)
det2[u_List, v_List] /; Length[u] == Length[v] == 2 :=
    Det[{u, v}];
(* 计算两个二维列向量的外积矩阵。 *)
outer2[u_List, v_List] /; Length[u] == Length[v] == 2 :=
    Outer[Times, u, v];
(* 根据 epsilon^12 = -1 的约定升高二维 Lorentz 旋量指标。 *)
raise2[{a_, b_}] := {-b, a};
(* 根据两组 little-group 旋量构造第 i 条有质量动量矩阵。 *)
massiveMomentum[lambdas_List, lambdaTildes_List, i_Integer] :=
    Total[MapThread[outer2, {lambdas[[i]], lambdaTildes[[i]]}]];

MassiveSpinorEvaluate::data = "需要两组长度相同的有质量二维 spinor 列表，但收到的是 `1`。";
(* 使用给定数据对有质量括号、动量和开放旋量求值。 *)
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
          raise2[lambdas[[i, ii]]],
        HoldPattern[mrs[i_Integer, ii_Integer]] /;
            1 <= i <= n && 1 <= ii <= 2 :>
          lambdaTildes[[i, ii]],
        HoldPattern[mls[i_Integer, ii_Integer]] /;
            1 <= i <= n && 1 <= ii <= 2 :>
          raise2[lambdaTildes[[i, ii]]]
    }
];

(* 从 Association 格式的数据中读取两组有质量旋量。 *)
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

(* 检查有质量旋量数据的维数、质量平方、总动量和守恒。 *)
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
        "OnShell" -> Missing["MassesNotProvided"],
        "MassSquared" -> massSquared,
        "TotalMomentum" -> Simplify[total],
        "MomentumConserving" ->
            And @@ (TrueQ[Simplify[# == 0]] & /@ Flatten[total])
    |>
];

(* 在给定质量列表时额外检查每条腿的质壳条件。 *)
MassiveKinematicsCheck[
    {lambdas_List, lambdaTildes_List},
    masses_List
] := Module[
    {report, momenta, onShell},
    report = MassiveKinematicsCheck[{lambdas, lambdaTildes}];
    If[
        !TrueQ[report["ValidDimensions"]] ||
            Length[masses] =!= Length[lambdas],
        Return[ReplacePart[report, "OnShell" -> False]]
    ];
    momenta = Table[
        massiveMomentum[lambdas, lambdaTildes, i],
        {i, Length[lambdas]}
    ];
    onShell = And @@ MapThread[
        TrueQ[Simplify[Det[#1] == #2^2]] &,
        {momenta, masses}
    ];
    ReplacePart[report, "OnShell" -> onShell]
];

(* 对 Association 格式的数据执行有质量运动学检查。 *)
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

(* 对 Association 数据和给定质量列表执行质壳检查。 *)
MassiveKinematicsCheck[data_Association, masses_List] := With[
    {
        lambdas = Lookup[data, "Lambda", Missing["NotAvailable"]],
        lambdaTildes =
            Lookup[data, "LambdaTilde", Missing["NotAvailable"]]
    },
    If[
        ListQ[lambdas] && ListQ[lambdaTildes],
        MassiveKinematicsCheck[{lambdas, lambdaTildes}, masses],
        <|
            "ValidDimensions" -> False,
            "OnShell" -> False,
            "MomentumConserving" -> False
        |>
    ]
];

(*----------以下代码控制显示格式，与计算过程无关----------*)

ClearAll[
    rawCallBoxes,
    interpretedBoxes,
    massiveIndexBox,
    massiveBracketBoxes,
    massiveChainBoxes,
    massiveMixedChainBoxes,
    massiveSpinorBoxes
];

(* 为未识别的内部对象生成原始函数调用框。 *)
rawCallBoxes[head_String, args_List, form_] :=
    RowBox[
        {
            head,
            "[",
            RowBox[Riffle[MakeBoxes[#, form] & /@ args, ","]],
            "]"
        }
    ];

SetAttributes[interpretedBoxes, HoldRest];
(* 创建可显示且仍可还原为原表达式的解释框。 *)
interpretedBoxes[display_, expr_] :=
    InterpretationBox[display, expr];

(* 根据指标位置生成上标或下标框。 *)
massiveIndexBox[i_, ii_, position_, form_] :=
    If[
        position === Upper,
        SuperscriptBox[MakeBoxes[i, form], MakeBoxes[ii, form]],
        SubscriptBox[MakeBoxes[i, form], MakeBoxes[ii, form]]
    ];

(* 生成基本有质量括号的显示框。 *)
massiveBracketBoxes[left_, right_, i_, ii_, j_, jj_, form_] :=
    RowBox[
        {
            left,
            massiveIndexBox[i, ii, Upper, form],
            "\[ThinSpace]",
            massiveIndexBox[j, jj, Upper, form],
            right
        }
    ];

(* 生成同手征有质量链的显示框。 *)
massiveChainBoxes[
    left_, right_, i_, ii_, middle_List, j_, jj_, firstPosition_,
    lastPosition_, form_
] :=
    RowBox[
        {
            left,
            massiveIndexBox[i, ii, firstPosition, form],
            "|",
            RowBox[
                Riffle[
                    MakeBoxes[#, form] & /@ middle,
                    "\[CenterDot]"
                ]
            ],
            "|",
            massiveIndexBox[j, jj, lastPosition, form],
            right
        }
    ];

(* 生成混合手征有质量链的显示框。 *)
massiveMixedChainBoxes[
    left_, right_, i_, ii_, middle_List, j_, jj_, firstPosition_,
    lastPosition_, form_
] :=
    RowBox[
        {
            left,
            massiveIndexBox[i, ii, firstPosition, form],
            "\[ThinSpace]",
            RowBox[
                Riffle[
                    MakeBoxes[#, form] & /@ middle,
                    "\[CenterDot]"
                ]
            ],
            "\[ThinSpace]",
            massiveIndexBox[j, jj, lastPosition, form],
            right
        }
    ];

(* 为有质量 spinor 对象选择相应的显示形式。 *)
massiveSpinorBoxes[mab, {i_, ii_, j_, jj_}, form_] :=
    massiveBracketBoxes[
        "\[LeftAngleBracket]",
        "\[RightAngleBracket]",
        i,
        ii,
        j,
        jj,
        form
    ];
(* 为 msb 选择方括号显示形式。 *)
massiveSpinorBoxes[msb, {i_, ii_, j_, jj_}, form_] :=
    RowBox[
        {
            "[",
            massiveIndexBox[i, ii, Lower, form],
            "\[ThinSpace]",
            massiveIndexBox[j, jj, Lower, form],
            "]"
        }
    ];
(* 为带动量插入的 mab 选择同手征链显示形式。 *)
massiveSpinorBoxes[mab, {i_, ii_, middle___, j_, jj_}, form_] :=
    massiveChainBoxes[
        "\[LeftAngleBracket]",
        "\[RightAngleBracket]",
        i,
        ii,
        {middle},
        j,
        jj,
        Upper,
        Upper,
        form
    ];
(* 为带动量插入的 msb 选择同手征链显示形式。 *)
massiveSpinorBoxes[msb, {i_, ii_, middle___, j_, jj_}, form_] :=
    massiveChainBoxes[
        "[",
        "]",
        i,
        ii,
        {middle},
        j,
        jj,
        Lower,
        Lower,
        form
    ];
(* 为 masb 选择混合手征链显示形式。 *)
massiveSpinorBoxes[masb, {i_, ii_, middle___, j_, jj_}, form_] :=
    massiveMixedChainBoxes[
        "\[LeftAngleBracket]",
        "]",
        i,
        ii,
        {middle},
        j,
        jj,
        Upper,
        Lower,
        form
    ];
(* 为 msab 选择混合手征链显示形式。 *)
massiveSpinorBoxes[msab, {i_, ii_, middle___, j_, jj_}, form_] :=
    massiveMixedChainBoxes[
        "[",
        "\[RightAngleBracket]",
        i,
        ii,
        {middle},
        j,
        jj,
        Lower,
        Upper,
        form
    ];
(* 对未知有质量对象退回原始函数调用显示。 *)
massiveSpinorBoxes[head_, args_List, form_] :=
    rawCallBoxes[SymbolName[Unevaluated[head]], args, form];

(* 为 mab 定义可解释的角括号显示格式。 *)
mab /: MakeBoxes[
    mab[args___],
    form : (StandardForm | TraditionalForm)
] :=
    interpretedBoxes[
        massiveSpinorBoxes[mab, {args}, form],
        mab[args]
    ];
(* 为 msb 定义可解释的方括号显示格式。 *)
msb /: MakeBoxes[
    msb[args___],
    form : (StandardForm | TraditionalForm)
] :=
    interpretedBoxes[
        massiveSpinorBoxes[msb, {args}, form],
        msb[args]
    ];
(* 为 masb 定义可解释的混合括号显示格式。 *)
masb /: MakeBoxes[
    masb[args___],
    form : (StandardForm | TraditionalForm)
] :=
    interpretedBoxes[
        massiveSpinorBoxes[masb, {args}, form],
        masb[args]
    ];
(* 为 msab 定义可解释的混合括号显示格式。 *)
msab /: MakeBoxes[
    msab[args___],
    form : (StandardForm | TraditionalForm)
] :=
    interpretedBoxes[
        massiveSpinorBoxes[msab, {args}, form],
        msab[args]
    ];

(* 为 mra 定义右侧 angle ket 的显示格式。 *)
mra /: MakeBoxes[
    mra[i_, ii_],
    form : (StandardForm | TraditionalForm)
] :=
    interpretedBoxes[
        RowBox[
            {
                "|",
                massiveIndexBox[i, ii, Upper, form],
                "\[RightAngleBracket]"
            }
        ],
        mra[i, ii]
    ];
(* 为 mla 定义左侧 angle bra 的显示格式。 *)
mla /: MakeBoxes[
    mla[i_, ii_],
    form : (StandardForm | TraditionalForm)
] :=
    interpretedBoxes[
        RowBox[
            {
                "\[LeftAngleBracket]",
                massiveIndexBox[i, ii, Upper, form],
                "|"
            }
        ],
        mla[i, ii]
    ];
(* 为 mrs 定义右侧 square ket 的显示格式。 *)
mrs /: MakeBoxes[
    mrs[i_, ii_],
    form : (StandardForm | TraditionalForm)
] :=
    interpretedBoxes[
        RowBox[
            {
                "|",
                massiveIndexBox[i, ii, Lower, form],
                "]"
            }
        ],
        mrs[i, ii]
    ];
(* 为 mls 定义左侧 square bra 的显示格式。 *)
mls /: MakeBoxes[
    mls[i_, ii_],
    form : (StandardForm | TraditionalForm)
] :=
    interpretedBoxes[
        RowBox[
            {
                "[",
                massiveIndexBox[i, ii, Lower, form],
                "|"
            }
        ],
        mls[i, ii]
    ];

(* 将有质量 spinor 表达式转换为 TraditionalForm。 *)
MassiveSpinorForm[expr_] := TraditionalForm[expr];

End[];

Protect[
    mab, msb, masb, msab, mra, mla, mrs, mls, mp, mm,
    MassiveEpsilon, MassiveSpinorExpand, MassiveSpinorEvaluate,
    MassiveKinematicsCheck, MassiveSpinorForm
];
EndPackage[];
