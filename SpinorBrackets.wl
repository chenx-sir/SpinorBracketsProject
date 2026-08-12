BeginPackage["SpinorBrackets`"];

Unprotect[
    ab, sb, asb, sab, ra, la, rs, ls, p, s,
    SpinorExpand, SpinorCanonicalize, SpinorSimplify,
    MandelstamExpand, MomentumConserve,
    SchoutenIdentity, SchoutenRule, ParityConjugate,
    BCFWShift, SpinorEvaluate, SpinorKinematicsCheck,
    SpinorForm, Angle, Square
];

ab::usage = "ab[i,j] 表示角括号 <i j>；ab[i,p[...],...,j] 表示 <i|P...|j> 型 angle-angle spinor 链。";
sb::usage = "sb[i,j] 表示方括号 [i j]；sb[i,p[...],...,j] 表示 [i|P...|j] 型 square-square spinor 链。";
asb::usage = "asb[i,p[...],...,j] 表示混合 spinor 链 <i|P...|j]。";
sab::usage = "sab[i,p[...],...,j] 表示混合 spinor 链 [i|P...|j>。";
ra::usage = "ra[i] 表示右侧 angle ket |i>。";
la::usage = "la[i] 表示左侧 angle bra <i|。";
rs::usage = "rs[i] 表示右侧 square ket |i]。";
ls::usage = "ls[i] 表示左侧 square bra [i|。";
p::usage = "p[i,j,...] 表示动量和 p_i+p_j+...。";
s::usage = "s[i,j,...] 表示 (p_i+p_j+...)^2；本包约定 s[i,j]=ab[i,j] sb[i,j]，即 s_ij=<ij>[ij]。";

SpinorExpand::usage = "SpinorExpand[expr] 将 spinor 链中的动量和展开为二旋量括号的乘积之和。";
SpinorCanonicalize::usage = "SpinorCanonicalize[expr] 规范化二旋量括号和动量标签的顺序，并根据反对称性补上符号。";
SpinorSimplify::usage = "SpinorSimplify[expr] 展开 spinor 链与 Mandelstam 不变量，然后规范化括号并进行代数化简。";
MandelstamExpand::usage = "MandelstamExpand[expr] 将 s[A] 替换为 A 中所有 i<j 的 <ij>[ij] 之和。";
MomentumConserve::usage = "MomentumConserve[expr,n,leg] 利用 Sum_i p_i=0 消去第 leg 条外腿的动量；省略 leg 时默认消去第 n 条腿。";
SchoutenIdentity::usage = "SchoutenIdentity[Angle|Square,a,b,c,d] 返回恒等于零的 Schouten 组合。";
SchoutenRule::usage = "SchoutenRule[Angle|Square,a,b,c,d] 返回一个定向替换规则，用其余两项消去第一对括号的乘积。";
ParityConjugate::usage = "ParityConjugate[expr] 交换 angle 与 square spinor，并交换混合链 asb 与 sab。";
BCFWShift::usage = "BCFWShift[expr,{a,b},z] 执行 [a b> 变形：tilde-lambda_a -> tilde-lambda_a-z tilde-lambda_b，lambda_b -> lambda_b+z lambda_a。";
SpinorEvaluate::usage = "SpinorEvaluate[expr,{lambdas,lambdaTildes}] 使用给定的二维 spinor 数据对括号和 spinor 链进行数值或符号求值。";
SpinorKinematicsCheck::usage = "SpinorKinematicsCheck[{lambdas,lambdaTildes}] 检查 spinor 数据的维数、质壳条件和总动量守恒。";
SpinorForm::usage = "SpinorForm[expr] 使用 TraditionalForm 显示表达式；本包中的 spinor 对象默认已显示为常规 bra-ket 记号。";
Angle::usage = "Angle 用于选择角括号类型的恒等式。";
Square::usage = "Square 用于选择方括号类型的恒等式。";

Begin["`Private`"];

ClearAll[momentumSequenceQ];
(* 判断列表中的对象是否全部为无质量动量。 *)
momentumSequenceQ[items_List] := MatchQ[items, {p[___] ...}];

ClearAll[spinorEndpointQ, spinorChainMiddleQ];
(* 判断对象是否为开放旋量端点。 *)
spinorEndpointQ[expr_] :=
    MatchQ[expr, la[___] | ra[___] | ls[___] | rs[___]];
(* 判断中间对象中是否不含开放旋量端点。 *)
spinorChainMiddleQ[items_List] :=
    !AnyTrue[items, spinorEndpointQ];

ClearAll[scalarBracketQ, orderedProduct];
(* 判断对象是否为可交换的标量括号对象。 *)
scalarBracketQ[expr_] :=
    MatchQ[expr, ab[___] | sb[___] | asb[___] | sab[___]];
(* 根据因子类型选择普通乘法或非交换乘法。 *)
orderedProduct[items_List] := Which[
    AnyTrue[items, SameQ[#, 0] &],
        0,
    Length[items] == 0,
        1,
    Length[items] == 1,
        First[items],
    And @@ (scalarBracketQ /@ items),
        Times @@ items, (*items里面的所有元素都放到scalarBracketQ得到的结果全部都与起来，然后如果为true的话就，对items里面的所有元素都用普通乘法乘起来*)
    True,
        NonCommutativeMultiply @@ items
];

(* 将相邻的 angle bra 和 angle ket 收缩为角括号。 *)
la /: HoldPattern[
    NonCommutativeMultiply[before___, la[i_], ra[j_], after___]
] :=
    orderedProduct[{before, ab[i, j], after}];

(* 将相邻的 square bra 和 square ket 收缩为方括号。 *)
ls /: HoldPattern[
    NonCommutativeMultiply[before___, ls[i_], rs[j_], after___]
] :=
    orderedProduct[{before, sb[i, j], after}];

(* 将 angle bra 和 square ket 收缩为混合链。 *)
la /: HoldPattern[
    NonCommutativeMultiply[before___, la[i_], rs[j_], after___]
] :=
    orderedProduct[{before, asb[i, j], after}];

(* 将 square bra 和 angle ket 收缩为共轭混合链。 *)
ls /: HoldPattern[
    NonCommutativeMultiply[before___, ls[i_], ra[j_], after___]
] :=
    orderedProduct[{before, sab[i, j], after}];

(* 将带中间对象的 angle-angle 链收缩为 ab。 *)
la /: HoldPattern[
    NonCommutativeMultiply[la[i_], middle___, ra[j_]]
] /; spinorChainMiddleQ[{middle}] :=
    ab[i, middle, j];

(* 将带中间对象的 square-square 链收缩为 sb。 *)
ls /: HoldPattern[
    NonCommutativeMultiply[ls[i_], middle___, rs[j_]]
] /; spinorChainMiddleQ[{middle}] :=
    sb[i, middle, j];

(* 将带中间对象的 angle-square 链收缩为 asb。 *)
la /: HoldPattern[
    NonCommutativeMultiply[la[i_], middle___, rs[j_]]
] /; spinorChainMiddleQ[{middle}] :=
    asb[i, middle, j];

(* 将带中间对象的 square-angle 链收缩为 sab。 *)
ls /: HoldPattern[
    NonCommutativeMultiply[ls[i_], middle___, ra[j_]]
] /; spinorChainMiddleQ[{middle}] :=
    sab[i, middle, j];

(* 相同标签的反对称括号为零。 *)
ab[i_, i_] := 0;
(* 相同标签的方括号为零。 *)
sb[i_, i_] := 0;

ClearAll[chainFactor, expandChain];
(* 根据手征类型把相邻标签转换成基本括号。 *)
chainFactor[Angle, {i_, j_}] := ab[i, j];
chainFactor[Square, {i_, j_}] := sb[i, j];

(* 在链末端保留开放旋量收缩。 *)
openFactor[Angle, {i_, j_}] := la[i] ** rs[j];
openFactor[Square, {i_, j_}] := ls[i] ** ra[j];

(* 构造含动量插入的 spinor 链展开式。 *)
expandChain[start_, end_, i_, momenta_List, j_] := Module[
    {choices, terms, m = Length[momenta], lastType, pairList},
    choices = List @@@ momenta;
    If[AnyTrue[choices, EmptyQ], Return[0]];
    terms = Tuples[choices];  (*从choices的每个子列表中各选一个元素，生成所有的可能的组合*)
    Total[
        Function[labels,
            pairList = Partition[
                Join[{i}, labels, {j}], 
                2,
                1
            ]; (*将labels与{i}和{j}连接起来，然后按2个一组进行分割*)
            lastType = If[ (*交替规则，和tree_amplitude的定义一样*)
                OddQ[m], 
                If[start === Angle, Square, Angle],
                start
            ];
            Times[
                Sequence @@ MapIndexed[ (*对Most[pairList]中的每个元素应用chainFactor*)
                    chainFactor[
                        If[
                            OddQ[First[#2]],
                            start,
                            If[start === Angle, Square, Angle]
                        ],
                        #1
                    ] &, (*变成匿名函数，#1是列表，#2是索引*)
                    Most[pairList] (*去掉最后一个元素*)(*pairlist是分割后的列表*)
                ],
                If[
                    lastType === end,
                    chainFactor[lastType, Last[pairList]],
                    openFactor[lastType, Last[pairList]]
                ]
            ]
        ] /@ terms
    ]
];

(* 展开 spinor 链中的动量和。 *)
SpinorExpand[expr_] := Expand[
    expr /. {
        HoldPattern[ab[i_, middle___, j_]] /;
            Length[{middle}] > 0 && momentumSequenceQ[{middle}] :>
          expandChain[Angle, Angle, i, {middle}, j],
        HoldPattern[sb[i_, middle___, j_]] /;
            Length[{middle}] > 0 && momentumSequenceQ[{middle}] :>
          expandChain[Square, Square, i, {middle}, j],
        HoldPattern[asb[i_, middle___, j_]] /;
            Length[{middle}] > 0 && momentumSequenceQ[{middle}] :>
          expandChain[Angle, Square, i, {middle}, j],
        HoldPattern[sab[i_, middle___, j_]] /;
            Length[{middle}] > 0 && momentumSequenceQ[{middle}] :>
          expandChain[Square, Angle, i, {middle}, j]
    }
  ] /. HoldPattern[NonCommutativeMultiply[___, 0, ___]] :> 0;

(* 将 Mandelstam 动量平方展开为基本括号乘积。 *)
MandelstamExpand[expr_] := Expand[
    expr /. HoldPattern[s[labels__]] :>
        Total[ (*加法*)
            (ab[First[#], Last[#]] sb[First[#], Last[#]]) & /@
                Subsets[{labels}, {2}] (*生成所有包含两个元素的子集*)
        ]
];

ClearAll[canonicalStep];
(* 执行一次动量标签排序和反对称性规范化。 *)
canonicalStep[expr_] := expr /. { (*转换成标准排序（1，2，3，4，5）*)
    HoldPattern[p[labels___]] /; !OrderedQ[{labels}] :> (*将p[labels]中的labels排序，如果不是有序的就进行排序*)
        p @@ Sort[{labels}],
    HoldPattern[s[labels___]] /; !OrderedQ[{labels}] :>
        s @@ Sort[{labels}],
    HoldPattern[ab[i_, j_]] /; !OrderedQ[{i, j}] :>
        -ab[j, i],
    HoldPattern[sb[i_, j_]] /; !OrderedQ[{i, j}] :>
        -sb[j, i],
    HoldPattern[ab[i_, middle___, j_]] /;
        Length[{middle}] > 0 && momentumSequenceQ[{middle}] &&
            !OrderedQ[{i, j}] :>
        -ab[j, Sequence @@ Reverse[{middle}], i],
    HoldPattern[sb[i_, middle___, j_]] /;
        Length[{middle}] > 0 && momentumSequenceQ[{middle}] &&
            !OrderedQ[{i, j}] :>
        -sb[j, Sequence @@ Reverse[{middle}], i]
};

(* 反复执行规范化，直到表达式不再变化。 *)
SpinorCanonicalize[expr_] := FixedPoint[canonicalStep, expr];

(* 综合展开、规范化和代数化简 spinor 表达式。 *)
SpinorSimplify[expr_] :=
    Factor @ Together @ SpinorCanonicalize @
        Expand @ SpinorExpand @ MandelstamExpand[expr]; 

ClearAll[validMomentumSubsetQ, eliminateMomentumOnce];
(* 检查动量标签是否为合法的无重复外腿子集。 *)
validMomentumSubsetQ[labels_List, n_Integer] :=
    VectorQ[labels, IntegerQ] &&
        DuplicateFreeQ[labels] &&
        SubsetQ[Range[n], labels];

(* 用动量守恒规则消去一次指定外腿的动量。 *)
eliminateMomentumOnce[expr_, n_Integer, leg_Integer] := expr /. {
    HoldPattern[ab[i_, before___, p[labels___], after___, j_]] /;
        validMomentumSubsetQ[{labels}, n] && MemberQ[{labels}, leg] :>
          -ab[i, before, p @@ Complement[Range[n], {labels}], after, j], (*complement是求补集的函数*)
    HoldPattern[sb[i_, before___, p[labels___], after___, j_]] /;
        validMomentumSubsetQ[{labels}, n] && MemberQ[{labels}, leg] :>
          -sb[i, before, p @@ Complement[Range[n], {labels}], after, j],
    HoldPattern[asb[i_, before___, p[labels___], after___, j_]] /;
        validMomentumSubsetQ[{labels}, n] && MemberQ[{labels}, leg] :>
          -asb[i, before, p @@ Complement[Range[n], {labels}], after, j],
    HoldPattern[sab[i_, before___, p[labels___], after___, j_]] /;
        validMomentumSubsetQ[{labels}, n] && MemberQ[{labels}, leg] :>
          -sab[i, before, p @@ Complement[Range[n], {labels}], after, j],
    HoldPattern[p[labels___]] /;
        validMomentumSubsetQ[{labels}, n] && MemberQ[{labels}, leg] :>
          -p @@ Complement[Range[n], {labels}],
    HoldPattern[s[labels___]] /;
        validMomentumSubsetQ[{labels}, n] && MemberQ[{labels}, leg] :>
          s @@ Complement[Range[n], {labels}]
};

(* 利用总动量守恒消去指定编号的外腿动量。 *)
MomentumConserve[expr_, n_Integer, leg_Integer] /; 1 <= leg <= n :=
    SpinorCanonicalize[
        FixedPoint[eliminateMomentumOnce[#, n, leg] &, expr] (*expr不变，每次都接受上次的结果*)
    ];
(* 未指定外腿时默认消去第 n 条外腿。 *)
MomentumConserve[expr_, n_Integer] :=
    MomentumConserve[expr, n, n];

(* 返回角括号或方括号 Schouten 恒等式的左端表达式。 *)
SchoutenIdentity[Angle, a_, b_, c_, d_] :=
    ab[a, b] ab[c, d] +
    ab[a, c] ab[d, b] +
    ab[a, d] ab[b, c];
SchoutenIdentity[Square, a_, b_, c_, d_] :=
    sb[a, b] sb[c, d] +
    sb[a, c] sb[d, b] +
    sb[a, d] sb[b, c];

(* 返回用 Schouten 恒等式消去第一项的替换规则。 *)
SchoutenRule[Angle, a_, b_, c_, d_] :=
    HoldPattern[ab[a, b] ab[c, d]] :>
    -ab[a, c] ab[d, b] - ab[a, d] ab[b, c];
SchoutenRule[Square, a_, b_, c_, d_] :=
    HoldPattern[sb[a, b] sb[c, d]] :>
    -sb[a, c] sb[d, b] - sb[a, d] sb[b, c];

(* 交换角括号、方括号以及相应的开放旋量。 *)
ParityConjugate[expr_] := expr /. {
    ab -> sb, sb -> ab,
    ra -> rs, rs -> ra,
    la -> ls, ls -> la,
    asb -> sab, sab -> asb
};

(* 对指定的两条外腿执行无质量 BCFW 变形。 *)
BCFWShift[expr_, {a_, b_}, z_: z] :=
    Module[{expanded},
        expanded = SpinorExpand @ MandelstamExpand[expr];
        Expand[
            expanded /. {
                HoldPattern[ab[i_, j_]] :>
                    ab[i, j] +
                    If[SameQ[i, b], z ab[a, j], 0] +
                    If[SameQ[j, b], z ab[i, a], 0] +
                    If[SameQ[i, b] && SameQ[j, b], z^2 ab[a, a], 0], (*其实这一项是0，可以不写，但是为了代码的完整性还是加上了，但是我感觉删了也一样，无所谓了*)
                HoldPattern[sb[i_, j_]] :>
                    sb[i, j] -
                    If[SameQ[i, a], z sb[b, j], 0] -
                    If[SameQ[j, a], z sb[i, b], 0] +
                    If[SameQ[i, a] && SameQ[j, a], z^2 sb[b, b], 0],
                la[i_] /; SameQ[i, b] :>
                    la[b] + z la[a],
                ra[i_] /; SameQ[i, b] :>
                    ra[b] + z ra[a],
                ls[i_] /; SameQ[i, a] :>
                    ls[a] - z ls[b],
                rs[i_] /; SameQ[i, a] :>
                    rs[a] - z rs[b]
            }
        ]
    ];

ClearAll[det2, outer2];
(* 计算两个二维列向量横向排列后的行列式。 *)
det2[u_List, v_List] /; Length[u] == Length[v] == 2 := Det[{u, v}];
(* 计算两个二维列向量的外积矩阵。 *)
outer2[u_List, v_List] /; Length[u] == Length[v] == 2 :=
    Outer[Times, u, v]; (*计算u,v的外积，得到一个2x2矩阵*)

(*计算具体的数值*)
SpinorEvaluate::data = "需要两组长度相同的二维 spinor 列表，但收到的是 `1`。";
(* 使用给定数据对括号、动量和开放旋量求值。 *)
SpinorEvaluate[expr_, {lambdas_List, lambdaTildes_List}] := Module[
    {n = Length[lambdas], expanded},
    If[
        Length[lambdaTildes] =!= n ||
            !And @@ (MatchQ[#, {_, _}] & /@ (*检查每个元素是不是都是2维向量*)
                Join[lambdas, lambdaTildes]),
        Message[SpinorEvaluate::data, {lambdas, lambdaTildes}];
        Return[$Failed](*错误直接返回*)
    ];
    expanded = SpinorExpand @ MandelstamExpand[expr];
    expanded /. {
        HoldPattern[ab[i_Integer, j_Integer]] /;
            1 <= i <= n && 1 <= j <= n :>
          det2[lambdas[[i]], lambdas[[j]]],
        HoldPattern[sb[i_Integer, j_Integer]] /;
            1 <= i <= n && 1 <= j <= n :>
          det2[lambdaTildes[[i]], lambdaTildes[[j]]],
        HoldPattern[p[labels___Integer]] /;
            And @@ Thread[1 <= {labels} <= n] :> (*thread是分配函数，就是把labels中的每个元素都应用到判断是否在1，n之间*)
          Total[outer2[lambdas[[#]], lambdaTildes[[#]]] & /@ {labels}],
        la[i_Integer] /; 1 <= i <= n :>
            lambdas[[i]],
        ra[i_Integer] /; 1 <= i <= n :>
            lambdas[[i]],
        ls[i_Integer] /; 1 <= i <= n :>
            lambdaTildes[[i]],
        rs[i_Integer] /; 1 <= i <= n :>
            lambdaTildes[[i]]
    }
];


(* 检查二维数据、无质量质壳条件和总动量守恒。 *)
SpinorKinematicsCheck[{lambdas_List, lambdaTildes_List}] := Module[
    {sameLength, twoComponent, momenta, total},
    sameLength = Length[lambdas] == Length[lambdaTildes];
    twoComponent =
        And @@ (MatchQ[#, {_, _}] & /@
            Join[lambdas, lambdaTildes]);
    If[
        !sameLength || !twoComponent,
        Return[
            <|
                "ValidDimensions" -> False,
                "OnShell" -> False,
                "MomentumConserving" -> False
            |>
        ]
    ];
    momenta = MapThread[outer2, {lambdas, lambdaTildes}]; (*MapThread 会把两个列表中相同位置的元素配对，然后调用 outer2*)
    total = Total[momenta];
    <|
        "ValidDimensions" -> True, (*维数是否合法*)
        "OnShell" ->    (*是否在壳上*)
            And @@ (TrueQ[Simplify[Det[#] == 0]] & /@ momenta),
        "TotalMomentum" -> Simplify[total], (*返回总动量*)
        "MomentumConserving" -> (*是否守恒*)
            And @@ (TrueQ[Simplify[# == 0]] & /@ Flatten[total]) (*Flatten把嵌套的列表展开成一维列表*)
    |>
];

(*----------以下代码控制显示格式，与计算过程无关，可以完全用ai写，我感觉不是很重要----------*)

ClearAll[rawCallBoxes, interpretedBoxes, bracketBoxes, chainBoxes, spinorBoxes];

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

(* 生成普通括号的显示框。 *)
bracketBoxes[left_, right_, i_, j_, form_] :=
    RowBox[
        {
            left,
            MakeBoxes[i, form],
            "\[ThinSpace]",
            MakeBoxes[j, form],
            right
        }
    ];

(* 生成带动量插入的同手征链显示框。 *)
chainBoxes[left_, right_, i_, middle_List, j_, form_] :=
    RowBox[
        {
            left,
            MakeBoxes[i, form],
            "|",
            RowBox[
                Riffle[
                    MakeBoxes[#, form] & /@ middle,
                    "\[CenterDot]"
                ]
            ],
            "|",
            MakeBoxes[j, form],
            right
        }
    ];

(* 生成混合手征链显示框。 *)
mixedChainBoxes[left_, right_, i_, middle_List, j_, form_] :=
    RowBox[
        {
            left,
            MakeBoxes[i, form],
            "\[ThinSpace]",
            RowBox[
                Riffle[
                    MakeBoxes[#, form] & /@ middle,
                    "\[CenterDot]"
                ]
            ],
            "\[ThinSpace]",
            MakeBoxes[j, form],
            right
        }
    ];

(* 根据内部对象类型选择对应的显示框。 *)
spinorBoxes[ab, {i_, j_}, form_] :=
    bracketBoxes[
        "\[LeftAngleBracket]",
        "\[RightAngleBracket]",
        i,
        j,
        form
    ];
spinorBoxes[sb, {i_, j_}, form_] :=
    bracketBoxes["[", "]", i, j, form];
spinorBoxes[ab, {i_, middle___, j_}, form_] :=
    chainBoxes[
        "\[LeftAngleBracket]",
        "\[RightAngleBracket]",
        i,
        {middle},
        j,
        form
    ];
spinorBoxes[sb, {i_, middle___, j_}, form_] :=
    chainBoxes["[", "]", i, {middle}, j, form];
spinorBoxes[asb, {i_, middle___, j_}, form_] :=
    mixedChainBoxes["\[LeftAngleBracket]", "]", i, {middle}, j, form];
spinorBoxes[sab, {i_, middle___, j_}, form_] :=
    mixedChainBoxes[
        "[",
        "\[RightAngleBracket]",
        i,
        {middle},
        j,
        form
    ];
spinorBoxes[head_, args_List, form_] :=
    rawCallBoxes[SymbolName[Unevaluated[head]], args, form];

ab /: MakeBoxes[ab[args___], form : (StandardForm | TraditionalForm)] :=
    interpretedBoxes[spinorBoxes[ab, {args}, form], ab[args]];
(* 为 ab 定义可解释的角括号显示格式。 *)
sb /: MakeBoxes[sb[args___], form : (StandardForm | TraditionalForm)] :=
    interpretedBoxes[spinorBoxes[sb, {args}, form], sb[args]];
(* 为 sb 定义可解释的方括号显示格式。 *)
asb /: MakeBoxes[asb[args___], form : (StandardForm | TraditionalForm)] :=
    interpretedBoxes[spinorBoxes[asb, {args}, form], asb[args]];
(* 为 asb 定义可解释的混合括号显示格式。 *)
sab /: MakeBoxes[sab[args___], form : (StandardForm | TraditionalForm)] :=
    interpretedBoxes[spinorBoxes[sab, {args}, form], sab[args]];

(* 为 ra 定义右侧 angle ket 的显示格式。 *)
ra /: MakeBoxes[ra[i_], form : (StandardForm | TraditionalForm)] :=
    interpretedBoxes[
        RowBox[{"|", MakeBoxes[i, form], "\[RightAngleBracket]"}],
        ra[i]
    ];
(* 为 la 定义左侧 angle bra 的显示格式。 *)
la /: MakeBoxes[la[i_], form : (StandardForm | TraditionalForm)] :=
    interpretedBoxes[
        RowBox[{"\[LeftAngleBracket]", MakeBoxes[i, form], "|"}],
        la[i]
    ];
(* 为 rs 定义右侧 square ket 的显示格式。 *)
rs /: MakeBoxes[rs[i_], form : (StandardForm | TraditionalForm)] :=
    interpretedBoxes[
        RowBox[{"|", MakeBoxes[i, form], "]"}],
        rs[i]
    ];
(* 为 ls 定义左侧 square bra 的显示格式。 *)
ls /: MakeBoxes[ls[i_], form : (StandardForm | TraditionalForm)] :=
    interpretedBoxes[
        RowBox[{"[", MakeBoxes[i, form], "|"}],
        ls[i]
    ];

(* 将表达式转换为 TraditionalForm。 *)
SpinorForm[expr_] := TraditionalForm[expr];

End[];

Protect[
    ab, sb, asb, sab, ra, la, rs, ls, p, s,
    SpinorExpand, SpinorCanonicalize, SpinorSimplify,
    MandelstamExpand, MomentumConserve,
    SchoutenIdentity, SchoutenRule, ParityConjugate,
    BCFWShift, SpinorEvaluate, SpinorKinematicsCheck,
    SpinorForm, Angle, Square
];
EndPackage[];
