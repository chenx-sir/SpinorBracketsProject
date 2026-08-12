# SpinorBracketsProject

SpinorBracketsProject 是一组用于四维 spinor-helicity 计算的 Wolfram Language 包：

- SpinorBrackets.wl：无质量粒子的二旋量括号与 spinor 链。
- MassiveSpinorBrackets.wl：有质量粒子的 massive spinor-helicity 括号与 spinor 链。

两个包都使用符号表达式表示 spinor bracket，并使用上值（UpValues）将相邻的开放旋量自动收缩为括号或 spinor 链。表达式在 Mathematica 中默认以 bra-ket 形式显示，但内部仍保留 ab[...]、mab[...] 等原始符号，便于继续进行规则替换和代数计算。

## 安装与加载

克隆仓库后，可以直接用文件路径加载：

    project = "/path/to/SpinorBracketsProject";
    Get[FileNameJoin[{project, "SpinorBrackets.wl"}]];
    Get[FileNameJoin[{project, "MassiveSpinorBrackets.wl"}]];

也可以将项目目录加入 Mathematica 的 $Path 后使用标准包加载方式。两个包的包名分别是 SpinorBrackets 和 MassiveSpinorBrackets。

## 物理约定

### 无质量粒子

无质量四动量写成一个秩一的二乘矩阵：

$$
p_{\alpha\dot\alpha}
=\lambda_\alpha\tilde\lambda_{\dot\alpha}.
$$

每条外腿由一组二维 Weyl spinor lambda 和 lambdaTilde 描述。基本括号约定为：

$$
\langle i j\rangle
=\epsilon^{\alpha\beta}\lambda_{i,\alpha}\lambda_{j,\beta},
\qquad
[i j]
=\epsilon^{\dot\alpha\dot\beta}
\tilde\lambda_{i,\dot\alpha}
\tilde\lambda_{j,\dot\beta}.
$$

本项目采用：

$$
s_{ij}=\langle i j\rangle[i j].
$$

### 有质量粒子

有质量四动量矩阵一般是满秩的，需要两个 little-group 分量：

$$
p_{\alpha\dot\alpha}
=\lambda^I_\alpha\tilde\lambda_{\dot\alpha I},
\qquad I=1,2.
$$

有质量包采用：

$$
\epsilon_{12}=1,\qquad \epsilon^{12}=-1.
$$

因此：

    MassiveEpsilon[1, 1]  (* 0 *)
    MassiveEpsilon[1, 2]  (* 1 *)
    MassiveEpsilon[2, 1]  (* -1 *)
    MassiveEpsilon[2, 2]  (* 0 *)

质量平方由动量矩阵的行列式给出：

$$
m_i^2=\det(p_i).
$$

所以 mm[i] 表示质量平方 m_i^2，而不是质量 m_i。

## 无质量包：SpinorBrackets

### 基本对象

| 符号 | 含义 |
| --- | --- |
| ab[i,j] | 角括号 $\langle i j\rangle$ |
| sb[i,j] | 方括号 $[i j]$ |
| asb[i,...,j] | 混合链 $\langle i|\cdots|j]$ |
| sab[i,...,j] | 混合链 $[i|\cdots|j\rangle$ |
| la[i] | 左 angle bra $\langle i|$ |
| ra[i] | 右 angle ket $|i\rangle$ |
| ls[i] | 左 square bra $[i|$ |
| rs[i] | 右 square ket $|i]$ |
| p[i,j,...] | 动量和 $p_i+p_j+\cdots$ |
| s[i,j,...] | 动量和平方 $(p_i+p_j+\cdots)^2$ |

相邻开放旋量使用双星号收缩：

    la[1] ** ra[2]                       (* ab[1,2] *)
    ls[1] ** rs[2]                       (* sb[1,2] *)
    la[1] ** p[2] ** rs[3]               (* asb[1,p[2],3] *)
    ls[1] ** p[2] ** ra[3]               (* sab[1,p[2],3] *)
    la[1] ** p[2] ** p[3] ** ra[4]       (* ab[1,p[2],p[3],4] *)

SpinorExpand[expr] 将动量插入展开为基本括号：

    SpinorExpand[ab[1, p[2], 3]]
    (* ab[1,2] sb[2,3] *)

    SpinorExpand[ab[1, p[2], p[3], 4]]
    (* ab[1,2] sb[2,3] ab[3,4] *)

    SpinorExpand[asb[1, p[2,3], 4]]
    (* ab[1,2] sb[2,4] + ab[1,3] sb[3,4] *)

p[2,3] 表示 p[2]+p[3]，展开时会逐项分配。

### 代数化简功能

    SpinorCanonicalize[expr]

按 OrderedQ 规范排序动量标签和括号标签，并根据角括号、方括号的反对称性补充负号。

    MandelstamExpand[expr]

将 s[i,j,...] 替换为所有二元子集的括号乘积之和。例如：

    MandelstamExpand[s[1,2,3]]
    (* ab[1,2] sb[1,2] + ab[1,3] sb[1,3]
       + ab[2,3] sb[2,3] *)

    SpinorSimplify[expr]

依次执行 Mandelstam 展开、spinor 链展开、规范化和代数化简。

    MomentumConserve[expr, n]
    MomentumConserve[expr, n, leg]

利用总动量守恒消去某条外腿。省略 leg 时默认消去第 n 条腿：

    MomentumConserve[p[1,2], 3]
    (* -p[3] *)

### Schouten、宇称和 BCFW

SchoutenIdentity[Angle, a, b, c, d] 和 SchoutenIdentity[Square, a, b, c, d] 返回：

$$
\langle a b\rangle\langle c d\rangle
+\langle a c\rangle\langle d b\rangle
+\langle a d\rangle\langle b c\rangle=0.
$$

SchoutenRule[Angle, a, b, c, d] 和 SchoutenRule[Square, a, b, c, d] 返回定向替换规则，例如：

    expr /. SchoutenRule[Angle, a, b, c, d]

ParityConjugate[expr] 交换 angle 与 square 对象，同时交换 asb 和 sab。

BCFWShift[expr, {a,b}, z] 执行无质量 [a b> shift：

$$
\tilde\lambda_a\to\tilde\lambda_a-z\tilde\lambda_b,
\qquad
\lambda_b\to\lambda_b+z\lambda_a.
$$

### 无质量数值求值

输入数据是两个等长列表，每条外腿对应一个二维向量：

    lambdas = {{lambda11, lambda12}, {lambda21, lambda22}};
    lambdaTildes = {
        {tildeLambda11, tildeLambda12},
        {tildeLambda21, tildeLambda22}
    };

    SpinorEvaluate[ab[1,2], {lambdas, lambdaTildes}]
    SpinorEvaluate[p[1,2], {lambdas, lambdaTildes}]

SpinorEvaluate 支持基本括号、动量、开放旋量及可由 SpinorExpand 展开的 spinor 链。数据维度不合法时返回 $Failed，并给出 SpinorEvaluate::data 消息。

SpinorKinematicsCheck[{lambdas, lambdaTildes}] 返回 ValidDimensions、OnShell、TotalMomentum 和 MomentumConserving 等字段，使用每条动量矩阵的行列式为零检查无质量质壳条件。

## 有质量包：MassiveSpinorBrackets

有质量包保留了无质量包的命名风格，但在外腿编号之后增加 little-group 指标。

### 基本对象

| 符号 | 含义 |
| --- | --- |
| mab[i,I,j,J] | $\langle i^I j^J\rangle$ |
| msb[i,I,j,J] | $[i_I j_J]$ |
| masb[i,I,...,j,J] | $\langle i^I|\cdots|j_J]$ |
| msab[i,I,...,j,J] | $[i_I|\cdots|j^J\rangle$ |
| mla[i,I] | 左 angle bra $\langle i^I|$ |
| mra[i,I] | 右 angle ket $|i^I\rangle$ |
| mls[i,I] | 左 square bra $[i_I|$ |
| mrs[i,I] | 右 square ket $|i_I]$ |
| mp[i,j,...] | 有质量动量和 $p_i+p_j+\cdots$ |
| mm[i,j,...] | 动量和的质量平方 $\det(p_i+p_j+\cdots)$ |
| MassiveEpsilon[I,J] | little-group 的二维 epsilon 张量 |

例如：

    mab[1, 1, 2, 2]          (* 显示为 <1^1 2^2> *)
    msb[1, 1, 2, 2]          (* 显示为 [1_1 2_2] *)
    masb[1, 1, mp[2], 3, 2] (* 显示为 <1^1|mp[2]|3_2] *)

显示由 MakeBoxes 实现，并通过 InterpretationBox 保留内部表达式。SpinorForm[expr] 可以显式请求 TraditionalForm 显示。

注意：Wolfram Language 中大写 I 是内置虚数单位。实际使用 little-group 指标时建议使用 ii、I1 或整数 1,2：

    mab[i, ii, j, jj]
    mab[1, 1, 2, 2]

### 有质量开放旋量收缩

有质量链的手征性和动量插入数必须匹配：

- angle-angle 或 square-square 两端：动量插入数必须为偶数。
- angle-square 或 square-angle 两端：动量插入数必须为奇数。

例如：

    mla[1, 1] ** mra[2, 2]       (* mab[1,1,2,2] *)
    mls[1, 1] ** mrs[2, 2]       (* msb[1,1,2,2] *)
    mla[1, 1] ** mp[2] ** mrs[3, 2]  (* masb[...] *)
    mls[1, 1] ** mp[2] ** mra[3, 2]  (* msab[...] *)

非法手征性组合会保持为开放的 NonCommutativeMultiply 表达式，而不会被强行转成标量括号。例如 mla[1,1] ** mp[2] ** mra[3,2] 不是合法的 angle-angle 链。

### 有质量链展开

MassiveSpinorExpand[expr] 按照

$$
p_{\alpha\dot\alpha}
=\lambda^I_\alpha\tilde\lambda_{\dot\alpha I}
$$

展开动量插入，并对中间 little-group 指标求和：

    MassiveSpinorExpand[masb[1, 1, mp[2], 3, 2]]

结果为：

    mab[1, 1, 2, 1] msb[2, 1, 3, 2]
    + mab[1, 1, 2, 2] msb[2, 2, 3, 2]

多个动量插入时，包同时对动量标签和每个中间 little-group 指标求和，并生成交替的 mab、msb、mab 链。

### 有质量数值求值

每条外腿的 lambdas 和 lambdaTildes 都是一个 2 x 2 数组：第一层索引是 little-group 指标，第二层索引是二维 Lorentz spinor 分量。

    lambdas = {
        {{lambda1^1_1, lambda1^1_2}, {lambda1^2_1, lambda1^2_2}},
        {{lambda2^1_1, lambda2^1_2}, {lambda2^2_1, lambda2^2_2}}
    };

    lambdaTildes = {
        {{tildeLambda1_1^1, tildeLambda1_1^2},
         {tildeLambda1_2^1, tildeLambda1_2^2}},
        {{tildeLambda2_1^1, tildeLambda2_1^2},
         {tildeLambda2_2^1, tildeLambda2_2^2}}
    };

也可以使用 association：

    data = <|"Lambda" -> lambdas, "LambdaTilde" -> lambdaTildes|>;
    MassiveSpinorEvaluate[mab[1, 1, 2, 2], data]
    MassiveSpinorEvaluate[mp[1], data]
    MassiveSpinorEvaluate[mm[1], data]

支持的求值对象包括 mab、msb 基本括号，经 MassiveSpinorExpand 展开的链，mp 动量矩阵，mm 动量和的行列式，以及四种开放二维 spinor。

### 有质量运动学检查

不提供质量列表时：

    MassiveKinematicsCheck[{lambdas, lambdaTildes}]

返回 ValidDimensions、OnShell、MassSquared、TotalMomentum 和 MomentumConserving 等字段，其中 OnShell 为 Missing["MassesNotProvided"]。

如果给出质量列表：

    MassiveKinematicsCheck[
        {lambdas, lambdaTildes},
        {m1, m2, ...}
    ]

则额外检查：

$$
\det(p_i)=m_i^2.
$$

association 输入也受到支持：

    MassiveKinematicsCheck[data]
    MassiveKinematicsCheck[data, {m1, m2, ...}]

## 两个包的主要区别与当前边界

| 功能 | SpinorBrackets | MassiveSpinorBrackets |
| --- | --- | --- |
| 外腿数据 | 每腿一组二维 spinor | 每腿两组 little-group spinor |
| 角括号 | ab[i,j] | mab[i,I,j,J] |
| 方括号 | sb[i,j] | msb[i,I,j,J] |
| 动量 | p[i,j,...] | mp[i,j,...] |
| 不变量 | s[i,j,...] | mm[i,j,...] |
| 动量展开 | SpinorExpand | MassiveSpinorExpand |
| 数值求值 | SpinorEvaluate | MassiveSpinorEvaluate |
| 运动学检查 | SpinorKinematicsCheck | MassiveKinematicsCheck |
| Schouten | SchoutenIdentity、SchoutenRule | 当前未提供自动化接口 |
| BCFW | BCFWShift | 当前未实现 massive BCFW shift |
| 宇称共轭 | ParityConjugate | 当前未提供对应接口 |
| 规范化 | SpinorCanonicalize | 当前没有完整的 massive canonicalizer |

有质量包目前重点实现了 massive spinor 的数据结构、little-group 指标、动量分解、手征性检查、质壳关系、数值求值和输出渲染。它不会自动把所有带有 little-group 指标的表达式化为完整的 Lorentz 标量，也不会自动实现所有文献中的 epsilon 降指标和 massive Schouten 化简。

普通 Lorentz Schouten 恒等式仍然存在，因为 Weyl spinor 的 Lorentz 指标仍然是二维的；在当前包中，需要先用 MassiveSpinorExpand 展开后手动使用这些关系。

## 混合包：MixedSpinorBrackets

`MixedSpinorBrackets.wl` 用于有质量粒子和无质量粒子同时出现在同一个表达式中的情形，例如截图中的 massive Compton scattering。它不复制前面两个包的实现，而是通过 `Needs` 直接调用：

- `SpinorBrackets`` 的无质量括号、动量和数值求值；
- `MassiveSpinorBrackets`` 的有质量括号、动量分解和数值求值。

混合包加载时会自动把自身所在目录加入临时搜索路径，并自动加载同目录下的这两个基础包。因此，即使从其他工作目录使用绝对路径执行 `Get`，也不需要事先单独加载两个基础包；如果它们已经加载，`Needs` 不会重复加载。

加载方式：

    Get[FileNameJoin[{project, "MixedSpinorBrackets.wl"}]];

### 混合外腿标签

为了避免只使用整数时无法判断某条腿是有质量还是无质量，新包使用显式腿标签：

    MasslessLeg[2]
    MassiveLeg[1, I]

其中 `MassiveLeg[1,I]` 的第二个参数是有质量粒子的 `SU(2)` little-group 指标。

混合括号写成：

    MixedAngle[MasslessLeg[2], MassiveLeg[1, I]]
    MixedSquare[MassiveLeg[4, J], MasslessLeg[3]]

对应：

$$
\langle 2\,1^I\rangle,
\qquad
[4_J\,3].
$$

### 混合动量链

无质量端点之间、插入有质量动量的链写成：

    MixedChain[
        MasslessLeg[2],
        {MassiveSpinorBrackets`mp[1]},
        MasslessLeg[3]
    ]

它表示：

$$
\langle 2|p_1|3]
=
\sum_{I=1}^{2}
\langle 2\,1^I\rangle[1_I\,3].
$$

`MixedSpinorExpand` 会显式展开 little-group 求和：

    MixedSpinorExpand[
        MixedChain[
            MasslessLeg[2],
            {MassiveSpinorBrackets`mp[1]},
            MasslessLeg[3]
        ]
    ]

结果结构为：

    MixedAngle[MasslessLeg[2], MassiveLeg[1, 1]]
        MixedSquare[MassiveLeg[1, 1], MasslessLeg[3]]
    + MixedAngle[MasslessLeg[2], MassiveLeg[1, 2]]
        MixedSquare[MassiveLeg[1, 2], MasslessLeg[3]]

### 混合数据格式

混合包的数据 association 包含 `Massless` 和 `Massive` 两部分。每一部分的内部格式与对应原包完全相同：

    mixedData = <|
        "Massless" -> <|
            "Lambda" -> masslessLambdas,
            "LambdaTilde" -> masslessLambdaTildes
        |>,
        "Massive" -> <|
            "Lambda" -> massiveLambdas,
            "LambdaTilde" -> massiveLambdaTildes
        |>
    |>;

也支持把每一部分写成原包使用的两个列表：

    mixedData = <|
        "Massless" -> {masslessLambdas, masslessLambdaTildes},
        "Massive" -> {massiveLambdas, massiveLambdaTildes}
    |>;

求值时使用：

    MixedSpinorEvaluate[
        MixedChain[
            MasslessLeg[2],
            {MassiveSpinorBrackets`mp[1]},
            MasslessLeg[3]
        ],
        mixedData
    ]

`MixedKinematicsCheck[mixedData]` 会分别调用 `SpinorKinematicsCheck` 和 `MassiveKinematicsCheck`，返回：

    <|
        "Massless" -> (...),
        "Massive" -> (...)
    |>

### Compton 振幅

`ComptonAmplitude` 提供截图中 tree-level Compton 公式的 stripped spinor 结构：

    ComptonAmplitude[
        0,
        {1, 2, 3, 4},
        {s, u, mass},
        g
    ]

表示：

$$
\mathcal M_0
=
\frac{g^2}{(s-m^2)(u-m^2)}
\langle 2|(p_1-p_4)|3]^2.
$$

自旋 `1/2` 需要一个入射和一个出射的 massive little-group 指标：

    ComptonAmplitude[
        1/2,
        {1, 2, 3, 4},
        {s, u, mass},
        g,
        {I, J}
    ]

它返回：

$$
\frac{g^2}{(s-m^2)(u-m^2)}
\langle 2|(p_1-p_4)|3]
\left(
\langle 2\,1^I\rangle[4_J\,3]
+\langle 2\,4^J\rangle[1_I\,3]
\right).
$$

自旋 `1` 的输入指标和输出指标分别是两个指标的列表：

    ComptonAmplitude[
        1,
        {1, 2, 3, 4},
        {s, u, mass},
        g,
        {{I1, I2}, {J1, J2}}
    ]

它返回两个自旋 `1/2` 型 `Y` 因子的乘积。需要注意：`s`、`u`、`mass` 和 `g` 在这个接口中是外部传入的运动学变量和耦合常数；新包不会擅自根据动量数据推断它们之间的关系。

## 输出渲染

三个包都为主要 spinor 对象定义了 MakeBoxes：

    ab[1, 2]
    sb[1, 2]
    mab[1, 1, 2, 2]
    msb[1, 1, 2, 2]

在 StandardForm 或 TraditionalForm 下会显示为相应的角括号、方括号、上下标和 bra-ket 结构。渲染只影响前端显示，不改变内部表达式。

混合包的对象例如：

    expr = {
        MixedAngle[MasslessLeg[2], MassiveLeg[1, I]],
        MixedSquare[MassiveLeg[4, J], MasslessLeg[3]],
        MixedChain[
            MasslessLeg[2],
            {MassiveSpinorBrackets`mp[1] - MassiveSpinorBrackets`mp[4]},
            MasslessLeg[3]
        ]
    };

在 notebook 中会显示为：

$$
\left\langle 2\,1^I\right\rangle,
\qquad
[4_J\,3],
\qquad
\langle 2|p_1-p_4|3].
$$

也可以显式使用 `MixedSpinorForm[expr]` 输出 TraditionalForm。显示格式不会改变内部表达式。

## 测试

项目包含 test_spinor_brackets.wl 和 test_massive_spinor_brackets.wl。在项目目录中运行：

    wolframscript -file test_spinor_brackets.wl
    wolframscript -file test_massive_spinor_brackets.wl
    wolframscript -file test_mixed_spinor_brackets.wl

测试覆盖开放旋量收缩、动量插入展开、动量和展开、反对称性、Mandelstam 展开、动量守恒、Schouten、宇称变换、BCFW shift、数值求值、质壳检查、little-group epsilon 约定、MakeBoxes 输出渲染，以及 mixed massive/massless 括号和 Compton 振幅结构。

## 文件结构

    SpinorBracketsProject/
    ├── SpinorBrackets.wl
    ├── MassiveSpinorBrackets.wl
    ├── MixedSpinorBrackets.wl
    ├── sample.wl
    ├── test_spinor_brackets.wl
    ├── test_massive_spinor_brackets.wl
    ├── test_mixed_spinor_brackets.wl
    └── README.md

## GitHub

项目仓库：

https://github.com/chenx-sir/SpinorBracketsProject
