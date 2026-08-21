# 四点 Compton 散射的统一 On-Shell 表达式

本文整理 Arkani-Hamed, Huang, Huang, *Scattering Amplitudes For All Masses and Spins* (arXiv:1709.04891v2) 中的四点 Compton 振幅。讨论限于四维、树级、on-shell、minimal coupling。

## 约定

采用全出射约定：

\[
(\mathbf{1}^{S}_i,\,B_2^{A,+h},\,B_3^{B,-h},\,\mathbf{4}^{S}_j),
\qquad m_1=m_4=m.
\]

- \(\mathbf{1},\mathbf{4}\) 是同质量的 massive spin-\(S\) 粒子；粗体表示省略的 massive \(SU(2)\) little-group 指标。
- \(B_2,B_3\) 是 helicity 分别为 \(+h,-h\) 的无质量规范玻色子或引力子。
- \(i,j\) 是 massive 粒子的内部表示指标；\(A,B\) 是无质量玻色子的内部指标。

定义共同的 spinor-kinematic 结构：

\[
X\equiv\langle3|p_1|2],
\qquad
N\equiv
\langle\mathbf{4}3\rangle[\mathbf{1}2]
+\langle\mathbf{1}3\rangle[\mathbf{4}2].
\]

## 统一闭式

三类 Compton 振幅可写为

\[
\boxed{
\mathcal M^{\mathcal G}_{S,h}
(\mathbf1_i^S,B_2^{A,+h},B_3^{B,-h},\mathbf4_j^S)
=
\frac{X^{2h-2S}N^{2S}}{(s-m^2)(u-m^2)}
\,\mathcal K_{\mathcal G;ij}^{AB}
}
\]

其中 \(\mathcal G\) 指定相互作用，\(\mathcal K\) 是理论相关的极点与内部量子数结构。

| 理论 | 外部无质量粒子 | \(h\) | 相互作用核 \(\mathcal K\) |
| --- | --- | ---: | --- |
| Abelian gauge theory | \(\gamma_2^+,\gamma_3^-\) | 1 | \(e^2q^2\,\delta_{ij}\) |
| Yang--Mills theory | \(g_2^{a,+},g_3^{b,-}\) | 1 | \(\displaystyle \frac{g^2}{t}\big[(u-m^2)(T^aT^b)_{ij}+(s-m^2)(T^bT^a)_{ij}\big]\) |
| Einstein gravity | \(h_2^{+2},h_3^{-2}\) | 2 | \(\displaystyle-\frac{\kappa^2}{t}\delta_{ij}\), \(\kappa=M_{\rm Pl}^{-1}\) |

耦合常数的位置是约定相关的。原文的部分公式将整体规范耦合吸收到三点顶点或生成元 \(T^a\) 的定义中。

## 三种显式形式

### Abelian Compton

\[
\boxed{
\mathcal M_{\gamma\gamma}
=
e^2q^2\,
\frac{X^{2-2S}N^{2S}}{(s-m^2)(u-m^2)}
}
\]

这是原文式 (5.20) 的带耦合常数写法。

### 非 Abelian Compton

\[
\boxed{
\mathcal M_{gg,ij}^{ab}
=
g^2\,
\frac{X^{2-2S}N^{2S}}{t}
\left[
\frac{(T^aT^b)_{ij}}{s-m^2}
+
\frac{(T^bT^a)_{ij}}{u-m^2}
\right]
}
\]

这是原文式 (5.23) 的形式。\(t\)-道极点来自三胶子顶点；其存在与

\[
[T^a,T^b]=f^{abc}T^c
\]

共同保证三道因子化。

### Gravi-Compton

\[
\boxed{
\mathcal M_{hh}
=
-\frac{\kappa^2X^{4-2S}N^{2S}}
{(s-m^2)(u-m^2)t}
}
\]

这是原文在 "Graviton Compton Scattering" 小节给出的 general-spin minimal-coupling 结果（式 (5.36) 附近）。

## 可取的外部粒子与自旋范围

| 过程 | massive 粒子 \(1,4\) 的例子 | 无质量粒子 \(2,3\) 的例子 | 此单项闭式局域的 \(S\) |
| --- | --- | --- | --- |
| Abelian Compton | \(S=0\)：带电标量；\(S=1/2\)：电子、\(\mu\) 子、带电 \(\tau\)、带电夸克；\(S=1\)：\(W^\pm\) 或带电 vector matter | 光子 | \(0,1/2,1\) |
| 非 Abelian Compton | \(S=0\)：colored scalar；\(S=1/2\)：夸克或其他带色 fermion；\(S=1\)：带色 massive vector matter | 胶子 | \(0,1/2,1\) |
| Gravi-Compton | \(S=0\)：标量；\(S=1/2\)：电子、夸克、质量非零的中微子、Dirac/Majorana fermion；\(S=1\)：\(W\)、\(Z\)；\(S=3/2\)：gravitino；\(S=2\)：massive spin-2 resonance | 引力子 | \(0,1/2,1,3/2,2\) |

## 对费米子的具体条件

| 过程 | 哪些 massive spin-\(1/2\) 费米子适用 | 不直接适用的情形 |
| --- | --- | --- |
| Abelian Compton | 任何带相应 \(U(1)\) 电荷、具有 minimal coupling 的 massive Dirac fermion，例如电子、\(\mu\) 子、带电夸克 | 电中性中微子；包含独立 Pauli 项、异常磁矩或电偶极矩的顶点 |
| 非 Abelian Compton | 属于该非 Abelian 规范群表示的 fermion；QCD 中为夸克及带色新 fermion | 电子、\(\mu\) 子、无色中微子；不带该规范荷的粒子 |
| Gravi-Compton | 任何 massive Dirac 或 Majorana spin-\(1/2\) 粒子，因为它们都最小地耦合到能量动量张量 | 严格无质量极限；含额外曲率耦合或高维引力算符的理论 |

## 范围与限制

1. 上述结果仅给出 minimal coupling 的 pole part。非最小多极矩、异常磁矩、flavor-changing vertex 和独立四点 contact term 必须另行加入。
2. \(S>1\) 的 photon/gluon 公式、以及 \(S>2\) 的 graviton 公式会出现赝极点，不能直接视作完整的局域振幅。原文对 \(S=3/2\) 的 photon Compton 例子单独重组为局域表达式，但没有给出任意高自旋的一行统一局域公式。
3. 本文写出的外部 helicity 为 \((+h,-h)\)。宇称共轭 \((-h,+h)\) 由 angle/square brackets 交换得到；同 helicity 的分量不由这一个式子完整描述。
4. \(m_1=m_4\) 是上述紧凑形式的前提。不同质量的非弹性过程需要从一般三点振幅粘合重新构造。

## 来源

N. Arkani-Hamed, T.-C. Huang, Y.-t. Huang, *Scattering Amplitudes For All Masses and Spins*, arXiv:1709.04891v2。

- Section 5, "Four Particle Amplitudes For Massive Particles"。
- "Compton Scattering For \(S\leq1\)": Abelian 式 (5.20)、非 Abelian 式 (5.23)。
- "Graviton Compton Scattering": general-spin gravi-Compton 闭式。

## 独立遍历脚本

`enumerate_four_point_compton.wl` 是独立于包实现的 Mathematica 脚本。它加载 `../SpinorBracketsProject/MixedSpinorBrackets.wl`，并调用已提交的 `UnifiedComptonAmplitude`，不会修改任何包文件。

```wl
Get["enumerate_four_point_compton.wl"];

results = EnumerateFourPointCompton[exampleConfiguration];
ComptonEnumerationSummary[results]
```

默认示例会遍历每种理论的全部局域 minimal-coupling 情形：

```text
QED        -> 14  = 1 + 4 + 9
YangMills  -> 56  = 4 color components x (1 + 4 + 9)
Gravity    -> 55  = 1 + 4 + 9 + 16 + 25
```

其中每个数字是外部 massive 自旋分量的数量。QED/Yang--Mills 分别遍历 \(S=0,1/2,1\)；gravity 遍历 \(S=0,1/2,1,3/2,2\)。Yang--Mills 示例使用二维表示，因此额外遍历四个 \((i,j)\) 颜色分量。

结果按理论和 spin 分组，每一个记录包含：

```wl
<|
    "Spin" -> spin,
    "LittleGroupIndices" -> {inIndices, outIndices},
    "MatterIndices" -> {i, j}, (* Yang--Mills only *)
    "Amplitude" -> expression
|>
```

传入完整的 mixed spinor 数据后，遍历器会对每个记录执行数值求值：

```wl
numericResults = EnumerateFourPointCompton[
    exampleConfiguration,
    spinorData
];
```
