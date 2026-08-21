# 四点康普顿散射

本目录给出四点、树级、最小耦合康普顿散射振幅的可复现 Wolfram Language 遍历程序。过程包含两条等质量的有质量物质外腿和两条无质量玻色子外腿；计算采用 massive spinor-helicity 变量，同时导出便于阅读的 Markdown 报告与可直接加载的 Wolfram Language 精确结果。

## 文件说明

- `enumerate_four_point_compton.wl`：QED、Yang--Mills 和 gravity 的独立遍历器。
- `run_default_enumeration.wl`：运行默认符号配置并导出结果的脚本。
- `all_local_compton_amplitudes.md`：已生成的可视化符号结果，列出全部外态分量。
- `all_local_compton_amplitudes.wl`：已生成的精确符号结果，可由 Mathematica/Wolfram Language 读取。
- `all_local_compton_amplitudes_summary.md`：分量数目的简要汇总。
- `../MixedSpinorBrackets.wl`：父项目提供的混合有质量/无质量 spinor-bracket 实现。

## 复现计算

克隆父项目后，在本目录运行默认配置：

```sh
git clone https://github.com/chenx-sir/SpinorBracketsProject.git
cd SpinorBracketsProject/Campton_Scattering
wolframscript -file run_default_enumeration.wl
```

该命令会重新生成下列文件：

- `all_local_compton_amplitudes.wl`
- `all_local_compton_amplitudes.md`
- `all_local_compton_amplitudes_summary.md`

默认配置遍历：QED 中 $ S=0,\frac{1}{2},1 $ 的物质；二维示例表示下 Yang--Mills 中 $ S=0,\frac{1}{2},1 $ 的物质；以及 gravity 中 $ S=0,\frac{1}{2},1,\frac{3}{2},2 $ 的物质。相应地产生 14 个 QED、56 个 Yang--Mills 和 55 个 gravity 外态分量。

## 振幅约定

外腿顺序固定为

$$
(1^S,2^{+h},3^{-h},4^S)
=(\text{massiveIn},\text{bosonPlus},\text{bosonMinus},\text{massiveOut}).
$$

令 $ s,u,m $ 为输入的 Mandelstam 变量和物质质量，并取

$$
t=2m^2-s-u.
$$

程序实现的闭式为

$$
\mathcal{M}_4=
\frac{X^{2h-2S}\displaystyle\prod_{r=1}^{2S}N_{I_rJ_r}}
     {(s-m^2)(u-m^2)}\,K,
\qquad
X=\langle3|p_1-p_4|2],
$$

$$
N_{IJ}=\langle4^J3\rangle[1^I2]
+\langle1^I3\rangle[4^J2].
$$

其中，QED 和 Yang--Mills 取 $ h=1 $，gravity 取 $ h=2 $； $ I,J=1,2 $ 是 massive SU(2) little-group 指标。对于自旋 $ S $ 的物质，每条有质量外腿为 rank 等于 $ 2S $ 的对称张量，因此程序逐一列出 $ (2S+1)^2 $ 个独立的入射/出射指标分量。

三种理论的核为

$$
K_{\mathrm{QED}}=e^2q^2,
$$

$$
K_{\mathrm{YM}}=
\frac{g^2}{t}
\left[(u-m^2)(T^aT^b)_{ij}+(s-m^2)(T^bT^a)_{ij}\right],
$$

$$
K_{\mathrm{Gravity}}=-\frac{\kappa^2}{t}.
$$

电子、 $ \mu $ 子等具体粒子名称不是输入参数；在最小耦合近似下，它们由自旋 $ S $、质量 $ m $ 和电荷 $ q $（Yang--Mills 情形还包括表示矩阵 $ T^a,T^b $）确定。

## 运行环境

- 安装 Mathematica 或 Wolfram Engine，并可使用 `wolframscript`。

已提交的结果为符号表达式，不要求提供数值运动学点。若需要数值计算，可按 `enumerate_four_point_compton.wl` 中的说明向 `EnumerateFourPointCompton` 传入兼容的 `spinorData`。
