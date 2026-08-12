BeginPackage["SpinorUtilities`"];

Unprotect[SpinorDeterminant2];

SpinorDeterminant2::usage =
    "SpinorDeterminant2[u, v] 计算两个二维旋量的行列式。";

SpinorDeterminant2[u_List, v_List] /;
    Length[u] == Length[v] == 2 :=
    Det[{u, v}];

Protect[SpinorDeterminant2];
EndPackage[];
