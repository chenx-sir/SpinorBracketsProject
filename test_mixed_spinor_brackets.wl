Get[FileNameJoin[{DirectoryName[$InputFileName], "MixedSpinorBrackets.wl"}]];

masslessData = {
    {{1, 0}, {0, 1}, {1, 1}},
    {{1, 0}, {0, 1}, {1, -1}}
};

massiveData = {
    {
        {{1, 0}, {0, 1}},
        {{1, 1}, {2, 3}},
        {{1, 2}, {2, 4}},
        {{1, 3}, {2, 5}}
    },
    {
        {{2, 0}, {0, 2}},
        {{1, 2}, {3, 5}},
        {{1, 1}, {2, 3}},
        {{2, 1}, {4, 6}}
    }
};

mixedData = <|
    "Massless" -> masslessData,
    "Massive" -> massiveData
|>;

invariants = {17, 29, 2};
coupling = 3;

qedCouplings = <|"e" -> e, "Charge" -> q|>;
yangMillsCouplings = <|"g" -> g|>;
gravityCouplings = <|"kappa" -> kappa|>;
ta = {{0, 1}, {1, 0}};
tb = {{1, 0}, {0, -1}};
yangMillsInternal = <|
    "Ta" -> ta,
    "Tb" -> tb,
    "MatterIndices" -> {1, 2}
|>;

unifiedX = MixedSpinorBrackets`MixedChain[
    MixedSpinorBrackets`MasslessLeg[3],
    {MassiveSpinorBrackets`mp[1] - MassiveSpinorBrackets`mp[4]},
    MixedSpinorBrackets`MasslessLeg[2]
];
unifiedN[inIndex_, outIndex_] :=
    MixedSpinorBrackets`MixedAngle[
        MixedSpinorBrackets`MassiveLeg[4, outIndex],
        MixedSpinorBrackets`MasslessLeg[3]
    ] * MixedSpinorBrackets`MixedSquare[
        MixedSpinorBrackets`MassiveLeg[1, inIndex],
        MixedSpinorBrackets`MasslessLeg[2]
    ] +
    MixedSpinorBrackets`MixedAngle[
        MixedSpinorBrackets`MassiveLeg[1, inIndex],
        MixedSpinorBrackets`MasslessLeg[3]
    ] * MixedSpinorBrackets`MixedSquare[
        MixedSpinorBrackets`MassiveLeg[4, outIndex],
        MixedSpinorBrackets`MasslessLeg[2]
    ];

angle11 = MixedSpinorBrackets`MixedAngle[
    MixedSpinorBrackets`MasslessLeg[1],
    MixedSpinorBrackets`MassiveLeg[1, 1]
];
angle12 = MixedSpinorBrackets`MixedAngle[
    MixedSpinorBrackets`MasslessLeg[1],
    MixedSpinorBrackets`MassiveLeg[1, 2]
];
square11 = MixedSpinorBrackets`MixedSquare[
    MixedSpinorBrackets`MassiveLeg[1, 1],
    MixedSpinorBrackets`MasslessLeg[2]
];
square12 = MixedSpinorBrackets`MixedSquare[
    MixedSpinorBrackets`MassiveLeg[1, 2],
    MixedSpinorBrackets`MasslessLeg[2]
];

chain = MixedSpinorBrackets`MixedChain[
    MixedSpinorBrackets`MasslessLeg[1],
    {MassiveSpinorBrackets`mp[1]},
    MixedSpinorBrackets`MasslessLeg[2]
];

tests = {
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorExpand[chain],
        angle11 square11 + angle12 square12,
        TestID -> "massive momentum mixed-chain expansion"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[angle11, mixedData],
        0,
        TestID -> "mixed angle bracket evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[angle12, mixedData],
        1,
        TestID -> "second mixed angle bracket evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[square11, mixedData],
        2,
        TestID -> "mixed square bracket evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[square12, mixedData],
        0,
        TestID -> "second mixed square bracket evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[chain, mixedData],
        0,
        TestID -> "mixed chain matrix evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[
            MixedSpinorBrackets`MixedSpinorExpand[chain],
            mixedData
        ],
        0,
        TestID -> "expanded mixed chain evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[
            SpinorBrackets`ab[1, 2],
            mixedData
        ],
        1,
        TestID -> "delegated massless bracket evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[
            MassiveSpinorBrackets`mab[1, 1, 2, 1],
            mixedData
        ],
        1,
        TestID -> "delegated massive bracket evaluation"
    ],
    VerificationTest[
        Head[
            MixedSpinorBrackets`MixedKinematicsCheck[mixedData]
        ],
        Association,
        TestID -> "delegated mixed kinematics check"
    ],
    VerificationTest[
        Head[
            MixedSpinorBrackets`ComptonAmplitude[
                1/2,
                {1, 2, 3, 4},
                {s, u, m},
                g,
                {1, 2}
            ]
        ],
        Times,
        TestID -> "Compton spin one-half structure"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[
            MixedSpinorBrackets`MixedSpinorExpand[
                MixedSpinorBrackets`ComptonAmplitude[
                    0,
                    {1, 2, 3, 4},
                    invariants,
                    coupling
                ]
            ],
            mixedData
        ],
        3969/325,
        TestID -> "Compton scalar numerical evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[
            MixedSpinorBrackets`MixedSpinorExpand[
                MixedSpinorBrackets`ComptonAmplitude[
                    1/2,
                    {1, 2, 3, 4},
                    invariants,
                    coupling,
                    {1, 2}
                ]
            ],
            mixedData
        ],
        -2646/325,
        TestID -> "Compton spin one-half numerical evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`MixedSpinorEvaluate[
            MixedSpinorBrackets`MixedSpinorExpand[
                MixedSpinorBrackets`ComptonAmplitude[
                    1,
                    {1, 2, 3, 4},
                    invariants,
                    coupling,
                    {{1, 2}, {1, 2}}
                ]
            ],
            mixedData
        ],
        36/65,
        TestID -> "Compton spin one numerical evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`UnifiedComptonAmplitude[
            "QED",
            1/2,
            {1, 2, 3, 4},
            {s, u, m},
            qedCouplings,
            {{I}, {J}}
        ],
        e^2 q^2 unifiedX unifiedN[I, J] / ((s - m^2) (u - m^2)),
        TestID -> "unified QED spin one-half structure"
    ],
    VerificationTest[
        Simplify[
            MixedSpinorBrackets`UnifiedComptonAmplitude[
                "YangMills",
                0,
                {1, 2, 3, 4},
                {s, u, m},
                yangMillsCouplings,
                Automatic,
                yangMillsInternal
            ]
        ],
        Simplify[
            (
                g^2 unifiedX^2 ((u - m^2) Dot[ta, tb] + (s - m^2) Dot[tb, ta]) /
                    (t (s - m^2) (u - m^2))
            )[[1, 2]] /. t -> 2 m^2 - s - u
        ],
        TestID -> "unified Yang-Mills scalar color component"
    ],
    VerificationTest[
        MixedSpinorBrackets`UnifiedComptonAmplitude[
            "Gravity",
            3/2,
            {1, 2, 3, 4},
            {s, u, m},
            gravityCouplings,
            {{I1, I2, I3}, {J1, J2, J3}}
        ],
        -kappa^2 unifiedX unifiedN[I1, J1] unifiedN[I2, J2] unifiedN[I3, J3] /
            ((2 m^2 - s - u) (s - m^2) (u - m^2)),
        TestID -> "unified gravity spin three-halves structure"
    ],
    VerificationTest[
        NumberQ[
            MixedSpinorBrackets`MixedSpinorEvaluate[
                MixedSpinorBrackets`UnifiedComptonAmplitude[
                    "QED",
                    1/2,
                    {1, 2, 3, 4},
                    invariants,
                    <|"e" -> coupling, "Charge" -> -1|>,
                    {{1}, {2}}
                ],
                mixedData
            ]
        ],
        True,
        TestID -> "unified QED numerical evaluation"
    ],
    VerificationTest[
        NumberQ[
            MixedSpinorBrackets`MixedSpinorEvaluate[
                MixedSpinorBrackets`UnifiedComptonAmplitude[
                    "YangMills",
                    0,
                    {1, 2, 3, 4},
                    invariants,
                    <|"g" -> coupling|>,
                    Automatic,
                    yangMillsInternal
                ],
                mixedData
            ]
        ],
        True,
        TestID -> "unified Yang-Mills numerical evaluation"
    ],
    VerificationTest[
        NumberQ[
            MixedSpinorBrackets`MixedSpinorEvaluate[
                MixedSpinorBrackets`UnifiedComptonAmplitude[
                    "Gravity",
                    3/2,
                    {1, 2, 3, 4},
                    invariants,
                    <|"kappa" -> coupling|>,
                    {{1, 2, 1}, {2, 1, 2}}
                ],
                mixedData
            ]
        ],
        True,
        TestID -> "unified gravity numerical evaluation"
    ],
    VerificationTest[
        MixedSpinorBrackets`UnifiedComptonAmplitude[
            "QED",
            3/2,
            {1, 2, 3, 4},
            {s, u, m},
            qedCouplings,
            {{1, 1, 1}, {1, 1, 1}}
        ],
        $Failed,
        {MixedSpinorBrackets`UnifiedComptonAmplitude::spin},
        TestID -> "unified QED rejects nonlocal spin"
    ]
};

report = TestReport[tests];
Print["Succeeded: ", Length[report["TestsSucceeded"]]];
Print["Failed: ", Total[Length /@ Values[report["TestsFailed"]]]];
If[Length[report["TestsFailed"]] =!= 0, Print[report["TestsFailed"]]; Exit[1]];
