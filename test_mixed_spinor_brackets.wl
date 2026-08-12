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
    ]
};

report = TestReport[tests];
Print["Succeeded: ", Length[report["TestsSucceeded"]]];
Print["Failed: ", Total[Length /@ Values[report["TestsFailed"]]]];
If[Length[report["TestsFailed"]] =!= 0, Print[report["TestsFailed"]]; Exit[1]];
