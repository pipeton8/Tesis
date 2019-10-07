If[GetMatFile == True,Get[MatFileToGet],


Print["Setting up matrices."];

VtArgArrayEmpty = 
Table[0
,{i,Length[x0MatGrid]}
,{j,Length[hMatGrid]}
,{k,NumOfAggStates}
,{l,NumOfEmpStates}
,{m,Length[AggKGrid]}
];

OmegatArgArrayEmpty = 
Table[0
,{i,Length[s0MatGrid]}
,{j,Length[hMatGrid]}
,{k,NumOfAggStates}
,{l,NumOfEmpStates}
];


RScaledTimesVxtArgArrayEmpty = 
Table[0
,{i,Length[x0MatGrid]}
,{j,Length[hMatGrid]}
,{k,NumOfAggStates}
,{l,NumOfEmpStates}
,{m,Length[AggKGrid]}
,{n,Length[RScaledMatGrid]}
];

FOCwrtRiskySharetMatArgArrayEmpty = 
Table[0
,{i,Length[x0MatGrid]}
,{j,Length[hMatGrid]}
,{k,NumOfAggStates}
,{l,NumOfEmpStates}
,{m,Length[AggKGrid]}
,{n,Length[RGapMatGrid]}
];

Print["Constructing Vtp1ArgGridDistGivenOmegatAllArgArray."];
Vtp1ArgGridDistGivenOmegatAllArgArray = 
  Table[
  Vtp1ArgGridDistGivenOmegatAllArg[s0MatGrid[[i]],hMatGrid[[j]],AggStateGrid[[k]],EmpStateGrid[[l]],RiskyShareMatGrid[[m]]]
  ,{i,Length[s0MatGrid]}
  ,{j,Length[hMatGrid]}
  ,{k,NumOfAggStates}
  ,{l,NumOfEmpStates}
  ,{m,Length[RiskyShareMatGrid]}
];

(* Vtp1ArgGridDistGivenOmegatAllArgMatrix is not used in current programs, so don't create it *)
(*

Vtp1ArgGridDistGivenOmegatAllArgMatrix = 
Transpose[
  Partition[
    Flatten[
      Vtp1ArgGridDistGivenOmegatAllArgArray
    ],
  Length[x0MatGrid]*Length[hMatGrid]*NumOfAggStates*NumOfEmpStates*Length[AggKGrid]
  ]
];
*)

Print["Constructing RScaledTimesVxtArgGridDistGivenOmegatAllArgArray."];
RScaledTimesVxtArgGridDistGivenOmegatAllArgArray  = 
  Table[
  RScaledTimesVxtArgGridDistGivenOmegatAllArg[s0MatGrid[[i]],hMatGrid[[j]],AggStateGrid[[k]],EmpStateGrid[[l]],RiskyShareMatGrid[[m]]]
  ,{i,Length[s0MatGrid]}
  ,{j,Length[hMatGrid]}
  ,{k,NumOfAggStates}
  ,{l,NumOfEmpStates}
  ,{m,Length[RiskyShareMatGrid]}
];

(* RScaledTimesVxtArgGridDistGivenOmegatAllArgMatrix is not used in current programs, so don't create it *)
(*
RScaledTimesVxtArgGridDistGivenOmegatAllArgMatrix = 
Transpose[
  Partition[
    Flatten[
      RScaledTimesVxtArgGridDistGivenOmegatAllArgArray
    ],
  Length[x0MatGrid]*Length[hMatGrid]*NumOfAggStates*NumOfEmpStates*Length[AggKGrid]*Length[RScaledMatGrid]
  ]
];
*)
If[SolveForRiskyShare == True, 

Print["Constructing FOCwrtRiskySharetMatArgGridDistGivenOmegatAllArgArray."];
(*
FOCwrtRiskySharetMatArgGridDistGivenOmegatAllArgArray = 
  Table[
  FOCwrtRiskySharetMatArgGridDistGivenOmegatAllArg[s0MatGrid[[i]],hMatGrid[[j]],AggStateGrid[[k]],EmpStateGrid[[l]],RiskyShareMatGrid[[m]]]
  ,{i,Length[s0MatGrid]}
  ,{j,Length[hMatGrid]}
  ,{k,NumOfAggStates}
  ,{l,NumOfEmpStates}
  ,{m,Length[RiskyShareMatGrid]}
];
*)
FOCwrtRiskySharetMatArgGridDistGivenOmegatAllArgMatrix = 
  Partition[
    Flatten[
        Table[
        FOCwrtRiskySharetMatArgGridDistGivenOmegatAllArg[s0MatGrid[[i]],hMatGrid[[j]],AggStateGrid[[k]],EmpStateGrid[[l]],RiskyShareMatGrid[[m]]]
        ,{i,Length[s0MatGrid]}
        ,{j,Length[hMatGrid]}
        ,{k,NumOfAggStates}
        ,{l,NumOfEmpStates}
        ,{m,Length[RiskyShareMatGrid]}
      ]
    ],
  Length[x0MatGrid]*Length[hMatGrid]*NumOfAggStates*NumOfEmpStates*Length[AggKGrid]*Length[RGapMatGrid]
  ];
]; (* End If SolveForRiskyShare *)


MatricesExist = True;

(*
ChangeOrMakeDirectory[DirName_] := Block[{ResultOfCreate},
  ResultOfCreate = Check[CreateDirectory[DirName],AlreadyExists,CreateDirectory::privv];
  If[ResultOfCreate == AlreadyExists,SetDirectory[DirName]]];
*)
(*
DeleteFile[":TransitionStuff:Matrices:tempshock.sav"];
DumpSave[":TransitionStuff:Matrices:tempshock.sav",{
DiscountFactor,beta,
xmin,xmax,smin,smax,hmin,hmax,x0MatGrid,s0MatGrid,hMatGrid,NumOfxMatSteps,
    NumOfsMatSteps,NumOfhMatSteps,NumOfetVals,NumOfepVals,NumOfepValsPerEmpState
NumOfAggStates,NumOfEmpStates,NumOfRiskyShareMatSteps,NumOfRVals,catchup,
    Cutoff,RCutoff,etSig, 
etVals,epVals,etProb,epProb,epSig,RSig,EquityPremium,Rcertain,etZeroProb,etZeroProbVals,
    etUnempProb,UnempWage,EmpStateTransitionMatrix, 
CumEmpStateTransitionMatrix,AggStateTransitionMatrix,
    CumAggStateTransitionMatrix, 
RGapMatGrid,RScaledMatGrid,G,R
,SolveForRiskyShare
,FOCwrtRiskySharetMatArgGridDistGivenOmegatAllArgMatrix
,FOCwrtRiskySharetMatArgGridDistGivenOmegatAllArgArray
,RScaledTimesVxtArgGridDistGivenOmegatAllArgMatrix
,RScaledTimesVxtArgGridDistGivenOmegatAllArgArray
,Vtp1ArgGridDistGivenOmegatAllArgArray
,Vtp1ArgGridDistGivenOmegatAllArgMatrix
,RGapTimesVxtArgGridDistGivenOmegatAllArgArray
,RGapTimesVxtArgGridDistGivenOmegatAllArgMatrix
,MatricesExist
,VtArgArrayEmpty
,RScaledTimesVxtArgArrayEmpty
,FOCwrtRiskySharetMatArgArrayEmpty
,OmegatArgArrayEmpty
}
];
*)
];
