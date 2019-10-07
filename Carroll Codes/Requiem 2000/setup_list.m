If[GetListFile == True,Get[ListFileToGet],


Print["Setting up lists."];

Print["Constructing Vtp1ArgListDistGivenOmegatAllArgArray."];
Vtp1ArgListDistGivenOmegatAllArgArray = 
  Table[
  Vtp1ArgListDistGivenOmegatAllArgFunc[s0ListGrid[[i]],hListGrid[[j]],AggStateGrid[[k]],EmpStateGrid[[l]],AggSGrid[[m]],RiskyShareListGrid[[n]]]
  ,{i,Length[s0ListGrid]}
  ,{j,Length[hListGrid]}
  ,{k,NumOfAggStates}
  ,{l,NumOfEmpStates}
  ,{m,Length[AggSGrid]}
  ,{n,Length[RiskyShareListGrid]}
];

Vtp1ArgListDistGivenOmegatAllArg = Map[Apply[Vtp1ArgListDistGivenOmegatAllArgFunc, #] &,Vtp1ArgListDistGivenOmegatAllArgArgArray];



Print["Constructing RScaledTimesVxtArgListDistGivenOmegatAllArgArray."];
RScaledTimesVxtArgListDistGivenOmegatAllArgArray  = 
  Table[
  RScaledTimesVxtArgListDistGivenOmegatAllArgFunc[s0ListGrid[[i]],hListGrid[[j]],AggStateGrid[[k]],EmpStateGrid[[l]],AggSGrid[[m]],RiskyShareListGrid[[n]]]
  ,{i,Length[s0ListGrid]}
  ,{j,Length[hListGrid]}
  ,{k,NumOfAggStates}
  ,{l,NumOfEmpStates}
  ,{m,Length[AggSGrid]}
  ,{n,Length[RiskyShareListGrid]}
];

RScaledTimesVxtArgListDistGivenOmegatAllArg = Map[Apply[RScaledTimesVxtArgListDistGivenOmegatAllArgFunc, #] &,RScaledTimesVxtArgListDistGivenOmegatAllArgArgArray];

If[SolveForRiskyShare == True,
  Print["Constructing RGapTimesVxtArgListDistGivenOmegatAllArgArray."];
  RGapTimesVxtArgListDistGivenOmegatAllArgArray = 
    Table[
    RGapTimesVxtArgListDistGivenOmegatAllArgFunc[s0ListGrid[[i]],hListGrid[[j]],AggStateGrid[[k]],EmpStateGrid[[l]],AggSGrid[[m]],RiskyShareListGrid[[n]]]
    ,{i,Length[s0ListGrid]}
    ,{j,Length[hListGrid]}
    ,{k,NumOfAggStates}
    ,{l,NumOfEmpStates}
    ,{m,Length[AggSGrid]}
    ,{n,Length[RiskyShareListGrid]}
  ];
  RGapTimesVxtArgListDistGivenOmegatAllArg = Map[Apply[RGapTimesVxtArgListDistGivenOmegatAllArgFunc, #] &,RGapTimesVxtArgListDistGivenOmegatAllArgArgArray];
];   (* End If *)

ListsExist = True;
(*
ChangeOrMakeDirectory[DirName_] := Block[{ResultOfCreate},
  ResultOfCreate = Check[CreateDirectory[DirName],AlreadyExists,CreateDirectory::privv];
  If[ResultOfCreate == AlreadyExists,SetDirectory[DirName]]];
*)
(*
DeleteFile[":TransitionStuff:Lists:tempshock.sav"];
DumpSave[":TransitionStuff:Lists:tempshock.sav",{
DiscountFactor,beta,
xmin,xmax,smin,smax,hmin,hmax,x0ListGrid,s0ListGrid,hListGrid,NumOfxListSteps,NumOfsListSteps,NumOfhListSteps,NumOfetVals,NumOfepVals,NumOfepValsPerEmpState ,
NumOfAggStates,NumOfEmpStates,NumOfRiskyShareListSteps,NumOfRVals,catchup,Cutoff,RCutoff,etSig, 
etVals,epVals,etProb,epProb,epSig,RSig,EquityPremium,Rcertain,etZeroProb,etZeroProbVals,etUnempProb,UnempWage,EmpStateTransitionMatrix, 
CumEmpStateTransitionMatrix,AggStateTransitionMatrix,CumAggStateTransitionMatrix, 
RGapListGrid,RScaledListGrid,G,R
,SolveForRiskyShare
,FOCwrtRiskySharetListArgListDistGivenOmegatAllArgMatrix
,FOCwrtRiskySharetListArgListDistGivenOmegatAllArgArray
,RScaledTimesVxtArgListDistGivenOmegatAllArgMatrix
,RScaledTimesVxtArgListDistGivenOmegatAllArgArray
,Vtp1ArgListDistGivenOmegatAllArgArray
,Vtp1ArgListDistGivenOmegatAllArgMatrix
,RGapTimesVxtArgListDistGivenOmegatAllArgArray
,RGapTimesVxtArgListDistGivenOmegatAllArgMatrix
,ListsExist
}
];
*)
]
