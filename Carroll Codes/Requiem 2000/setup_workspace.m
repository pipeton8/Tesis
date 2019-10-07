(*
% This program reads in the following packages:
% 	DiscreteMath'Permutations'
% 	Statistics'DataManipulation'
% It also sets up the routines that initialize the variables,
% and takes care of housekeeping preliminaries 
*)

<<ticks.m;

If[$MachineType == "Macintosh",ParentDirectoryString = "::",ParentDirectoryString = ".."];
If[$MachineType == "Macintosh",SubDirectoryString = ":",SubDirectoryString = "/"];
If[$MachineType == "PC"       ,SubDirectoryString = "\\"];


Print["Setting up the background routines"];

(* % Define the probability of moving to FutureState next period if in CurrentState today *)

AggStateProb[CurrentState_,FutureState_] := AggStateTransitionMatrix[[CurrentState,FutureState]];

ClearAll[EmpStateProb];
EmpStateProb[AggState_,EmpStateCurrent_,EmpStateFuture_] := 
  EmpStateTransitionMatrix[[AggState,EmpStateCurrent,EmpStateFuture]];
EmpStateEmployed = 1;

(* % Finds the position number of the list item with greatest first element *)
ClearAll[MaxPos]; 
MaxPos[list_] := Block[{MaxSoFar = 1}, 
   Do[If[list[[ListPos,1]]>list[[MaxSoFar,1]],MaxSoFar=ListPos],{ListPos,Length[list]}];MaxSoFar];

(* % Returns the maximum value in a list *)
ClearAll[MaxList]; 
MaxList[list_] := list[[MaxPos[list]]];

(* % Adds a value corresponding to each set of values in the state grid *)
ClearAll[AddOutcome]; 
AddOutcome[StateGrid_,ValuesToAdd_] := Transpose[Join[Transpose[StateGrid],{ValuesToAdd}]];

Print["Initializing the background variables"];


(*# % Assign position numbers to each state variable used for Vt *)
xtVtPos                      = 1;
htStartVtPos                 = 2;
AggStateVtPos                = 3;
EmpStateVtPos                = 4;
AggKtVtPos                 = 5;
LifePosnVtPos                = 6;

(*# Assign position numbers to each state variable used for input to EtVtp1 *)
stEtVtp1Pos                = 1;
htEndEtVtp1Pos             = 2;
AggStateEtVtp1Pos          = 3;
EmpStateEtVtp1Pos          = 4;
RiskyShareEtVtp1Pos        = 5; (*# Position of the variable indicating fraction of portfolio in risky asset *)
LifePosnEtVtp1Pos          = 6;

(*# Assign position numbers to each state variable used for input to OmegaInvt *)
stOmegaInvtPos                = 1;
htEndOmegaInvtPos             = 2;
AggStateOmegaInvtPos          = 3;
EmpStateOmegaInvtPos          = 4;
LifePosnOmegaInvtPos          = 5;

(* % Assign position numbers to each control variable in the output list from FindBest *)
VtPosVtOut      = 1;
ChoicetPosVtOut = 2;
ctPosVtOut      = 3;
htEndPosVtOut   = 4;
stPosVtOut      = 5;
RiskySharetPosVtOut = 6; (*# Position of risky share variable in output list *)

(* # Assign position numbers to each variable in the output of OmegaInvt *)
RiskySharetOmegaInvtPosOut = 1;

GreatBadness    = 1000*(epsilon^(1-rho))/(1-rho);
AvoidLikePlague = 1000*epsilon^-rho;

(* % Verbose output will display various status message throughout the 
   % execution of the program *)
VerboseOutput = True;

(* % Read in necessary packages *)
<<DiscreteMath`Permutations`
<<Statistics`DataManipulation`
<<Statistics`DescriptiveStatistics`
<<Statistics`ContinuousDistributions`
<<Statistics`LinearRegression`
<<Graphics`Legend`


(* % Finds the eigenvector that represents the steady-state distribution of 
% outcomes for a given probability transition matrix.  It will be the 
% eigenvector that corresponds to an eigenvalue of 1. *)
Clear[FindRightEigenVec];
FindRightEigenVec[TransitionMatrix_] := Block[{},RightEigenVec = -1;
{EigenVals,EigenVecs} = Eigensystem[TransitionMatrix];
Do[
If[EigenVals[[i]] == 1.,
    (* then *) RightEigenVec = EigenVecs[[i]]];
,{i,Length[EigenVals]}];
If[RightEigenVec == -1, 
  (* then *) Print["No suitable Eigenvectors exist."]];
Return[Chop[RightEigenVec / Last[CumulativeSums[RightEigenVec]]]]
];

AllSameSign[list_] := Block[{LoopOverList,SignOfFirstNonzero=0},
  Do[
    SignOfThisOne = Sign[list[[LoopOverList]]];
    If[SignOfThisOne != 0,
        If[SignOfFirstNonzero == 0,SignOfFirstNonzero=SignOfThisOne];
		If[SignOfFirstNonzero != SignOfThisOne, Return[False]];
    ]; (* End If SignOfThisOne != 0 *)
  ,{LoopOverList,Length[list]}];
  If[SignOfFirstNonzero != 0,Return[True],Return[False]]
];

(* % Sets up easy-to-use routine for doing linear regressions *)
ClearAll[MyReg];
MyReg[DepVarName_,IndepVarNames_] := Block[{DepVar = ToExpression[DepVarName]},
Print["Dependent Variable "<>DepVarName];
Outstuff = 
  Regress[
    Transpose[Join[ToExpression[IndepVarNames],{DepVar}]]
,ToExpression[Table["x"<>ToString[i],{i,Length[IndepVarNames]}]]
,ToExpression[Table["x"<>ToString[i],{i,Length[IndepVarNames]}]]
,BasisNames->Join[{"Constant"},IndepVarNames]
,RegressionReport->{ParameterTable,AdjustedRSquared,DurbinWatsonD,BestFitParameters}];
Print[ParameterTable /. Outstuff];
Print[""];
Print["R-Bar:         ",AdjustedRSquared /. Outstuff];
Print["Durbin-Watson: ",DurbinWatsonD   /. Outstuff];
Print[""];
Print[""];
Return[Outstuff]];

(* % Performs linear regressions using only a given range of values *)
ClearAll[MyRegRange];
MyRegRange[DepVar_,IndepVarNames_,RangeToUse_] := Block[{},
SetOptions[$Output,PageWidth->Infinity];
Outstuff = 
  Regress[
    Take[Transpose[Join[ToExpression[IndepVarNames],{DepVar}]],RangeToUse]
,ToExpression[Table["x"<>ToString[i],{i,Length[IndepVarNames]}]]
,ToExpression[Table["x"<>ToString[i],{i,Length[IndepVarNames]}]]
,BasisNames->Join[{"Constant"},IndepVarNames]
,RegressionReport->{ParameterTable,AdjustedRSquared,DurbinWatsonD}];
Print[ParameterTable /. Outstuff];
Print[""];
Print["R-Bar:         ",AdjustedRSquared /. Outstuff];
Print["Durbin-Watson: ",DurbinWatsonD   /. Outstuff];
SetOptions[$Output,PageWidth->132];
Return[Outstuff]];


(* % Finds the position number of the first element in listname 
   % whose value is greater than the value of comparison *)
FirstElementGreaterThan[listname_,comparison_] := 
Block[{ListLength=Length[listname]},
    For[CurrentElement=1,
    comparison>listname[[CurrentElement]] && CurrentElement<ListLength,CurrentElement++];
    CurrentElement
];

(* % Finds the position number of the first element in listname 
   % whose value is greater than the value of comparison *)
LastElementLessThan[listname_,comparison_] := 
Block[{ListLength=Length[listname]},
    For[CurrentElement=-1,
    comparison<listname[[CurrentElement]] && -CurrentElement<ListLength,CurrentElement--];
    CurrentElement+ListLength+1
];

(* % Generates a new list of the values in list lagged one period *)
Lagged[list_] := Join[{0},Table[list[[i-1]],{i,2,Length[list]}]];

(* % Finds all position numbers of the elements in list 
   % whose value is greater than the value of GreaterThan 
   % and less than the value of LessThan *)
PositionsGreaterLess[list_,GreaterThan_,LessThan_] := Block[{},
building = {};
Do[
If[list[[i]] > GreaterThan && list[[i]] < LessThan,building = Append[building,i]]
,{i,Length[list]}];
building]

ClearAll[PlotRecessions];
PlotRecessions[Scale_,Height_,StartPeriod_,EndPeriod_] := Block[{},
(* First find beginnings and ends of recessions *)
RecessionPeriods = {};
Counter = StartPeriod;
While[Counter <= EndPeriod,
  (* If the current period is a recession *)
  If[aggstate[[Counter]] == Bust,
    (* then find the end of the recession *)
      StartOfRecession = Counter;
      While[aggstate[[Counter]] == Bust,Counter++];
      LastPeriodOfRecession = Counter-1;
      RecessionPeriods = Append[RecessionPeriods,{StartOfRecession-.5,LastPeriodOfRecession+.5}],
    (* else keep looking *)
      Counter++
  ] (* End if *)
];

Table[{GrayLevel[.35],Rectangle[{RecessionPeriods[[i,1]],Height},{RecessionPeriods[[i,2]],Height+Scale}]}
,{i,Length[RecessionPeriods]}]
];

ClearAll[PlotRecovery];
PlotRecovery[Scale_,Height_,StartPeriod_,EndPeriod_] := Block[{},
(* First find beginnings and ends of Recoveries *)
RecoveryPeriods = {};
Counter = StartPeriod;
While[Counter <= EndPeriod,
  (* If the current period is a Recovery *)
  If[aggstate[[Counter]] == PostBust,
    (* then find the end of the Recovery *)
      StartOfRecovery = Counter;
      While[aggstate[[Counter]] == PostBust,Counter++];
      LastPeriodOfRecovery = Counter-1;
      RecoveryPeriods = Append[RecoveryPeriods,{StartOfRecovery-.5,LastPeriodOfRecovery+.5}],
    (* else keep looking *)
      Counter++
  ] (* End if *)
];

Table[{GrayLevel[.55],Rectangle[{RecoveryPeriods[[i,1]],Height},{RecoveryPeriods[[i,2]],Height+Scale}]}
,{i,Length[RecoveryPeriods]}]
];

(* 
Routines to construct discrete approximations to lognormal distributions 
*)
ClearAll[DiscreteApproxToMeanOneLogNormal];
DiscreteApproxToMeanOneLogNormal[StdDev_,NumOfPoints_] := Block[{},
LevelAdjustingParameter = -(1/2) (StdDev)^2;  (* This parameter takes on the value necessary to make the mean in levels = 1 *)
ListOfEdgePoints = Table[Quantile[LogNormalDistribution[LevelAdjustingParameter,StdDev],(i/NumOfPoints)],{i,NumOfPoints-1}];
ListOfEdgePoints = Flatten[{{0},ListOfEdgePoints,{Infinity}}];
ProbOfMeanPoints = Table[CDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],ListOfEdgePoints[[i]]]
                        -CDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],ListOfEdgePoints[[i-1]]]
                        ,{i,2,Length[ListOfEdgePoints]}];
ListOfMeanPoints = Table[NIntegrate[z PDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],z],{z,ListOfEdgePoints[[i-1]],ListOfEdgePoints[[i]]}],{i,2,Length[ListOfEdgePoints]}] / ProbOfMeanPoints;
Return[{ListOfMeanPoints,ProbOfMeanPoints}]
]

ClearAll[DiscreteApproxToMeanOneLogNormalWithCutoff];
DiscreteApproxToMeanOneLogNormalWithCutoff[Cutoff_,StdDev_,NumOfPoints_] := Block[{},
(* Cutoff is the number of standard deviations above and below the mean at which we assume the distribution is cut off *)
LevelAdjustingParameter = -(1/2) (StdDev)^2;  (* This parameter takes on the value necessary to make the mean in levels = 1 *)
CDFofMissing = 2*CDF[NormalDistribution[0,1],-Cutoff] //N;
ListOfEdgePoints = Table[Quantile[LogNormalDistribution[LevelAdjustingParameter,StdDev],CDFofMissing+(i/NumOfPoints)(1-2*CDFofMissing)],{i,0,NumOfPoints}];
ProbOfMeanPoints = Table[CDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],ListOfEdgePoints[[i]]]
                        -CDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],ListOfEdgePoints[[i-1]]]
                        ,{i,2,Length[ListOfEdgePoints]}]/(1-2*CDFofMissing);
ListOfMeanPoints = Table[NIntegrate[z PDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],z] (1/(1-2*CDFofMissing)),{z,ListOfEdgePoints[[i-1]],ListOfEdgePoints[[i]]}],{i,2,Length[ListOfEdgePoints]}] / ProbOfMeanPoints;
AdjustmentFactor = ListOfMeanPoints . ProbOfMeanPoints;
ListOfMeanPoints = ListOfMeanPoints/AdjustmentFactor;
Return[{ListOfMeanPoints,ProbOfMeanPoints}]
]

(* 
The following version chooses equally-spaced edge points, resulting in
an uneven probability for each cell and thereby increasing the range spanned
by the cells 
*)

ClearAll[DiscreteApproxToMeanOneLogNormalWithCutoff];
DiscreteApproxToMeanOneLogNormalWithCutoff[Cutoff_,StdDev_,NumOfPoints_] := Block[{},
(* Cutoff is the number of standard deviations above and below the mean at which we assume the distribution is cut off *)
LevelAdjustingParameter = -(1/2) (StdDev)^2;  (* This parameter takes on the value necessary to make the mean in levels = 1 *)
ListOfEdgePoints = 
Table[Exp[LoopOverEdgePoints-(1/2) (StdDev)^2],{LoopOverEdgePoints,-Cutoff*StdDev,Cutoff*StdDev,2*Cutoff*StdDev/NumOfPoints}];
CDFofMissing =1-( CDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],ListOfEdgePoints[[-1]]]
                 -CDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],ListOfEdgePoints[[1]]]);
ProbOfMeanPoints = Table[CDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],ListOfEdgePoints[[i]]]
                        -CDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],ListOfEdgePoints[[i-1]]]
                        ,{i,2,Length[ListOfEdgePoints]}]/(1-CDFofMissing);
ListOfMeanPoints = Table[NIntegrate[z PDF[LogNormalDistribution[LevelAdjustingParameter,StdDev],z] (1/(1-2*CDFofMissing)),{z,ListOfEdgePoints[[i-1]],ListOfEdgePoints[[i]]}],{i,2,Length[ListOfEdgePoints]}] / ProbOfMeanPoints;
AdjustmentFactor = ListOfMeanPoints . ProbOfMeanPoints;
ListOfMeanPoints = ListOfMeanPoints/AdjustmentFactor;
Return[{ListOfMeanPoints,ProbOfMeanPoints}]
]


ClearAll[MakeNewVarByAppendingStringToName];
MakeNewVarByAppendingStringToName[ListOfVarNames_,StringToAppend_] := Block[{},
 TableOfCommands = 
      Table[
        ListOfVarNames[[LoopOverVarNames]]<>StringToAppend<>" = Table[{},{i,"<>ToString[LifeLength]<>"}];"
        ,{LoopOverVarNames,Length[ListOfVarNames]}]; (* End Table *)
 Map[ToExpression,TableOfCommands];
]

(* # Assign position numbers to each variable in the output of OmegaInvt *)
OmegaInvtOut            = 1;
RiskySharetOmegaInvtOut = 2;



ClearAll[MakeListFunctions];
MakeListFunctions[FunctionNameAndArgList_] :=    (* Create a function which maps f[{a,b,c}] to f[a,b,c] *)
Block[{},
Do[
  FunctionName = FunctionNameAndArgList[[LoopOverFunctions,NamePosInFunctionList   ]];
  FunctionArgs = FunctionNameAndArgList[[LoopOverFunctions,ArgListPosInFunctionList]];
  NumOfArgs = Length[FunctionArgs];
  CommandString = 
    "ClearAll["<>FunctionName<>"];"<>
    FunctionName<>"[ListOfArguments_] := "<>FunctionName<>"["<>
        Table["ListOfArguments[["<>ToString[LoopOverArgs]<>"]]"<>If[LoopOverArgs<NumOfArgs+1,",",""],{LoopOverArgs,NumOfArgs+1}]
    <>"];";
(*  Print[CommandString];*)
  ToExpression[CommandString];
,{LoopOverFunctions,Length[FunctionNameAndArgList]}
]
]

ClearAll[MakeFunctionsFromInterp];
MakeFunctionsFromInterp[FunctionNameAndArgList_] :=    (* Create a function which extracts values from interpolating functions *)
Block[{},
Do[
  FunctionName = FunctionNameAndArgList[[LoopOverFunctions,NamePosInFunctionList   ]];
  FunctionArgs = FunctionNameAndArgList[[LoopOverFunctions,ArgListPosInFunctionList]];
  NumOfArgs = Length[FunctionArgs];
  ListOfArgsAsFunctionArgs = StringJoin[Table[FunctionArgs[[i]]<>"_"<>If[i<NumOfArgs,",",""],{i,NumOfArgs}]];
  ListOfArgsAsVariables    = StringJoin[Table[FunctionArgs[[i]]<>If[i<NumOfArgs,",",""],{i,NumOfArgs}]];
  ToExpression[
    FunctionName<>"["<>ListOfArgsAsFunctionArgs<>",LifePosn_] := "<>FunctionName<>"InterpFunc[[Max[Min[EarliestVtSolved,EarliestOmegatSolved],LifePosn]]]["<>ListOfArgsAsVariables<>"]"
    ];
,{LoopOverFunctions,Length[FunctionNameAndArgList]}
]
]



MakeInterpGivenFunctionName[FunctionName_,InterpOrder_] := Block[{},
  If[VerboseOutput == True,Print["Creating "<>FunctionName<>"."]];
  ArgArray = FunctionName<>"ArgArray";
  ToExpression[
    FunctionName<>"InterpData[[LifePosn]] = AddOutcome["<>ArgArray<>",Map[Apply["<>FunctionName<>"Raw,#] &,"<>ArgArray<>"]]"
    ];
  ToExpression[
    FunctionName<>"InterpFunc[[LifePosn]] = Interpolation["<>FunctionName<>"InterpData[[LifePosn]],InterpolationOrder->"<>ToString[InterpOrder]<>"]"
    ];
]

MakeNormedInterpGivenFunctionName[FunctionName_,InterpOrder_,MultiplyBy_,NormFactor_] := Block[{TmpSign=FunctionSign},
  ArgArray = FunctionName<>"ArgArray";
  Augmented = ArgArray<>"Augmented";
  ToExpression[
    Augmented<>" = AddOutcome["<>ArgArray<>",Table[LifePosn,{Length["<>ArgArray<>"]}]]"
    ];
  ToExpression[
    FunctionName<>"InterpData[[LifePosn]] = AddOutcome["<>ArgArray<>",("<>ToString[MultiplyBy]<>"*Map[Apply["<>FunctionName<>"Raw,#] &,"<>Augmented<>"])^("<>ToString[NormFactor]<>")]"
    ];
  ToExpression[
    FunctionName<>"InterpFunc[[LifePosn]] = Interpolation["<>FunctionName<>"InterpData[[LifePosn]],InterpolationOrder->"<>ToString[InterpOrder]<>"]"
    ];
]


MakeInterpGivenFunctionNameAndResults[FunctionName_,Results_,InterpOrder_] := Block[{},
  ArgArray = FunctionName<>"ArgArray";
  Augmented = ArgArray<>"Augmented";
  ToExpression[
    Augmented<>" = AddOutcome["<>ArgArray<>",Table[LifePosn,{Length["<>ArgArray<>"]}]]"
    ];
  ToExpression[
    FunctionName<>"InterpData[[LifePosn]] = AddOutcome["<>ArgArray<>",Map[Apply["<>ToString[FunctionContents]<>",#] &,"<>Augmented<>"]]"
    ];
  ToExpression[
    FunctionName<>"InterpFunc[[LifePosn]] = Interpolation["<>FunctionName<>"InterpData[[LifePosn]],InterpolationOrder->"<>ToString[InterpOrder]<>"]"
    ];
]


(* Constructs and executes a string of the form VtArgArray=Flatten[Outer[List,sGrid,hMatGrid,AggStateGrid,EmpStateGrid],3] *)
MakeArgArrays[FunctionList_] := Block[{},
Do[
  CurrentFunctionArgArray = FunctionList[[LoopOverFunctions,NamePosInFunctionList]]<>"ArgArray";
  ListOfArguments =    (* Makes a string of the form ",sGrid,hMatGrid,AggStateGrid,EmpStateGrid" *)
      Table[
          StringJoin[
            ","
            ,FunctionList[[LoopOverFunctions,ArgListPosInFunctionList,LoopOverArguments]]
            ,"Grid"]
        ,{LoopOverArguments,Length[FunctionList[[LoopOverFunctions,ArgListPosInFunctionList]]]}
          ];
  ToExpression[
      CurrentFunctionArgArray<>
      "=Flatten[Outer[List"<>StringJoin[ListOfArguments]<>"],"<>ToString[Length[FunctionList[[LoopOverFunctions,ArgListPosInFunctionList]]]-1]<>"]"
              ];
,{LoopOverFunctions,Length[FunctionList]}
]
];


ClearAll[SaveFOC];
SaveFOC[filename_] := Block[{},
Print["Attempting to save ..."];
SetDirectory["Output"];
DeleteFile[filename];
(*
FOCVars =
  Table[
    FuncName=FunctionList[[LoopOverFunctions,NamePosInFunctionList]];
    FuncName<>","<>FuncName<>"Raw,"<>FuncName<>"InterpData,"<>FuncName<>"InterpFunc,"<>FuncName<>"ArgArray"
    <>If[LoopOverFunctions<Length[FunctionList],",",""]
  ,{LoopOverFunctions,Length[FunctionList]}];
*)
FOCSaveFileName=filename;
Do[
  Print["Save[FOCSaveFileName,"<>StringJoin[FOCVars[[LoopOverFuncs]]]<>"]"];
  ,{LoopOverFuncs,Length[FOCVars]}];
(*
Save[filename
,LifePosn,LifeLength,rho,gamma,YearsOfLife,PeriodsPerYear,catchup
,b,betaannual,Gannual,Rannual,mannual,lambdaannual,OmegaInvannual
,G,R,m,n,lambda,beta,OmegaInv,q,xmin,xmax,xstep,hmin,hmax,hsupermax,hstep,smin,smax,sstep
,InterpOrder,EarliestVtSolved,EarliestOmegatSolved
,epVals,NumOfepVals,NumOfepValsPerEmpState
,etVals,NumOfetVals
,xGrid,sGrid,hMatGrid
,Rcertain,NumOfRVals,Rprob,RSig,RCutoff,EquityPremium,SolveForRiskyShare,RetirementReplacementRatio,LowMem,u
,etSig,epSig,Cutoff,etZeroProb,etZeroProbVals,RSig,RCutoff,NumOfRiskyShareSteps,NumOfAggStates,NumOfEmpStates,EmpStateProb,EmpStateTransitionMatrix
,CumEmpStateTransitionMatrix,NumOfxSteps,NumOfsSteps,AggStateTransitionMatrix,CumAggStateTransitionMatrix
,RStateTransitionMatrix,CumRStateTransitionMatrix,LowMem,VerboseSetup,SolutionMethod]
*)
SetDirectory[ParentDirectory[]];
];


ClearAll[SaveFOC];
SaveFOC[filename_] := Block[{},
Print["Attempting to save ..."];
SetDirectory["Output"];
DeleteFile[filename];
(*
FOCVars =
  Table[
    FuncName=FunctionList[[LoopOverFunctions,NamePosInFunctionList]];
    FuncName<>","<>FuncName<>"Raw,"<>FuncName<>"InterpData,"<>FuncName<>
            "InterpFunc,"<>FuncName<>"ArgArray"
    <>If[LoopOverFunctions<Length[FunctionList],",",""]
  ,{LoopOverFunctions,Length[FunctionList]}];
*)
FOCSaveFileName=filename;
Do[
  SaveString = 
        "Save[FOCSaveFileName,"<>
          StringJoin[FunctionList[[LoopOverFuncs,NamePosInFunctionList]]]<>
          "]";
	Print[SaveString];
	ToExpression[SaveString];
  ,{LoopOverFuncs,Length[FunctionList]}];
Save[filename
,LifePosn,LifeLength,rho,gamma,YearsOfLife,PeriodsPerYear,catchup
,b,betaannual,Gannual,Rannual,mannual,lambdaannual,OmegaInvannual
,G,R,m,n,lambda,beta,OmegaInv,q,xmin,xmax,xstep,hmin,hmax,hsupermax,hstep,smin,
        smax,sstep
,InterpOrder,EarliestVtSolved,EarliestOmegatSolved
,epVals,NumOfepVals,NumOfepValsPerEmpState
,etVals,NumOfetVals
,xGrid,sGrid,hMatGrid
,Rcertain,NumOfRVals,Rprob,RSig,RCutoff,EquityPremium,SolveForRiskyShare,
        RetirementReplacementRatio,LowMem,u
,etSig,epSig,Cutoff,etZeroProb,etZeroProbVals,RSig,RCutoff,NumOfRSteps,NumOfAggStates,
        NumOfEmpStates,EmpStateProb,EmpStateTransitionMatrix
,CumEmpStateTransitionMatrix,NumOfxSteps,NumOfsSteps,AggStateTransitionMatrix,
        CumAggStateTransitionMatrix
,RStateTransitionMatrix,CumRStateTransitionMatrix,LowMem,VerboseSetup,
        SolutionMethod
];
SetDirectory[ParentDirectory[]];
]

ClosestElementTo[ListOfElems_,Match_] := 
Block[{},
  {MinDist,MinDistLoc} ={ Abs[ListOfElems[[1]]-Match],1};
  Do[
    If[Abs[ListOfElems[[i]]-Match]<MinDist,
      {MinDist,MinDistLoc} = {Abs[ListOfElems[[i]]-Match],i}]
  ,{i,Length[ListOfElems]}];
  Return[MinDistLoc]
];


SumOfList[list_] := CumulativeSums[Flatten[list]][[-1]];
