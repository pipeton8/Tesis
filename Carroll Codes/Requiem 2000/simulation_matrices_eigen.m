
sTransMatrix = Omegatp1ArgGridDistGivenOmegatArgList[[LifePosn]];
xTransMatrix = Vtp1ArgGridDistGivenVtArgList[[LifePosn]];

NumOfsGridStates = Length[s0MatGrid]*Length[hMatGrid]*Length[EmpStateGrid]*Length[AggStateGrid];

SelectsAggTransitions = Table[
  DiagonalMatrix[
    Flatten[    
	  Table[
	    Table[
          If[Mod[i-LoopOverAggStates,Length[AggStateGrid]] == 0,1,0]
        ,{j,Length[EmpStateGrid]*Length[hMatGrid]}]
      ,{i,NumOfsGridStates/(Length[EmpStateGrid]*Length[hMatGrid])}
	  ]
    ]
  ]
  ,{LoopOverAggStates,Length[AggStateGrid]}
];
  
sTransMatrixByAggState = Table[
  SelectsAggTransitions[[LoopOverResult]] 
  . sTransMatrix 
  . SelectsAggTransitions[[LoopOverSource]]
  / AggStateProb[LoopOverSource,LoopOverResult]
  ,{LoopOverSource,Length[AggStateGrid]}
  ,{LoopOverResult,Length[AggStateGrid]}];


	
sErgodicDist = FindRightEigenVec[sTransMatrix];

sErgodicArray = 
Partition[
  Partition[
	Partition[
      sErgodicDist
    ,NumOfEmpStates]
  ,NumOfAggStates]
,Length[hMatGrid]];


sDist = Table[
  SumOfList[sErgodicArray[[i]]]
  ,{i,Length[s0MatGrid]}];
  
sDistCum = CumulativeSums[sDist];

Print["Steady-State Distribution of s:"];
Print[MatrixForm[Transpose[{s0MatGrid,sDist}]]];

Print["Mean value of s:",sDist . s0MatGrid];

sDistByEmpState = 
 Transpose[
  Table[
    SumOfList[
      Table[
        sErgodicArray[[LoopOversVals,LoopOverhVals,LoopOverAggStates,LoopOverEmpStates]]
      ,{LoopOverAggStates,Length[AggStateGrid]}
      ,{LoopOverhVals,Length[hMatGrid]}]]
      ,{LoopOversVals,Length[s0MatGrid]}
    ,{LoopOverEmpStates,Length[EmpStateGrid]}]
 ];

EmpDist = 
  Table[
  SumOfList[
    Table[
    sErgodicArray[[LoopOversVals,LoopOverhVals,LoopOverAggStates,LoopOverEmpStates]]
    ,{LoopOverAggStates,Length[AggStateGrid]}
    ,{LoopOverhVals,Length[hMatGrid]}
    ,{LoopOversVals,Length[s0MatGrid]}]]
  ,{LoopOverEmpStates,Length[EmpStateGrid]}];
  
Print["EmpDist:",EmpDist];

sMeanByEmpState = 
  Table[
    sDistByEmpState[[LoopOverEmpStates]] . s0MatGrid/EmpDist[[LoopOverEmpStates]]
  ,{LoopOverEmpStates,Length[EmpStateGrid]}];
  
Print["sMeanByEmpState:",sMeanByEmpState];

NumOfxGridStates = Length[x0MatGrid]*Length[hMatGrid]*Length[EmpStateGrid]*Length[AggStateGrid];
(* Old
SelectxAggTransitions = Table[
  DiagonalMatrix[
	Table[If[Mod[i-LoopOverAggStates,Length[AggStateGrid]] == 0,1,0],{i,NumOfxGridStates}]
  ]
  ,{LoopOverAggStates,Length[AggStateGrid]}
];
  
xTransMatrixByAggState = Table[
  SelectxAggTransitions[[LoopOverResult]] 
  . xTransMatrix 
  . SelectxAggTransitions[[LoopOverSource]]
  / AggStateProb[LoopOverSource,LoopOverResult]
  ,{LoopOverSource,Length[AggStateGrid]}
  ,{LoopOverResult,Length[AggStateGrid]}];
*)
SelectxAggTransitions = Table[
  DiagonalMatrix[
    Flatten[    
	  Table[
	    Table[
          If[Mod[i-LoopOverAggStates,Length[AggStateGrid]] == 0,1,0]
        ,{j,Length[EmpStateGrid]*Length[hMatGrid]*Length[AggKGrid]}]
      ,{i,NumOfxGridStates/(Length[EmpStateGrid]*Length[hMatGrid]*Length[AggKGrid])}
	  ]
    ]
  ]
  ,{LoopOverAggStates,Length[AggStateGrid]}
];
  
xTransMatrixByAggState = Table[
  SelectxAggTransitions[[LoopOverResult]] 
  . xTransMatrix 
  . SelectxAggTransitions[[LoopOverSource]]
  / AggStateProb[LoopOverSource,LoopOverResult]
  ,{LoopOverSource,Length[AggStateGrid]}
  ,{LoopOverResult,Length[AggStateGrid]}];


xErgodicDist = FindRightEigenVec[xTransMatrix];

xErgodicArray = 
Partition[
Partition[
  Partition[
	Partition[
      xErgodicDist
     ,Length[AggKGrid]]
    ,NumOfEmpStates]
  ,NumOfAggStates]
,Length[hMatGrid]
];


xDist = Table[
  SumOfList[xErgodicArray[[i]]]
  ,{i,Length[x0MatGrid]}];
  
xDistCum = CumulativeSums[xDist];

Print["Steady-State Distribution of x:"];
Print[MatrixForm[Transpose[{x0MatGrid,xDist}]]];

Print["Steady-State Mean x:",xDist . x0MatGrid];

xDistByEmpState = 
 Transpose[
  Table[
    SumOfList[
      Table[
        xErgodicArray[[LoopOverxVals,LoopOverhVals,LoopOverAggStates,LoopOverEmpStates]]
      ,{LoopOverAggStates,Length[AggStateGrid]}
      ,{LoopOverhVals,Length[hMatGrid]}]]
      ,{LoopOverxVals,Length[x0MatGrid]}
    ,{LoopOverEmpStates,Length[EmpStateGrid]}]
 ];

xMeanByEmpState = 
  Table[
    xDistByEmpState[[LoopOverEmpStates]] . x0MatGrid/EmpDist[[LoopOverEmpStates]]
  ,{LoopOverEmpStates,Length[EmpStateGrid]}];

Print["xMeanByEmpState:",xMeanByEmpState];
