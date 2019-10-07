(*
Remove[
  RScaledTimesVxtArgGridDistGivenOmegatAllArgArray
 ,OmegatArgGridDistGivenVtArgListMatrix
 ,Vtp1ArgGridDistGivenOmegatAllArgArray
 ,EtMPCPermListResults
 ,EtMPCPermListInterpData
 ,RScaledTimesVxtArgListDistGivenOmegatAllArgArray
 ,RScaledTimesVxtArgListDistGivenOmegatAllArg
 ,Vtp1ArgListDistGivenOmegatAllArgArray
 ,Vtp1ArgListDistGivenOmegatAllArg
 ,RScaledTimesVxtArgArray
 ,RGapTimesVxtargArrray
 ];
*)
sTransMatrix = Omegatp1ArgGridDistGivenOmegatArgList[[LifePosn]];
xTransMatrix = Vtp1ArgGridDistGivenVtArgList[[LifePosn]];
StartsVec = Flatten[OmegatArgGridDistGivenVtArgList[2,1,1,1,1]];
StartxVec = Vtp1ArgGridDistGivenOmegatArgListMatrix[[LifePosn]] . StartsVec;

Remove[
  Omegatp1ArgGridDistGivenOmegatArgList
  ,Vtp1ArgGridDistGivenVtArgList
  ,Vtp1ArgGridDistGivenOmegatArgListMatrix
  ];

NumOfsGridStates = Length[s0MatGrid]*Length[hMatGrid]*Length[EmpStateGrid]*Length[AggStateGrid];

sErgodicDist = FindRightEigenVec[sTransMatrix];
{sEigenVecs,sEigenVals} = {EigenVecs,EigenVals};


sTransMatrixPower = sTransMatrix;
Do[
Print["Raising sTransMatrix to the power "<>ToString[2^(MatPower)]];
sTransMatrixPower = MatrixPower[sTransMatrixPower,2];
,{MatPower,6}];


SteadyStateEmpDist = MatrixPower[Transpose[EmpStateTransitionMatrix[[1]]],10000] . Table[1/NumOfEmpStates,{NumOfEmpStates}];


Flatten[{SteadyStateEmpDist,Table[0,{Length[sTransMatrix]-Length[SteadyStateEmpDist]}]}];

sErgodicDist = sTransMatrixPower . StartsVec;


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

hDist = 
  Table[
  SumOfList[
    Table[
    sErgodicArray[[LoopOversVals,LoopOverhVals,LoopOverAggStates,LoopOverEmpStates]]
    ,{LoopOverAggStates,Length[AggStateGrid]}
    ,{LoopOverEmpStates,Length[EmpStateGrid]}
    ,{LoopOversVals,Length[s0MatGrid]}]]
  ,{LoopOverhVals,Length[hMatGrid]}];
  
Print["hDist:",MatrixForm[Transpose[{hMatGrid,hDist}]]];

sMeanByEmpState = 
  Table[
    sDistByEmpState[[LoopOverEmpStates]] . s0MatGrid/EmpDist[[LoopOverEmpStates]]
  ,{LoopOverEmpStates,Length[EmpStateGrid]}];
  
Print["sMeanByEmpState:",sMeanByEmpState];



NumOfxGridStates = Length[x0MatGrid]*Length[hMatGrid]*Length[EmpStateGrid]*Length[AggStateGrid]*Length[AggKGrid];

xTransMatrixPower = xTransMatrix;
Do[
Print["Raising xTransMatrix to the power "<>ToString[2^(MatPower)]];
xTransMatrixPower = MatrixPower[xTransMatrixPower,2];
,{MatPower,6}];


xErgodicDist = FindRightEigenVec[xTransMatrix];

{xEigenVecs,xEigenVals} = {EigenVecs,EigenVals};

Flatten[{SteadyStateEmpDist,Table[0,{Length[xTransMatrix]-Length[SteadyStateEmpDist]}]}];

xErgodicDist = xTransMatrixPower . StartxVec;


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




