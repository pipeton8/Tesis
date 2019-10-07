(*

Define the functions which yield the value functions, marginal value functions, and
other useful constructs

*)

u[ct_,ht_]       := ((ct/(ht^gamma))^(1-rho))/(1-rho);
dudct[ct_,ht_]   := (ct^(-rho))(ht^(rho gamma - gamma));            (* du/dc *)
dudht[ct_,ht_] := -gamma (ct^(1-rho)) ht^(rho gamma - gamma - 1); (* du/dh *)


Patience[AggState_,EmpState_,LifePosn_] := Block[{},
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
        ((R[[LoopOverAggStates,LoopOverRVals]] beta[[LifePosn+1,EmpState]]*
         (G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]] epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]])^-(rho + (1-rho) gamma)
         ))*
          etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
         *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
        *Rprob[[LoopOverAggStates,LoopOverRVals]]
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
       *EmpStateProb[AggState,EmpState,LoopOverEmpStates]
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
      *AggStateProb[AggState,LoopOverAggStates]
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]; (* End Return[] *)
];



EtDLogGtp1[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
  Sum[ 
    Log[G[[LifePosn,LoopOverAggStates,LoopOverEmpStates]]*
     (etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]] . etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]])
    /(etVals[[LifePosn,AggState,EmpState]] . etProb[[LifePosn,AggState,EmpState]])
    ]*
    AggStateProb[AggState,LoopOverAggStates]*
    EmpStateProb[AggState,EmpState,LoopOverEmpStates]
    ,{LoopOverAggStates,NumOfAggStates}
    ,{LoopOverEmpStates,NumOfEmpStates}]

EtDLogGtp1[xt_,htStart_,AggState_,EmpState_,AggKt_] := EtDLogGtp1[xt,htStart,AggState,EmpState,AggKt,LifePosn];



EtDLogGtp2[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Sum[
   Log[G[[LifePosn,LoopOverTp2AggStates,EmpState]]]*
     AggStateProb[AggState            ,LoopOverTp1AggStates]*EmpStateProb[AggState,EmpState,LoopOverTp1EmpStates]*
     AggStateProb[LoopOverTp1AggStates,LoopOverTp2AggStates]*EmpStateProb[LoopOverTp1AggStates,LoopOverTp1EmpStates,LoopOverTp2EmpStates]
 ,{LoopOverTp1AggStates,NumOfAggStates}
 ,{LoopOverTp1EmpStates,NumOfEmpStates}
 ,{LoopOverTp2AggStates,NumOfAggStates}
 ,{LoopOverTp2EmpStates,NumOfEmpStates}]

EtDLogGtp2[xt_,htStart_,AggState_,EmpState_,AggKt_] := EtDLogGtp2[xt,htStart,AggState,EmpState,AggKt,LifePosn];

FindKSTargetsByAggState[AggState_] := Block[{},

If[FracPatient < 1,

{ImpatientU,ImpatientE} = {3,4};
{PatientU,PatientE}     = {1,2};

RootResults = 
FindRoot[
  {
   cP  == ctList[xP,1,AggStateNow,PatientU,k,LifePosn] ug
         +ctList[xP,1,AggStateNow,PatientE,k,LifePosn] (1-ug)
,  kP  == ((1-Depreciation)/G[[LifePosn, AggStateNow, PatientU]])
         *(
           (pigg00/pigg)*ug
            (xP-cP)
          +(pigg10/pigg)*(1-ug)
            (xP-cP)
          )/ug
,  xP  == kP(1 + rFunc[k]) + wFunc[k]
,  cI  == ctList[xI,1,AggStateNow,ImpatientU,k,LifePosn] ug
         +ctList[xI,1,AggStateNow,ImpatientE,k,LifePosn] (1-ug)
,  kI  == ((1-Depreciation)/G[[LifePosn, AggStateNow, PatientU]])
         *(
           (pigg00/pigg)*ug
            (xI-cI)
          +(pigg10/pigg)*(1-ug)
            (xI-cI)
          )/ug
,  xI  == kI(1 + rFunc[k]) + wFunc[k]
,  k  == (kP*ug + kP*(1-ug))FracPatient
        +(kI*ug + kI*(1-ug))FracImpatient
	  }
, {kP, {60,80}}
, {xP, {45,85}}
, {cP, {4,10}}
, {kI, {4,10}}
, {xI, {4,10}}
, {cI, {4,10}}
, {k,  {35,40}}
,MaxIterations->100
];

kPHat = (kP /. RootResults);
xPHat = (xP /. RootResults);
kIHat = (kI /. RootResults);
xIHat = (xI /. RootResults);
cIHat = (cI /. RootResults);
cPHat = (cP /. RootResults);

kHat   = (k   /. RootResults);

Print["{kPHat,xPHat,kIHat,xIHat,kHat}=",{kPHat,xPHat,kIHat,xIHat,kHat}];
,

  (* else there are only patient consumers *)

If[ug>0,

{PatientU,PatientE} = {1,2};
RootResults = 
  FindRoot[
  {
   cP  == ctList[xP,1,AggStateNow,PatientU,k,LifePosn] ug
         +ctList[xP,1,AggStateNow,PatientE,k,LifePosn] (1-ug)
,  kP  == ((1-Depreciation)/G[[LifePosn, AggStateNow, PatientU]])
         *(
           (pigg00/pigg)*ug
            (xP-cP)
          +(pigg10/pigg)*(1-ug)
            (xP-cP)
          )/ug
,  xP  == kP(1 + rFunc[k]) + wFunc[k]
,  k  == (kP*ug + kP*(1-ug))FracPatient
	  }
, {kP, {40,100}}
, {xP, {45,105}}
, {cP, {5,10}}
, {k,  {25,40}}
,MaxIterations->100
];
kPHat = kHat = (kP /. RootResults);
xPHat = xHat = (xP /. RootResults);
cPHat = cHat = (cP /. RootResults);
,
 (* else there is no unemployment *)
{PatientU,PatientE} = {1,2};
RootResults = 
  FindRoot[
  {
   cP  == ctList[xP,1,AggStateNow,1,k,LifePosn]
,  kP  == ((1-Depreciation)/G[[LifePosn, AggStateNow, 1]])
            (xP-cP)
,  xP  == kP(1 + rFunc[k]) + wFunc[k]
,  k  == kP
	  }
, {kP, {40,100}}
, {xP, {45,105}}
, {cP, {5,10}}
, {k,  {25,40}}
,MaxIterations->100
];
kPHat = kHat = (kP /. RootResults);
xPHat = xHat = (xP /. RootResults);
];
Print["{kHat,xPHat}=",{kHat,xHat}];
];
]; (* End FindKSTargetsByAggState - unfinished *)


ExtraDefsFileName = "func_defs_"<>ProblemToSolveString<>".m";

If[Length[FileNames[ExtraDefsFileName]]>0,Get[ExtraDefsFileName]];
