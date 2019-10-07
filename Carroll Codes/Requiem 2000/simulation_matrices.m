
ClearAll[DrawNextAggState];
DrawNextAggState[CurrAggState_] := FirstElementGreaterThan[CumAggStateTransitionMatrix[[CurrAggState]],Random[]];
DrawNextAggState[CurrAggStateList_List] := Table[FirstElementGreaterThan[CumAggStateTransitionMatrix[[CurrAggStateList[[i]]]],Random[]],{i,Length[CurrAggStateList]}];


xtValue[xt_,ht_,AggState_,EmpState_,epVal_]  := xt;
htValue[xt_,ht_,AggState_,EmpState_,epVal_] := ht;
shtValue[xt_,ht_,AggState_,EmpState_] := ht;
stValue[st_,ht_,AggState_,EmpState_] := st;
GrossAggSave[xt_,ht_,AggState_,EmpState_,epVal_] := Block[
{ctVal = ctList[xt,ht,AggState,EmpState,epVal]},
  (((Rcertain-1+Depreciation)/G[[LifePosn,AggState,EmpState]])(xt-ctVal)+1-ctVal)
  /(((Rcertain-1+Depreciation)/G[[LifePosn,AggState,EmpState]])(xt-ctVal)+1)];
Gt[xt_,ht_,AggState_,EmpState_,epVal_]:= G[[LifePosn,AggState,EmpState]];


PeriodsToSimulate = 150;

SelectAggState = Table[
  Flatten[
    Table[
      If[LoopOverAggs == GoingTo,
         (* then *) 1/AggStateProb[ComingFrom,GoingTo] //N,
         (* else *) 0]
    ,{LoopOverxs  ,Length[xMatGrid]}
    ,{LoopOverhs  ,Length[hMatGrid]}
    ,{LoopOverAggs,Length[AggStateGrid]}
    ,{LoopOverEmps,Length[EmpStateGrid]}
    ,{LoopOvereps ,Length[AggKGrid]}
    ]
  ]
  ,{ComingFrom,Length[AggStateGrid]}
  ,{GoingTo,Length[AggStateGrid]}
];

SelectEmpState = Table[
  Flatten[
    Table[
      If[LoopOverEmps == GoingTo,
         (* then *) 1/EmpStateTransitionMatrix[[1,ComingFrom,GoingTo]],
         (* else *) 0]
    ,{LoopOverxs  ,Length[xMatGrid]}
    ,{LoopOverhs  ,Length[hMatGrid]}
    ,{LoopOverAggs,Length[AggStateGrid]}
    ,{LoopOverEmps,Length[EmpStateGrid]}
    ,{LoopOvereps ,Length[AggKGrid]}
    ]
  ]
  ,{ComingFrom,Length[EmpStateGrid]}
  ,{GoingTo,Length[EmpStateGrid]}
];

SummarizeEmpState = Table[
  Flatten[
    Table[
      If[LoopOverEmps == GoingTo,
         (* then *) 1,
         (* else *) 0]
    ,{LoopOverxs  ,Length[xMatGrid]}
    ,{LoopOverhs  ,Length[hMatGrid]}
    ,{LoopOverAggs,Length[AggStateGrid]}
    ,{LoopOverEmps,Length[EmpStateGrid]}
    ,{LoopOvereps ,Length[AggKGrid]}
    ]
  ]
  ,{GoingTo,Length[EmpStateGrid]}
];

ListOfxtFuncsToCalc = {"ctList","xtValue","htValue","MPCList","EtDLogGtp1"
                      ,"EtDLogGtp2","Gt","EtLogctp1ListGtp1Oct"
                      ,"EtLogctp1ListGtp1OctSquared","EtEulerListApprox"
                      ,"GrossAggSave"
                      };
ListOfstFuncsToCalc = {"stValue","shtValue","EtMPCPermList"};
ListOfVarsToMake    = Union[ListOfxtFuncsToCalc,ListOfstFuncsToCalc,{"xDistByEmpState","sDist","xDist","xDistEmpState1","xDistEmpState2"},{"AggState"},{"EmpState"}];

Do[
ToExpression[ListOfVarsToMake[[i]]<>"Sequence = Table[0,{PeriodsToSimulate}];"]
,{i,Length[ListOfVarsToMake]}];


Do[
  ToExpression[ListOfxtFuncsToCalc[[i]]<>"EmpState1Sequence = "<>ListOfxtFuncsToCalc[[i]]<>"EmpState2Sequence = Table[0,{PeriodsToSimulate}];"]
,{i,Length[ListOfxtFuncsToCalc]}];


AggStateSequence[[1]] = 1;
EmpStateSequence[[1]] = 2;

MassInAggState2 = 
  CumulativeSums[
    sErgodicDist * SelectAggState[[1,1]]
  ][[-1]];

sDistSequence[[1]] = 
    sErgodicDist * (SelectAggState[[1,1]]/MassInAggState2)*SelectEmpState[[2,2]];
    
xDistSequence[[1]] = 
    xErgodicDist * (SelectAggState[[1,1]]/MassInAggState2)*SelectEmpState[[2,2]];

xDistByEmpStateSequence[[1]] = 
{0,1,0};
  
Do[
  ToExpression[ListOfxtFuncsToCalc[[i]]
               <>"Vals = Map[ Apply["<>ListOfxtFuncsToCalc[[i]]<>
                                    ", #] &, VxInvtMatArgArray];"]
  ,{i,Length[ListOfxtFuncsToCalc]}];

Do[
  ToExpression[ListOfstFuncsToCalc[[i]]
               <>"Vals = Map[ Apply["<>ListOfstFuncsToCalc[[i]]<>
                                    ", #] &, OmegasInvtMatArgArray];"]
  ,{i,Length[ListOfstFuncsToCalc]}];

Do[
  ToExpression[ListOfxtFuncsToCalc[[j]]<>"Sequence[["<>ToString[1]<>"]] = "<>ListOfxtFuncsToCalc[[j]]<>"Vals . xDistSequence[["<>ToString[1]<>"]]"];
  ,{j,Length[ListOfxtFuncsToCalc]}];


Do[
ToExpression[ListOfstFuncsToCalc[[j]]<>"Sequence[["<>ToString[1]<>"]] = "<>ListOfstFuncsToCalc[[j]]<>"Vals . sDistSequence[["<>ToString[1]<>"]]"];
,{j,Length[ListOfstFuncsToCalc]}];


Do[
  If[Mod[i,10] == 0,Print["Constructing data for period ",i]];

  AggStateSequence[[i]] = DrawNextAggState[AggStateSequence[[i-1]]];

  AggStateSequence[[i]] = If[i<50,1,2];  (* Live in AggState 2 for 50 periods, then switch to 1 *)

  If[i>100,AggStateSequence[[i]] = 1];  (* Live in AggState 2 for 50 periods, then switch to 1 *)
  
  EmpStateSequence[[i]] = 2; (* Always draw the 'normal' permanent income *)

  sDistSequence[[i]] = 
   (sTransMatrix . sDistSequence[[i-1]])
                   *SelectAggState[[AggStateSequence[[i-1]],AggStateSequence[[i]]]]
                   *SelectEmpState[[2,2]];
    
  xDistSequence[[i]] = 
   (xTransMatrix . xDistSequence[[i-1]])
                   *SelectAggState[[AggStateSequence[[i-1]],AggStateSequence[[i]]]]
                   *SelectEmpState[[2,2]];

  xDistByEmpStateSequence[[i]] =
    Table[
      xDistSequence[[i]] . SummarizeEmpState[[LoopOverEmpStates]]
    ,{LoopOverEmpStates,Length[EmpStateGrid]}];

  Do[
(*  Print[ListOfxtFuncsToCalc[[j]]<>"Sequence[["<>ToString[i]<>"]] = "<>ListOfxtFuncsToCalc[[j]]<>"Vals . xDistSequence[["<>ToString[i]<>"]]"];*)
  ToExpression[ListOfxtFuncsToCalc[[j]]<>"Sequence[["<>ToString[i]<>"]] = "<>ListOfxtFuncsToCalc[[j]]<>"Vals . xDistSequence[["<>ToString[i]<>"]]"];
  ,{j,Length[ListOfxtFuncsToCalc]}];

  Do[
(*  Print[ListOfstFuncsToCalc[[j]]<>"Sequence[["<>ToString[i]<>"]] = "<>ListOfstFuncsToCalc[[j]]<>"Vals . sDistSequence[["<>ToString[i]<>"]]"];*)
  ToExpression[ListOfstFuncsToCalc[[j]]<>"Sequence[["<>ToString[i]<>"]] = "<>ListOfstFuncsToCalc[[j]]<>"Vals . sDistSequence[["<>ToString[i]<>"]]"];
  ,{j,Length[ListOfstFuncsToCalc]}];

(*
If[Length[EmpDist]>1,
  xDistEmpState1Sequence[[i]] = 
    Table[
      (xDistSequence[[i,LoopOverxDist]]/EmpDist[[1]])*
      If[Mod[LoopOverxDist,Length[EmpDist]]==1,1,0]
    ,{LoopOverxDist,Length[xDistSequence[[i]]]}];


  xDistEmpState2Sequence[[i]] = 
    Table[
      (xDistSequence[[i,LoopOverxDist]]/EmpDist[[2]])*
      If[Mod[LoopOverxDist,Length[EmpDist]]==0,1,0]
    ,{LoopOverxDist,Length[xDistSequence[[i]]]}];
        
  Do[
  ToExpression[ListOfxtFuncsToCalc[[j]]<>"EmpState1Sequence[["<>ToString[i]<>"]] = "<>ListOfxtFuncsToCalc[[j]]
      <>"Vals . xDistEmpState1Sequence[["<>ToString[i]<>"]]"];
    ,{j,Length[ListOfxtFuncsToCalc]}];

  Do[
  ToExpression[ListOfxtFuncsToCalc[[j]]<>"EmpState2Sequence[["<>ToString[i]<>"]] = "<>ListOfxtFuncsToCalc[[j]]
      <>"Vals . xDistEmpState2Sequence[["<>ToString[i]<>"]]"];
    ,{j,Length[ListOfxtFuncsToCalc]}];
]; (* end If[Length[EmpDist] *)
*)
,{i,2,PeriodsToSimulate}];


Do[
ToExpression[ListOfVarsToMake[[i]]<>"Chop = Take["<>ListOfVarsToMake[[i]]<>"Sequence,-(PeriodsToSimulate-2)]"]
,{i,Length[ListOfVarsToMake]}];

Do[
ToExpression[ListOfVarsToMake[[i]]<>"Tm1 = Take["<>ListOfVarsToMake[[i]]<>"Sequence,{2,PeriodsToSimulate-1}]"]
,{i,Length[ListOfVarsToMake]}];

Do[
ToExpression[ListOfVarsToMake[[i]]<>"Tm2 = Take["<>ListOfVarsToMake[[i]]<>"Sequence,{1,PeriodsToSimulate-2}]"]
,{i,Length[ListOfVarsToMake]}];

Print["xtValuePlot"];
ListPlot[xtValueChop,PlotJoined->True,PlotRange->All];

(*
MyReg["stValueChop",{"AggStateChop","stValueTm1"}];
MyReg["xtValueChop",{"AggStateChop","xtValueTm1"}];
*)

DLogCtChop = Log[GtChop ctListChop]- Log[ctListTm1];
DLogCtTm1  = Log[GtTm1 ctListTm1]  - Log[ctListTm2];

(*
MyReg["DLogCtChop",{"EtDLogGtp1Tm1"}];
MyReg["DLogCtChop",{"EtDLogGtp2Tm2"}];
MyReg["DLogCtChop",{"DLogCtTm1"}];
MyReg["DLogCtChop",{"EtDLogGtp1Tm1","DLogCtTm1"}];
*)

ListPlot[EtMPCPermListSequence,PlotJoined->True,PlotRange->All,PlotLabel->"EtMPCPermListSequence"];
ListPlot[ctListSequence,PlotJoined->True,PlotRange->All,PlotLabel->"ctListSequence"];
ListPlot[GrossAggSaveSequence,PlotJoined->True,PlotRange->All,PlotLabel->"GrossAggSaveSequence"];
