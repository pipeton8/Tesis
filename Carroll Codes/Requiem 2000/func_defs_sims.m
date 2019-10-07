
(* 

Now define the functions which calculate the transition probabilities adjusted 
for growth rates

*)
   
Vtp1ArgGridDistGivenOmegatAllArg[st_,htEnd_,AggState_,EmpState_,RiskySharet_] := 
Block[{(*VtArgGridDistArray,WeightedR,xtCalc,ProbOfArgs,ListOfArgs,ProbOfArgs*)},
  VtArgGridDistArray = VtArgArrayEmpty;
  htEndVal = htEnd;
    Do[            (* over possible rates of return                       *)
     Do[           (* over possible future employment states              *)
      Do[          (* over possible future aggregate states               *)
       Do[         (* over possible future values of the permanent shock  *)
        Do[        (* over possible future values of the transitory shock *) 
              WeightedR = 
              (
               (R[[LoopOverAggStates,LoopOverRVals]]*RiskySharet
               +Rcertain                            *(1-RiskySharet)
               )/
               (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]*
                G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
               )
              );(*Print[WeightedR]*);
              xtCalc = WeightedR*st
               +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]];
            ListOfArgs = 
             {xtCalc
             ,htEndVal
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1};
            ProbOfArgs = 
              etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
             *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
             *Rprob[[LoopOverAggStates,LoopOverRVals]]
             *EmpStateProb[AggState,EmpState,LoopOverEmpStates]
             *AggStateProb[AggState,LoopOverAggStates];
            If[ProbOfArgs<0,Print[ListOfArgs];Print[ProbOfArgs]];
            VtArgGridDistArray[[
                 ClosestElementTo[x0MatGrid,xtCalc]
                ,ClosestElementTo[hMatGrid,htEndVal]
                ,LoopOverAggStates
                ,LoopOverEmpStates
                ,LoopOverepVals
                ]] += ProbOfArgs;
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}];                                         (* End loop over AggStates  *)
  Return[VtArgGridDistArray];
];


RScaledTimesVxtArgGridDistGivenOmegatAllArg[st_,htEnd_,AggState_,EmpState_,RiskySharet_] := 
Block[{(*VtArgGridDistArray,WeightedR,xtCalc,ProbOfArgs,ListOfArgs,ProbOfArgs*)},
  RScaledTimesVxtArgGridDistArray = RScaledTimesVxtArgArrayEmpty;
  htEndVal = htEnd;
    Do[            (* over possible rates of return                       *)
     Do[           (* over possible future employment states              *)
      Do[          (* over possible future aggregate states               *)
       Do[         (* over possible future values of the permanent shock  *)
        Do[        (* over possible future values of the transitory shock *) 
            RScaled = 
              (
               (R[[LoopOverAggStates,LoopOverRVals]]*RiskySharet
               +Rcertain                            *(1-RiskySharet)
               )/
               (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]*
                G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
               )
              );(*Print[WeightedR]*);
              xtCalc = RScaled*st
                 +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]];
             ListOfArgs = 
             {xtCalc
             ,htEndVal
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,RScaled
             ,LifePosn+1};
            ProbOfArgs = 
              etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
             *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
             *Rprob[[LoopOverAggStates,LoopOverRVals]]
             *EmpStateProb[AggState,EmpState,LoopOverEmpStates]
             *AggStateProb[AggState,LoopOverAggStates];
            If[ProbOfArgs<0,Print[ListOfArgs];Print[ProbOfArgs]];
            RScaledTimesVxtArgGridDistArray[[
                 ClosestElementTo[x0MatGrid,xtCalc]
                ,ClosestElementTo[hMatGrid,htEndVal]
                ,LoopOverAggStates
                ,LoopOverEmpStates
                ,LoopOverepVals
                ,ClosestElementTo[RScaledMatGrid,RScaled]]] += ProbOfArgs;
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}];                                         (* End loop over AggStates  *)
  Return[RScaledTimesVxtArgGridDistArray];
];

FOCwrtRiskySharetMatArgGridDistGivenOmegatAllArg[st_,htEnd_,AggState_,EmpState_,RiskySharet_] := 
Block[{(*VtArgGridDistArray,WeightedR,xtCalc,ProbOfArgs,ListOfArgs,ProbOfArgs*)},
  htEndVal = htEnd;
  FOCwrtRiskySharetMatArgGridDistArray = FOCwrtRiskySharetMatArgArrayEmpty;
    Do[            (* over possible rates of return                       *)
     Do[           (* over possible future employment states              *)
      Do[          (* over possible future aggregate states               *)
       Do[         (* over possible future values of the permanent shock  *)
        Do[        (* over possible future values of the transitory shock *) 
            WeightedR = 
                       ((R[[LoopOverAggStates,LoopOverRVals]]*RiskySharet+Rcertain*(1-RiskySharet))/
              (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]));
            RGap = 
                 ((R[[LoopOverAggStates,LoopOverRVals]]-Rcertain)
                /(G[[LifePosn,AggState,EmpState]]*
                 epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
                ));
            (*Print[RGap];Print[ClosestElementTo[RGapMatGrid,RGap]];*)
            xtCalc = WeightedR*st
               +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]];
            ListOfArgs = 
             {xtCalc
             ,htEndVal
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,RGap};
            ProbOfArgs = 
              etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
             *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
             *Rprob[[LoopOverAggStates,LoopOverRVals]]
             *EmpStateProb[AggState,EmpState,LoopOverEmpStates]
             *AggStateProb[AggState,LoopOverAggStates];
            If[ProbOfArgs<0,Print[ListOfArgs];Print[ProbOfArgs]];
            FOCwrtRiskySharetMatArgGridDistArray[[
                 ClosestElementTo[x0MatGrid,xtCalc]
                ,ClosestElementTo[hMatGrid,htEndVal]
                ,LoopOverAggStates
                ,LoopOverEmpStates
                ,LoopOverepVals
                ,ClosestElementTo[RGapMatGrid,RGap]]] += ProbOfArgs;
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}];                                         (* End loop over AggStates  *)
  Return[FOCwrtRiskySharetMatArgGridDistArray];
];


ClearAll[OmegatArgGridDistGivenVtArgMat];
OmegatArgGridDistGivenVtArgMat[xt_,htStart_,AggState_,EmpState_,AggKt_] := 
Block[{(*VtArgGridDistArray,WeightedR,xtCalc,ProbOfArgs,ListOfArgs,ProbOfArgs*)},
  OmegatArgGridDistArray = OmegatArgArrayEmpty;
        ctValOmegatArg = ctMat[xt,htStart,AggState,EmpState,AggKt];
        stValOmegatArg = xt - ctValOmegatArg;
        htValOmegatArg = htStart(1-catchup)/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]) + catchup ctValOmegatArg;
        OmegatArgGridDistArray[[
          ClosestElementTo[s0MatGrid,stValOmegatArg],
          ClosestElementTo[hMatGrid,htValOmegatArg],
          AggState,
          EmpState]] = 1;
  Return[OmegatArgGridDistArray];
];

RGapTimesVxt[xt_,htStart_,AggState_,EmpState_,AggK_,RGap_,LifePosn_] := 
  RGap*VxtMat[xt,htStart,AggState,EmpState,AggK,LifePosn];

RGapTimesVxt[xt_,htStart_,AggState_,EmpState_,AggK_,RGap_] := 
  RGap*VxtMat[xt,htStart,AggState,EmpState,AggK,LifePosn];

RScaledTimesVxt[xt_,htStart_,AggState_,EmpState_,AggK_,RScaled_,LifePosn_] := 
  RScaled*VxtMat[xt,htStart,AggState,EmpState,AggK,LifePosn];

RScaledTimesVxt[xt_,htStart_,AggState_,EmpState_,AggK_,RScaled_] := 
  RScaled*VxtMat[xt,htStart,AggState,EmpState,AggK,LifePosn];

RScaledTimesVxtp1[xt_,htStart_,AggState_,EmpState_,AggK_,RScaled_,LifePosn_] := 
  RScaled*VxtMat[xt,htStart,AggState,EmpState,AggK,LifePosn+1];

RScaledTimesVxtp1[xt_,htStart_,AggState_,EmpState_,AggK_,RScaled_] := 
  RScaled*VxtMat[xt,htStart,AggState,EmpState,AggK,LifePosn+1];

