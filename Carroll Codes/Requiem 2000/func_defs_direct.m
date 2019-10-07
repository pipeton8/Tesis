(*

Define the functions which yield the value functions, marginal value functions, and
other useful constructs

*)

(*

OmegasInvt(s_{t},h_{t}) is defined in the mathematical appendix as the 
pseudoinverse of the expected value to the consumer from choosing the 
optimal portfolio share.  OmegasInvt and OmegahInvt are the derivatives of 
this function with respect to the two state variables

*)

OmegastDirect[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[LifePosn>=LifeLength,Return[0]];
  If[st<smin             ,Return[AvoidLikePlague]];
  If[st == smin && Max[etZeroProbVals]>0,Return[Infinity]];  
    PeriodOfOptimalRule=LifePosn;If[LifePosn<EarliestEtSolved,(* then *) PeriodOfOptimalRule = EarliestOmegatSolved];
    Return[
      Max[0,
      ((OmegasInvtDirectInterpFunc[[PeriodOfOptimalRule]][st,htEnd,AggState,EmpState])^
      (1/OmegastToOmegasInvtPower)
      )/OmegastToOmegasInvtMultiply] (* End Max[] *)
          (* The raw data from OmegastDirectRaw are raised to the power (1/-rho) to make the function nearly linear before interpolating *)
          (* The exponentiation here undoes that to recover the function in levels *)
    ]  (* End Return[] *)
];


OmegastDirectRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,(* then *) Return[0]];
  If[st == smin && etZeroProbVals[[AggState,EmpState]] > 0,Return[Infinity]];
  OmegasInvtDirectRiskyShare = RiskySharetDirect[st,htEnd,AggState,EmpState,AggK,LifePosn];
  If[OmegasInvtDirectRiskyShare>0 && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
          ROmegast = 
            (((R[[LoopOverAggStates,LoopOverRVals]]*
             OmegasInvtDirectRiskyShare+Rcertain*(1-OmegasInvtDirectRiskyShare))/
            (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
            )));(*Print[ROmegast]*);
            ListOfArgs = 
             {ROmegast*st
               +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-RiskyTimeCost*OneIfRisky
             ,htEnd
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1};
            ProbOfArgs = etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
                        *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
                        *Rprob[[LoopOverAggStates,LoopOverRVals]]
                        *EmpStateProb[AggState,EmpState,AggK,LoopOverEmpStates]
                        *AggStateProb[AggState,LoopOverAggStates];
(*            Print[{ListOfArgs,ProbOfArgs,ROmegast*Apply[VxtDirect,ListOfArgs],G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]}];*)
          ROmegast*
          Apply[VxtDirect,ListOfArgs]*
          ProbOfArgs
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]; (* End Return[] *)
];

Clear[OmegasInvtDirectRaw];
OmegasInvtDirectRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,Return[Infinity]];
  Return[
    OmegastToOmegasInvtMultiply*
    (OmegastDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn]^OmegastToOmegasInvtPower)
  ];
];
OmegasInvtDirectRaw[st_,htEnd_,AggState_,EmpState_] := OmegasInvtDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn] ;

Clear[OmegasInvtDirectRaw];
OmegasInvtDirectRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,Return[Infinity]];
  Return[
    OmegastToOmegasInvtMultiply*
    (OmegastDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn]^OmegastToOmegasInvtPower)
  ];
];
OmegasInvtDirectRaw[st_,htEnd_,AggState_,EmpState_] := OmegasInvtDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn] ;

OmegahtDirect[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] :=
Block[{},
  If[LifePosn>=LifeLength || HabitsMatter==False,Return[0]];
  If[st<0,Return[AvoidLikePlague]];
  Return[
    PeriodOfOptimalRule=LifePosn;If[LifePosn<EarliestOmegatSolved,(* then *) PeriodOfOptimalRule = EarliestOmegatSolved];
    Min[0,((OmegahInvtDirectInterpFunc[[PeriodOfOptimalRule]][st,htEnd,AggState,EmpState])^(1/OmegahtToOmegahInvtPower))/OmegahtToOmegahInvtMultiply]
          (* The raw data from OmegahInvt are raised to the power (1/(1-rho)) to make the function nearly linear *)
          (* The exponentiation here undoes that to recover the function in levels *)
  ] (* End Return[] *)
];


OmegahtDirectRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{OmegahtDirectRiskyShare},
  If[LifePosn>=LifeLength || HabitsMatter==False,  (* then *) Return[0]];
  If[st <= smin && etZeroProb>0,Return[Infinity]];
  OmegahtDirectRiskyShare = RiskySharetDirect[st,htEnd,AggState,EmpState,AggK,LifePosn];
  If[OmegahInvtRiskyShare>0  && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
             VhtDirect[
             ((R[[LoopOverAggStates,LoopOverRVals]]*OmegahtDirectRiskyShare+Rcertain*(1-OmegahtDirectRiskyShare))/
              (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]))st
                  +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-RiskyTimeCost*OneIfRisky
             ,htEnd
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1
            ]
          *etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
         *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
        *Rprob[[LoopOverAggStates,LoopOverRVals]]
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
      *EmpStateProb[AggState,EmpState,AggK,LoopOverEmpStates]
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
      *AggStateProb[AggState,LoopOverAggStates]
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ] (*End Return[]*) 
];

Clear[OmegahInvtDirectRaw];
OmegahInvtDirectRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,Return[Infinity]];
  Return[
    (OmegahtToOmegahInvtMultiply*
    OmegahtDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn])^OmegahtToOmegahInvtPower
  ];
];
OmegahInvtDirectRaw[st_,htEnd_,AggState_,EmpState_] := OmegahInvtDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn] ;

(*
CertEquiv is the expected marginal value raised to the power 1/-rho,
which is sort of like asking what is the certain level of consumption
that would yield the marginal utility in question
*)
CertEquivDirectRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[st < 0,Return[0]];
  If[st == smin && etZeroProb > 0,Return[0]];
  Return[
    (OmegastDirect[st,htEnd,AggState,EmpState,AggK,LifePosn] - catchup OmegahtDirect[st,htEnd,AggState,EmpState,AggK,LifePosn])^(-1/rho)
  ];
];
CertEquivDirectRaw[st_,htEnd_,AggState_,EmpState_] := CertEquivDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn];

ClearAll[CertEquivDirect];
CertEquivDirect[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := CertEquivDirectInterpFunc[[LifePosn]][st,htEnd,AggState,EmpState];


FOCwrtRiskySharetDirect[s_, h_, AggState_, EmpState_, RiskyShare_, LifePosn_] := 
  FOCwrtRiskySharetDirectInterpFunc[[
        Max[Min[EarliestVtSolved, EarliestOmegatSolved], LifePosn]
                             ]][s, h,AggState, EmpState, RiskyShare]

FOCwrtRiskySharetDirect[s_, h_, AggState_, EmpState_, RiskyShare_] := FOCwrtRiskySharetDirect[s, h, AggState, EmpState, RiskyShare, LifePosn];


ClearAll[FOCwrtRiskySharetDirectRaw];
FOCwrtRiskySharetDirectRaw[st_,htEnd_,AggState_,EmpState_,RiskySharet_,LifePosn_] := 
Block[{},
  If[st == smin,Return[FOCwrtRiskySharetDirectRaw[s0DirectGrid[[2]],htEnd,AggState,EmpState,RiskySharet,LifePosn]]];
  If[RiskySharet>0  && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  If[SolveForRiskyShare == False, (* then *) Return[0]];
  FOCRaw = 
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
        ((R[[LoopOverAggStates,LoopOverRVals]]-Rcertain)
        /(G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]*
          epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
         ))*
             VxtDirect[
             ((R[[LoopOverAggStates,LoopOverRVals]]*RiskySharet+Rcertain*(1-RiskySharet))/
              (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]))*
              st
                  +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-RiskyTimeCost*OneIfRisky
             ,htEnd
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1
            ]
          *etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
         *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
        *Rprob[[LoopOverAggStates,LoopOverRVals]]
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
       *EmpStateProb[AggState,EmpState,AggK,LoopOverEmpStates]
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
      *AggStateProb[AggState,LoopOverAggStates]
    ,{LoopOverAggStates,NumOfAggStates}];                                         (* End loop over AggStates  *)
  Return[FOCRaw]
];    
FOCwrtRiskySharetDirectRaw[st_,htEnd_,AggState_,EmpState_,RiskySharet_] := FOCwrtRiskySharetDirectRaw[st,htEnd,AggState,EmpState,RiskySharet,LifePosn];


ClearAll[OmegatDirectRaw];
OmegatDirectRaw[st_,htEnd_,AggState_,EmpState_,RiskySharet_,LifePosn_] := 
Block[{},
  If[RiskySharet>0  && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  FOCRaw = 
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
             VtDirectRaw[
             ((R[[LoopOverAggStates,LoopOverRVals]]*RiskySharet+Rcertain*(1-RiskySharet))/
              (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]))st
                  +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
                  -RiskyTimeCost*OneIfRisky
             ,htEnd
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1
            ]
          *etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
         *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
        *Rprob[[LoopOverAggStates,LoopOverRVals]]
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
       *EmpStateProb[AggState,EmpState,AggK,LoopOverEmpStates]
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
      *AggStateProb[AggState,LoopOverAggStates]
    ,{LoopOverAggStates,NumOfAggStates}];                                         (* End loop over AggStates  *)
  Return[FOCRaw]
];    


ClearAll[RiskySharetDirectRaw];
RiskySharetDirectRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[SolveForRiskyShare == False,Return[0]];
  If[st < smin,Print["Error: RiskySharetDirect called with st<smin."];Interrupt[]];
  If[st == smin,
      Return[RiskySharetDirectRaw[s0DirectGrid[[2]],htEnd,AggState,EmpState,AggK,LifePosn]]];
  If[(  FOCwrtRiskySharetDirect[st,htEnd,AggState,EmpState,0,LifePosn] > 0
     && FOCwrtRiskySharetDirect[st,htEnd,AggState,EmpState,1,LifePosn] > 0),
     (* then *)
     If[RiskyTimeCost == 0, Return[1]];
     If[OmegatDirectRaw[st,htEnd,AggState,EmpState,1,LifePosn]
       >OmegatDirectRaw[st,htEnd,AggState,EmpState,0,LifePosn],
       (*then*)Return[1],
       (*else*)Return[0]
       ](* End If EtVxtp1Raw *);
  ];    (* End if FOC *)
  If[(  FOCwrtRiskySharetDirect[st,htEnd,AggState,EmpState,0,LifePosn] < 0
     && FOCwrtRiskySharetDirect[st,htEnd,AggState,EmpState,1,LifePosn] < 0),
     Return[0]];
  RiskySharetp1 = RiskySharetDirect[st,htEnd,AggState,EmpState,AggK,LifePosn+1];
  If[(RiskySharetp1==0) || (RiskySharetp1==1),
    (* then *) RiskyShareSearchMin = 0;RiskyShareSearchMax=1,
    (* else *) RiskyShareSearchMin = RiskySharetp1*.95;RiskyShareSearchMax=RiskySharetp1+.05*(1-RiskySharetp1)];
  RiskyShareResults=
    Check[FindRoot[
      (FOCwrtRiskySharetDirect[st,htEnd,AggState,EmpState,RiskyShare,LifePosn]
      /VxtDirect[st+UnempWage,htEnd,AggState,EmpState,1,LifePosn+1])==0
    ,{RiskyShare,{RiskyShareSearchMin,RiskyShareSearchMax}},MaxIterations->50],err,FindRoot::frsec];
  If[RiskyShareResults == err,
    RiskyShareResults=
      Print["Error in RiskySharetDirectRaw.  Values:"
            ,{st,htEnd,AggState,EmpState,AggK,LifePosn}];
      Print["Now trying to use FOCwrtRiskySharetDirectRaw."];
      Check[FindRoot[
        (FOCwrtRiskySharetDirectRaw[st,htEnd,AggState,EmpState,RiskyShare,LifePosn]
        /VxtDirect[st+UnempWage,htEnd,AggState,EmpState,1,LifePosn+1])==0
      ,{RiskyShare,{RiskyShareSearchMin,RiskyShareSearchMax}},MaxIterations->50],err,FindRoot::frsec];
    Interrupt[];
    If[RiskyShareResults == err,    
      Print["Error in RiskySharetDirectRaw even using FOCwrtRiskySharetDirectRaw.  Values:"
            ,{st,htEnd,AggState,EmpState,AggK,LifePosn}];
      Interrupt[]];
  ];
  If[Length[RiskyShareResults]>2,Print["RiskyShareResults of improper form; probably FindRoot failed"];Interrupt[]];
  RiskySharetDirectVal = Round[(RiskyShare /. RiskyShareResults)*1000]/1000;
  If[RiskySharetDirectVal>1, (* then *) RiskySharetDirectVal=1];
  If[RiskySharetDirectVal<0, (* then *) RiskySharetDirectVal=0];
  If[RiskyTimeCost>0,
    (* then *)
      If[OmegatDirectRaw[st,htEnd,AggState,EmpState,RiskySharetDirectVal,LifePosn]
        <OmegatDirectRaw[st,htEnd,AggState,EmpState,0,LifePosn],(*then*)Return[0]]
  ]; (* End If RiskyTimeCost > 0 *)
  Return[RiskySharetDirectVal //N];
];

RiskySharetDirectRaw[st_,htEnd_,AggState_,EmpState_] := RiskySharetDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn];

ClearAll[RiskySharetDirect];
RiskySharetDirect[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := 
    If[SolveForRiskyShare == True,
      (* then *) Return[Max[Min[RiskySharetDirectInterpFunc[[LifePosn]][st,htEnd,AggState,EmpState],1],0]],
      (* else *) Return[0]];
RiskySharetDirect[st_,htEnd_,AggState_,EmpState_]           := RiskySharetDirect[st,htEnd,AggState,EmpState,AggK,LifePosn];


ClearAll[VxtDirect];
VxtDirect[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[xt   < xmin         ,Return[AvoidLikePlague]];  (* Extremely high marginal utility should make them avoid forbidden x's *)
  If[LifePosn>= LifeLength,Return[(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]] htStart^-gamma)^(1-rho)*xt^(-rho)]];
  If[LifePosn <EarliestVtSolved,PeriodOfOptimalRule = EarliestVtSolved,PeriodOfOptimalRule = LifePosn]; 
  Return[
    Max[0,
      (VxInvtDirectInterpFunc[[PeriodOfOptimalRule]][xt,htStart,AggState,EmpState,AggKt])^-rho
      ] (* End Max[] *)
  ];    (* End Return[] *)
]; (* End of Block *)



ClearAll[VxtDirectRaw];
VxtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[0]];
  If[xt < xmin,Return[Infinity]];
  If[xt == xmin && etZeroProb>0,Return[Infinity]];
  If[xt>xmax, 
      (* then use the interpolatingfunction, which is actually more accurate outside the grid *)
        Return[VxtDirect[xt,htStart,AggState,EmpState,AggKt,LifePosn]]];
  ctVxtDirect = ctDirect[xt,htStart,AggState,EmpState,AggKt,LifePosn];
  stVxtDirect = xt-ctVxtDirect;
  GNFactorVxtDirect = (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(1-rho);
  If[LifePosn==LifeLength,Return[GNFactorVxtDirect*(ctVxtDirect^-rho) htStart^(gamma rho - gamma)]];
  htEndVxtDirect = htStart(1-catchup)/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]) + catchup ctVxtDirect;
  If[ctVxtDirect < 0,Return[AvoidLikePlague]];
  If[Chop[stVxtDirect] <= 0 && LifePosn<LifeLength,
      If[etZeroProb>0,
        Print["Error: ctDirect>=xt in VxtDirectRaw."];Print[{xt,htStart,AggState,EmpState,AggKt}];Interrupt[];,
        Return[GNFactorVxtDirect*dudct[ctVxtDirect,htStart]]]];
  VxtDirectOut = GNFactorVxtDirect*
          ((G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(gamma rho - gamma))*
          beta[[LifePosn+1,EmpState]] OmegastDirect[stVxtDirect,htEndVxtDirect,AggState,EmpState,AggK,LifePosn];
  If[VxtDirectOut<0,
    (* then *) 
      Print["Warning: VxtDirectRaw[",xt,",",htStart,",",AggState,",",EmpState,",",AggKt,",",LifePosn,"]=",VxtDirectOut,"<0."];
  ]; (* End If VxtDirectOut < 0 *)
Return[VxtDirectOut]
]

VxtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := VxtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn]



ClearAll[VxInvtDirectRaw];
VxInvtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[Infinity]];
  Return[
    VxtToVxInvtMultiply*(VxtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn])^(VxtToVxInvtPower)
  ];
];
VxInvtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := VxInvtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn];


VhtDirect[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[HabitsMatter == False,Return[0]];
  If[xt < xmin, Return[-AvoidLikePlague]];  (* Extremely high marginal utility should make them avoid forbidden x's *)
  If[LifePosn == LifeLength,
      (* then *) Return[((G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(1-rho))*dudht[xt,htStart]]];
  If[LifePosn < EarliestVtSolved,PeriodOfOptimalRule = EarliestVtSolved,PeriodOfOptimalRule = LifePosn]; 
  Return[
    Min[0,(VhInvtDirectInterpFunc[[LifePosn]][xt,htStart,AggState,EmpState,AggKt]^(1/VhtToVhInvtPower))/VhtToVhInvtMultiply]
  ] (* End Return[] *)
]; (* End of Block *)


ClearAll[VhtDirectRaw];
VhtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[HabitsMatter == False,Return[0]];
  If[LifePosn>LifeLength    ,Return[0]];
  If[xt<xmin || htStart<hmin,Return[-AvoidLikePlague]];
  ctVhtDirect = ctDirect[xt,htStart,AggState,EmpState,AggKt,LifePosn];
  If[Chop[ctVhtDirect] == 0,Return[-Infinity]];
  stVhtDirect = Chop[xt-ctVhtDirect];
  If[stVhtDirect <= 0 && LifePosn<LifeLength,
      If[etZeroProb>0,
        Print["Error: ctDirect>=xt in VhtDirectRaw."];Interrupt[]]];
  htEndVhtDirect = htStart(1-catchup)/G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]] + catchup ctVhtDirect;
  GNFactor = (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(1-rho);
  If[LifePosn == LifeLength,Return[GNFactor*dudht[ctVhtDirect,htStart]]];
  If[ctVhtDirect < 0,Return[-AvoidLikePlague]];
  Return[
    GNFactor*
    (dudht[ctVhtDirect,htStart] +
    (1-catchup) (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(rho gamma - gamma - 1)*
    beta[[LifePosn+1,EmpState]] OmegahtDirect[stVhtDirect,htEndVhtDirect,AggState,EmpState,AggK,LifePosn]
    )
  ];
];

ClearAll[VhInvtDirectRaw];
VhInvtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[Infinity]];
  Return[
    (VhtToVhInvtMultiply*VhtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn])^(VhtToVhInvtPower)
  ];
];
VhInvtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := VhInvtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn];

ClearAll[ctDirectRaw];
ctDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,Return[xt]];
  If[xt<xmin,Return[0.]];
  If[xt==xmin && etZeroProb >0,Return[0]];
  If[xt==xmin && etUnempProb>0,Return[UnempWage]];
  OmegaInvMultiplyFactor = 
    (((G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])/htStart)^(rho gamma - gamma)*
        beta[[LifePosn+1,EmpState]])^(-1/rho);
  htFactor = htStart/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]);
  ctp1Val  = ctDirect[xt,htStart,AggState,EmpState,AggKt,LifePosn+1];
  ctSearchMax = Max[Min[ctp1Val + (xmax-ctp1Val)*.01,xt],CertEquivDirect[0,1,1,1,LifePosn]];
  ctSearchMin = .9*ctp1Val;
  If[LifePosn == LifeLength-1,ctSearchMin = .49 xt;ctSearchMax = .51 xt];
  FOCResults = 
    Check[FindRoot[
      c == OmegaInvMultiplyFactor*CertEquivDirect[xt-c,htFactor*(1-catchup)+ catchup c,AggState,EmpState,AggK,LifePosn]
    ,{c,{ctSearchMin,ctSearchMax}},MaxIterations->50],err,FindRoot::frsec];
  If[FOCResults == err,
      Print["Error in FOC.  Values:"
            ,{xt,htStart,AggState,EmpState,AggKt,LifePosn}];
      Interrupt[]];
  If[Length[FOCResults]>2,Print["FOCResults of improper form; probably FindRoot failed."];Interrupt[]];
  ctVal = (c /. FOCResults);
  If[ctVal<0,ctVal=xt];
  If[Not[NumberQ[ctVal]], (* then *) Print["FindViaFOC failed."];Print[##];Interrupt[]]; 
  st = xt-ctVal;If[st < 0,st=0;ctVal=xt;(*#Print["Error in ChoiceBasic: st < 0"];Print[##];Interrupt[]#*)];
  Return[ctVal];
]; (* End If[xt > xmin] *)
ctDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := ctDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn];
  

ctDirect[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := ctDirectInterpFunc[[LifePosn]][xt,htStart,AggState,EmpState,AggKt];
ctDirect[xt_,htStart_,AggState_,EmpState_,AggKt_          ] := ctDirectInterpFunc[[LifePosn]][xt,htStart,AggState,EmpState,AggKt];

stDirect[x_,h_,AggState_,EmpState_,AggK_,LifePosn_] := x - ctDirect[x,h,AggState,EmpState,AggK,LifePosn];
htDirect[x_,h_,AggState_,EmpState_,AggK_,LifePosn_] := (1-catchup)*h/(G[[LifePosn,AggState,EmpState]])+catchup*ctDirect[x,h,AggState,EmpState,AggK,LifePosn];


OmegatDirect[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[LifePosn>=LifeLength,Return[0]];
  If[st<smin             ,Return[-Infinity]];
    PeriodOfOptimalRule=LifePosn;If[LifePosn<EarliestOmegatSolved,(* then *) PeriodOfOptimalRule = EarliestOmegatSolved];
    Return[
      Min[0,
      ((OmegaInvtInterpFunc[[PeriodOfOptimalRule]][st,htEnd,AggState,EmpState])^
      (1/OmegatToOmegaInvtPower)
      )/OmegatToOmegaInvtMultiply] (* End Max[] *)
          (* The raw data from OmegatRaw are raised to the power (1/(1-rho)) to make the function nearly linear before interpolating *)
          (* The exponentiation here undoes that to recover the function in levels *)
    ]  (* End Return[] *)
];


OmegatDirectRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,(* then *) Return[0]];
  If[st == smin && etZeroProb > 0,Return[-Infinity]];
  OmegatDirectRiskyShare = RiskySharetDirect[st,htEnd,AggState,EmpState,AggK,LifePosn];
  If[OmegatDirectRiskyShare>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
          ROmegast = 
            (((R[[LoopOverAggStates,LoopOverRVals]]*
             OmegasInvtRiskyShare+Rcertain*(1-OmegasInvtRiskyShare))/
            (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
            )));(*Print[ROmegast]*);
            ListOfArgs = 
             {ROmegast*st
               +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-OneIfRisky*RiskyTimeCost
             ,htEnd
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1};
             (*Print["Stocks:",ListOfArgs];*)
            Apply[VtDirect,ListOfArgs]
          *etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
         *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
        *Rprob[[LoopOverAggStates,LoopOverRVals]]
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
       *EmpStateProb[AggState,EmpState,AggK,LoopOverEmpStates]
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
      *AggStateProb[AggState,LoopOverAggStates]
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]
];

Clear[OmegaInvtDirectRaw];
OmegaInvtDirectRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,Return[Infinity]];
  Return[
    (OmegatToOmegaInvtMultiply*OmegatDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn])^OmegatToOmegaInvtPower
  ];
];
OmegaInvtDirectRaw[st_,htEnd_,AggState_,EmpState_] := OmegaInvtDirectRaw[st,htEnd,AggState,EmpState,AggK,LifePosn] ;


ClearAll[VtDirect];
VtDirect[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[xt   < xmin         ,Return[-Infinity]];  (* Extremely high negative utility should make them avoid forbidden x's *)
  If[LifePosn>= LifeLength,Return[((G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(1-rho))*u[xt,htStart]]];
  If[LifePosn <EarliestVtSolved,PeriodOfOptimalRule = EarliestVtSolved,PeriodOfOptimalRule = LifePosn]; 
  Return[
    Min[0,
      (VInvtDirectInterpFunc[[PeriodOfOptimalRule]][xt,htStart,AggState,EmpState,AggKt]^(1/VtToVInvtPower))/VtToVInvtMultiply
      ] (* End Max[] *)
  ];    (* End Return[] *)
]; (* End of Block *)


ClearAll[VtDirectRaw];
VtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[0]];
  If[xt <= xmin,Return[-Infinity]];
  If[xt>xmax, 
      (* then use the interpolatingfunction, which is actually more accurate outside the grid *)
        Return[VtDirect[xt,htStart,AggState,EmpState,AggKt,LifePosn]]];
  xtLeft = xt;
  ctVtDirect = ctDirect[xtLeft,htStart,AggState,EmpState,AggKt,LifePosn];
  stVtDirect = xtLeft-ctVtDirect;
  GNFactorVtDirect = (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(1-rho);
  If[LifePosn==LifeLength,Return[GNFactorVtDirect*u[xtLeft,htStart]]];
  htEndVtDirect = htStart(1-catchup)/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]) + catchup ctVtDirect;
  If[ctVtDirect < 0,Return[AvoidLikePlague]];
  If[Chop[stVtDirect] <= 0 && LifePosn<LifeLength,
      If[etZeroProb>0,
        Print["Error: ctDirect>=xtLeft in VtDirectRaw."];Print[{xtLeft,htStart,AggState,EmpState,epVals[[LifePosn,AggState,EmpState,AggKt]]}];Interrupt[];,
        Return[u[ctVtDirect,htStart]]]];
  VtDirectOut = GNFactorVtDirect*
          (u[ctVtDirect,htStart]+
          (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(gamma(rho-1))*
          beta[[LifePosn+1,EmpState]] OmegatDirect[stVtDirect,htEndVtDirect,AggState,EmpState,AggK,LifePosn]
          );
  If[VtDirectOut>0,
    (* then *) 
      Print["Warning: VtDirectRaw[",xtLeft,",",htStart,",",AggState,",",EmpState,",",epVals[[LifePosn,AggState,EmpState,AggKt]],",",LifePosn,"]=",VtDirectOut,">0."];
  ]; (* End If VtDirectOut > 0 *)
Return[VtDirectOut]
]
VtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := VtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn]

ClearAll[VtDirectRawAlt];
VtDirectRawAlt[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[0]];
  If[xt <= xmin,Return[-Infinity]];
  If[xt>xmax, 
      (* then use the interpolatingfunction, which is actually more accurate outside the grid *)
        Return[VtDirect[xt,htStart,AggState,EmpState,AggKt,LifePosn]]];
  xtLeft = xt;
  ctVtDirect = ctDirect[xtLeft,htStart,AggState,EmpState,AggKt,LifePosn];
  stVtDirect = xtLeft-ctVtDirect;
  GNFactorVtDirect = (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(1-rho);
  If[LifePosn==LifeLength,Return[GNFactorVtDirect*u[xtLeft,htStart]]];
  htEndVtDirect = htStart(1-catchup)/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]) + catchup ctVtDirect;
  If[ctVtDirect < 0,Return[AvoidLikePlague]];
  If[Chop[stVtDirect] <= 0 && LifePosn<LifeLength,
      If[etZeroProb>0,
        Print["Error: ctDirect>=xtLeft in VtDirectRaw."];Print[{xtLeft,htStart,AggState,EmpState,AggKt}];Interrupt[];,
        Return[u[ctVtDirect,htStart]]]];
  VtDirectOut = GNFactorVtDirect*
          (u[ctVtDirect,htStart]+
          (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(gamma(rho-1))*
          beta[[LifePosn+1,EmpState]] OmegatDirect[stVtDirect,htEndVtDirect,AggState,EmpState,AggK,LifePosn]
          );
  VxtOut = VxtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn];
  VhtOut = VhtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn];
  VAggtOut = 0;
  VEmptOut = 0;
  
  If[VtDirectOut>0,
    (* then *) 
      Print["Warning: VtDirectRaw[",xtLeft,",",htStart,",",AggState,",",EmpState,",",AggKt,",",LifePosn,"]=",VtDirectOut,">0."];
  ]; (* End If VtDirectOut > 0 *)
Return[VtDirectOut]
]
VtDirectRawAlt[xt_,htStart_,AggState_,EmpState_,AggKt_] := VtDirectRawAlt[xt,htStart,AggState,EmpState,AggKt,LifePosn]


ClearAll[VInvtDirectRaw];
VInvtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[Infinity]];
  Return[
    (VtToVInvtMultiply*VtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn])^(VtToVInvtPower)
  ];
];
VInvtDirectRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := VInvtDirectRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn];

MPCDirect[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
  (ctDirect[xt+.001,htStart,AggState,EmpState,AggKt,LifePosn]-
   ctDirect[xt-.001,htStart,AggState,EmpState,AggKt,LifePosn])/.002;

MPCDirect[xt_,htStart_,AggState_,EmpState_,AggKt_] := MPC[xt,htStart,AggState,EmpState,AggKt,LifePosn];



ClearAll[EtLogctp1DirectGtp1Raw];
EtLogctp1DirectGtp1Raw[st_,ht_,AggState_,EmpState_] := 
Block[{},
  RiskyShareVal = RiskySharetDirect[st,ht,AggState,EmpState,AggK,LifePosn];
  If[RiskyShareVal>0 && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
          Gctp1 = G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]];
          Rctp1 = 
            (((R[[LoopOverAggStates,LoopOverRVals]]*
             RiskyShareVal+Rcertain*(1-RiskyShareVal))/
            (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
            )));(*Print[Rctp1]*);
            ListOfArgs = 
             {Rctp1*st
               +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-RiskyTimeCost*OneIfRisky
             ,ht
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1};
            ProbOfArgs = etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
                        *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
                        *Rprob[[LoopOverAggStates,LoopOverRVals]]
                        *EmpStateProb[AggState,EmpState,AggK,LoopOverEmpStates]
                        *AggStateProb[AggState,LoopOverAggStates];
(*            Print[{ListOfArgs,ProbOfArgs,Rctp1*Apply[ctDirect,ListOfArgs]}];*)
          Log[Gctp1*Apply[ctDirect,ListOfArgs]]*ProbOfArgs
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]; (* End Return[] *)
];


ClearAll[EtLogctp1DirectGtp1SquaredRaw];
EtLogctp1DirectGtp1SquaredRaw[st_,ht_,AggState_,EmpState_] := 
Block[{},
  RiskyShareVal = RiskySharetDirect[st,ht,AggState,EmpState,AggK,LifePosn];
  If[RiskyShareVal>0 && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
          Gctp1 = G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]];
          Rctp1 = 
            (((R[[LoopOverAggStates,LoopOverRVals]]*
             RiskyShareVal+Rcertain*(1-RiskyShareVal))/
            (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
            )));(*Print[Rctp1]*);
            ListOfArgs = 
             {Rctp1*st
               +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-RiskyTimeCost*OneIfRisky
             ,ht
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1};
            ProbOfArgs = etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
                        *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
                        *Rprob[[LoopOverAggStates,LoopOverRVals]]
                        *EmpStateProb[AggState,EmpState,AggK,LoopOverEmpStates]
                        *AggStateProb[AggState,LoopOverAggStates];
(*            Print[{ListOfArgs,ProbOfArgs,Rctp1*Apply[ctList,ListOfArgs]}];*)
          ((Log[Gctp1*Apply[ctDirect,ListOfArgs]])^2)*ProbOfArgs
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]; (* End Return[] *)
];

ClearAll[EtLogctp1DirectGtp1OctRaw];
EtLogctp1DirectGtp1OctRaw[xt_,ht_,AggState_,EmpState_,AggKt_] := 
Block[{},
  If[LifePosn>=LifeLength,(* then *) Return[0]];
  ctVal = ctDirect[xt,ht,AggState,EmpState,AggKt];
  stVal = xt-ctVal;
  htVal = ht(1-catchup)/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]) + catchup ctVal;
  Return[EtLogctp1DirectGtp1Raw[stVal,htVal,AggState,EmpState]-Log[ctVal]];
]

ClearAll[EtLogctp1DirectGtp1OctSquaredRaw];
EtLogctp1DirectGtp1OctSquaredRaw[xt_,ht_,AggState_,EmpState_,AggKt_] := 
Block[{},
  If[LifePosn>=LifeLength,(* then *) Return[0]];
  If[xt == xmin && etZeroProb > 0,Return[Infinity]];
  ctVal = ctDirect[xt,ht,AggState,EmpState,AggKt];
  stVal = xt-ctVal;
  htVal = ht(1-catchup)/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]) + catchup ctVal;
  RiskyShare = RiskySharetDirect[st,htVal,AggState,EmpState,AggK,LifePosn];
  If[RiskyShare>0 && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
          Gctp1 = G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]];
          Rctp1 = 
            (((R[[LoopOverAggStates,LoopOverRVals]]*
             RiskyShare+Rcertain*(1-RiskyShare))/
            (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
            )));(*Print[Rctp1]*);
            ListOfArgs = 
             {Rctp1*stVal
               +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-RiskyTimeCost*OneIfRisky
             ,htVal
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1};
            ProbOfArgs = etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
                        *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
                        *Rprob[[LoopOverAggStates,LoopOverRVals]]
                        *EmpStateProb[AggState,EmpState,AggK,LoopOverEmpStates]
                        *AggStateProb[AggState,LoopOverAggStates];
(*            Print[{ListOfArgs,ProbOfArgs,Rctp1*Apply[ctDirect,ListOfArgs]}];*)
          (Log[Gctp1*Apply[ctDirect,ListOfArgs]/ctVal]^2)*ProbOfArgs
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]; (* End Return[] *)
];


ClearAll[EtEulerDirect];
EtEulerDirect[xt_,ht_,AggState_,EmpState_,AggKt_] := 
Block[{},
  If[LifePosn>=LifeLength,(* then *) Return[0]];
  If[xt == xmin && etZeroProb > 0,Return[Infinity]];
  ctVal = ctDirect[xt,ht,AggState,EmpState,AggKt];
  stVal = xt-ctVal;
  htVal = ht(1-catchup)/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]) + catchup ctVal;
  RiskyShareVal = RiskySharetDirect[st,htVal,AggState,EmpState,AggK,LifePosn];
  If[RiskyShareVal>0 && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
          Gctp1 = G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]] epVals[[LifePosn,AggState,EmpState,AggKt]];
          Rctp1 = 
            (((R[[LoopOverAggStates,LoopOverRVals]]*
             RiskyShareVal+Rcertain*(1-RiskyShareVal))/
            (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
            )));(*Print[Rctp1]*);
            ListOfArgs = 
             {Rctp1*stVal
               +etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-RiskyTimeCost*OneIfRisky
             ,htVal
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,LoopOverepVals
             ,LifePosn+1};
            ProbOfArgs = etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
                        *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
                        *Rprob[[LoopOverAggStates,LoopOverRVals]]
                        *EmpStateProb[AggState,EmpState,AggK,LoopOverEmpStates]
                        *AggStateProb[AggState,LoopOverAggStates];
(*            Print[{ListOfArgs,ProbOfArgs,Rctp1*Apply[ctDirect,ListOfArgs]}];*)
          (DiscountFactor*Rctp1*(Gctp1^(1-rho))*(Apply[ctDirect,ListOfArgs]/ctVal)^-rho)*ProbOfArgs
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]; (* End Return[] *)
];

ClearAll[EtEulerDirectApprox];
EtEulerDirectApprox[xt_,ht_,AggState_,EmpState_,AggKt_] := 
  Rcertain DiscountFactor*
  (1 - rho       EtLogctp1DirectGtp1Oct[xt,ht,AggState,EmpState,AggKt] 
     +((rho^2)/2)EtLogctp1DirectGtp1OctSquared[xt,ht,AggState,EmpState,AggKt]);


ExtraDefsFileName = "func_defs_direct_"<>ProblemToSolveString<>".m";

If[Length[FileNames[ExtraDefsFileName]]>0,Get[ExtraDefsFileName]];