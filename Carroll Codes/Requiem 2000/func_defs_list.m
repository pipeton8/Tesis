(*

Define the functions which yield the value functions, marginal value functions, and
other useful constructs

*)

AggSList[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  Return[(AggkSSByAggState[[AggState]] + kAR1ByAggState[[AggState]] (AggK-AggkSSByAggState[[AggState]]))/(1-Depreciation)];
(*  AggX = AggK + AggK^alpha;
    Return[AggX - ctList[AggX,htStart,AggState,EmpState,AggK,LifePosn+1]]
*)
  ];
(*

OmegasInvt(s_{t},h_{t}) is defined in the mathematical appendix as the 
pseudoinverse of the expected value to the consumer from choosing the 
optimal portfolio share.  OmegasInvt and OmegahInvt are the derivatives of 
this function with respect to the two state variables

*)

OmegastList[st_,htEnd_,AggState_,EmpState_,AggS_,LifePosn_] := 
Block[{},
  If[LifePosn>=LifeLength,Return[0]];
  If[st<smin             ,Return[AvoidLikePlague]];
  If[st == smin && Max[etZeroProbVals]>0,Return[Infinity]];
    PeriodOfOptimalRule=LifePosn;If[LifePosn<EarliestEtSolved,(* then *) PeriodOfOptimalRule = EarliestOmegatSolved];
    Return[
      Max[0,
      ((OmegasInvtListInterpFunc[[PeriodOfOptimalRule]][st,htEnd,AggState,EmpState,AggS])^
      (1/OmegastToOmegasInvtPower)
      )/OmegastToOmegasInvtMultiply] (* End Max[] *)
          (* The raw data from OmegastRaw are raised to the power (1/-rho) to make the function nearly linear before interpolating *)
          (* The exponentiation here undoes that to recover the function in levels *)
    ]  (* End Return[] *)
];


OmegahtList[st_,htEnd_,AggState_,EmpState_,AggS_,LifePosn_] :=
Block[{},
  If[LifePosn>=LifeLength || HabitsMatter==False,Return[0]];
  If[st<0,Return[AvoidLikePlague]];
  Return[
    PeriodOfOptimalRule=LifePosn;If[LifePosn<EarliestOmegatSolved,(* then *) PeriodOfOptimalRule = EarliestOmegatSolved];
    Min[0,((OmegahInvtListInterpFunc[[PeriodOfOptimalRule]][st,htEnd,AggState,EmpState,AggS])^(1/OmegahtToOmegahInvtPower))/OmegahtToOmegahInvtMultiply]
          (* The raw data from OmegahInvt are raised to the power (1/(1-rho)) to make the function nearly linear *)
          (* The exponentiation here undoes that to recover the function in levels *)
  ] (* End Return[] *)
];
OmegahtList[st_,htEnd_,AggState_,EmpState_,AggS_] :=OmegahtList[st,htEnd,AggState,EmpState,AggS] ;

(*
CertEquiv is the expected marginal value raised to the power 1/-rho,
which is sort of like asking what is the certain level of consumption
that would yield the marginal utility in question
*)
CertEquivListRaw[st_,htEnd_,AggState_,EmpState_,AggS_,LifePosn_] := Block[{},
  If[st < 0,Return[0]];
  If[st == smin && etZeroProbVals[[AggState,EmpState]] > 0,Return[0]];
  Return[
    (OmegastList[st,htEnd,AggState,EmpState,AggS,LifePosn] - catchup OmegahtList[st,htEnd,AggState,EmpState,AggS,LifePosn])^(-1/rho)
  ];
];
CertEquivListRaw[st_,htEnd_,AggState_,EmpState_,AggS_] := CertEquivListRaw[st,htEnd,AggState,EmpState,AggS,LifePosn];

ClearAll[CertEquivList];
CertEquivList[st_,htEnd_,AggState_,EmpState_,AggS_,LifePosn_] := 
    Return[CertEquivListInterpFunc[[LifePosn]][st,htEnd,AggState,EmpState,AggS]];



FOCwrtRiskySharetList[s_, h_, AggState_,EmpState_,AggS_,RiskyShare_,LifePosn_] := 
  FOCwrtRiskySharetListInterpFunc[[
        Max[Min[EarliestVtSolved, EarliestOmegatSolved], LifePosn]
                             ]][s,h,AggState,EmpState,AggS,RiskyShare];


ClearAll[OmegatListRaw];
OmegatListRaw[st_,htEnd_,AggState_,EmpState_,AggS_,RiskySharet_,LifePosn_] := 
Block[{},
  If[RiskySharet>0  && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  OmegatVal = 
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
             AggKtp1 = AggkSSByAggState[[AggState]] 
                     + kAR1ByAggState[[AggState]]*
                       (AggS*(1-Depreciation)/(GrowthByAggState[[LoopOverAggStates]])
                        -AggkSSByAggState[[AggState]]
                       );
             (G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]] epVals[[LifePosn,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]])^((1-rho)(1-gamma))*
             VtListRaw[
             ((R[[LoopOverAggStates,LoopOverRVals]]*RiskySharet+Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1])*(1-RiskySharet))/
              (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]))st
                  +wFunc[AggKtp1]*etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
                  -RiskyTimeCost*OneIfRisky
             ,htEnd
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,AggKtp1
             ,LifePosn+1
            ]
          *etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
         *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
        *Rprob[[LoopOverAggStates,LoopOverRVals]]
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                      (* End loop over RStates *)
       *EmpStateProb[LoopOverAggStates,EmpState,LoopOverEmpStates]
     ,{LoopOverEmpStates,NumOfEmpStates}]                                                   (* End loop over EmpStates  *)
      *AggStateProb[AggState,LoopOverAggStates]
    ,{LoopOverAggStates,NumOfAggStates}];                                                   (* End loop over AggStates  *)
  Return[OmegatVal]
];    


ClearAll[RiskySharetListRaw];
RiskySharetListRaw[st_,htEnd_,AggState_,EmpState_,AggS_,LifePosn_] := Block[{},
  If[SolveForRiskyShare == False,Return[0]];
  If[st < smin,Print["Error: RiskySharetList called with st<smin."];Interrupt[]];
  If[st == 0,
      Return[RiskySharetListRaw[s0ListGrid[[2]],htEnd,AggState,EmpState,AggS,LifePosn]]];
  If[(  FOCwrtRiskySharetList[st,htEnd,AggState,EmpState,AggS,0,LifePosn] > 0
     && FOCwrtRiskySharetList[st,htEnd,AggState,EmpState,AggS,1,LifePosn] > 0),
     (* then at a risky share of 0 and of 1 they want to increase their risky holdings, so return 1 *)
     If[RiskyTimeCost == 0, Return[1]];
     If[OmegatListRaw[st,htEnd,AggState,EmpState,AggS,1,LifePosn]
       >OmegatListRaw[st,htEnd,AggState,EmpState,AggS,0,LifePosn],
       (*then*)Return[1], (* if it is worth the time cost, return 1     *)
       (*else*)Return[0]  (* if it is not worth the time cost, return 0 *)
       ](* End If EtVxtp1Raw *);
  ];    (* End if FOC *)
  If[(  FOCwrtRiskySharetList[st,htEnd,AggState,EmpState,AggS,0,LifePosn] < 0
     && FOCwrtRiskySharetList[st,htEnd,AggState,EmpState,AggS,1,LifePosn] < 0),
     (* then at a risky share of zero and 1 they want to reduce their holdings, so return 0 *)
     Return[0]];
  RiskySharetp1 = RiskySharetList[st,htEnd,AggState,EmpState,AggS,LifePosn+1];
  If[(RiskySharetp1==0) || (RiskySharetp1==1),
    (* then *) RiskyShareSearchMin = 0;RiskyShareSearchMax=1,
    (* else *) RiskyShareSearchMin = RiskySharetp1*.95;RiskyShareSearchMax=RiskySharetp1+.05*(1-RiskySharetp1)];
  RiskyShareResults=
    Check[FindRoot[
      (FOCwrtRiskySharetList[st,htEnd,AggState,EmpState,AggS,RiskyShare,LifePosn]
      /VxtList[st+UnempWage,htEnd,AggState,EmpState,AggS,LifePosn+1])==0
    ,{RiskyShare,{RiskyShareSearchMin,RiskyShareSearchMax}},MaxIterations->50],err,FindRoot::frsec];
  If[RiskyShareResults == err,
    RiskyShareResults=
      Print["Error in RiskySharetListRaw.  Values:"
            ,{st,htEnd,AggState,EmpState,AggS,LifePosn}];
      Print["Now trying to use FOCwrtRiskySharetListRaw."];
      Check[FindRoot[
        (FOCwrtRiskySharetListRaw[st,htEnd,AggState,EmpState,AggS,RiskyShare,LifePosn]
        /VxtList[st+UnempWage,htEnd,AggState,EmpState,AggS,LifePosn+1])==0
      ,{RiskyShare,{RiskyShareSearchMin,RiskyShareSearchMax}},MaxIterations->50],err,FindRoot::frsec];
    Interrupt[];
    If[RiskyShareResults == err,    
      Print["Error in RiskySharetListRaw even using FOCwrtRiskySharetListRaw.  Values:"
            ,{st,htEnd,AggState,EmpState,AggS,LifePosn}];
      Interrupt[]];
  ];
  If[Length[RiskyShareResults]>2,Print["RiskyShareResults of improper form; probably FindRoot failed"];Interrupt[]];
  RiskySharetVal = Round[(RiskyShare /. RiskyShareResults)*1000]/1000;
  If[RiskySharetVal>1, (* then *) RiskySharetVal=1];
  If[RiskySharetVal<0, (* then *) RiskySharetVal=0];
  If[RiskyTimeCost>0,
    (* then *)
      If[OmegatListRaw[st,htEnd,AggState,EmpState,AggS,RiskySharetVal,LifePosn]
        <OmegatListRaw[st,htEnd,AggState,EmpState,AggS,0,LifePosn],
        (*then they are worse off even investing at the optimal risky share *)Return[0]]
  ]; (* End If RiskyTimeCost > 0 *)
  Return[RiskySharetVal //N];
];

RiskySharetListRaw[st_,htEnd_,AggState_,EmpState_,AggS_] := RiskySharetListRaw[st,htEnd,AggState,EmpState,AggS,LifePosn];

ClearAll[RiskySharetList];
RiskySharetList[st_,htEnd_,AggState_,EmpState_,AggS_,LifePosn_] := Max[Min[RiskySharetListInterpFunc[[LifePosn]][st,htEnd,AggState,EmpState,AggS],1],0];
RiskySharetList[st_,htEnd_,AggState_,EmpState_,AggS_]           := RiskySharetListInterpFunc[[LifePosn]][st,htEnd,AggState,EmpState,AggS];



ClearAll[VxtList];
VxtList[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[xt < xmin         ,Return[Infinity]];  (* Extremely high marginal utility should make them avoid forbidden x's *)
  If[xt == xmin && etZeroProbVals[[AggState,EmpState]]>0,Return[Infinity]];  
  If[LifePosn>= LifeLength,Return[(ctList[xt,htStart,AggState,EmpState,AggK,LifePosn]^-rho)(htStart^(rho gamma - gamma))]];
  If[LifePosn <EarliestVtSolved,PeriodOfOptimalRule = EarliestVtSolved,PeriodOfOptimalRule = LifePosn]; 
  Return[
    Max[0,
      (VxInvtListInterpFunc[[PeriodOfOptimalRule]][xt,htStart,AggState,EmpState,AggK])^-rho
      ] (* End Max[] *)
  ];    (* End Return[] *)
]; (* End of Block *)


ClearAll[VxtListRaw];
VxtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[0]];
  If[xt <= xmin,
    (* then *)
    If[xt < xmin,Return[Infinity]];
    If[etZeroProbVals[[AggState,EmpState]]>0,Return[Infinity]];
    If[xmin == 0,Return[Infinity]];
  ];
  If[xt>xmax, 
      (* then use the interpolatingfunction, which is actually more accurate outside the grid *)
        Return[VxtList[xt,htStart,AggState,EmpState,AggK,LifePosn]]];
  ctVxt = ctList[xt,htStart,AggState,EmpState,AggK,LifePosn];
  stVxt = xt-ctVxt;
  If[LifePosn==LifeLength,Return[(ctVxt^-rho) htStart^(gamma rho - gamma)]];
  htEndVxt = htStart(1-catchup) + catchup ctVxt;
  If[ctVxt < 0,Return[AvoidLikePlague]];
  If[Chop[stVxt] <= 0 && LifePosn<LifeLength,
      If[etZeroProbVals[[AggState,EmpState]]>0,
        Print["Error: ctList>=xt in VxtRaw."];Print[{xt,htStart,AggState,EmpState,epVals[[LifePosn,AggState,EmpState,AggK]]}];Interrupt[];,
        Return[dudct[ctVxt,htStart]]]];
  AggS = AggSList[xt,htStart,AggState,EmpState,AggK,LifePosn];      
  VxtOut = beta[[LifePosn+1,EmpState]] OmegastList[stVxt,htEndVxt,AggState,EmpState,AggS,LifePosn];
  If[VxtOut<0,
    (* then *) 
      Print["Warning: VxtRaw[",xt,",",htStart,",",AggState,",",EmpState,",",AggK,",",LifePosn,"]=",VxtOut,"<0."];
  ]; (* End If VxtOut < 0 *)
Return[VxtOut]
]
VxtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_] := VxtRaw[xt,htStart,AggState,EmpState,AggK,LifePosn]



ClearAll[VxInvtListRaw];
VxInvtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[Infinity]];
  Return[
    VxtToVxInvtMultiply*(VxtListRaw[xt,htStart,AggState,EmpState,AggK,LifePosn])^(VxtToVxInvtPower)
  ];
];
VxInvtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_] := VxInvtListRaw[xt,htStart,AggState,EmpState,AggK,LifePosn];


ClearAll[VhtList];
VhtList[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[HabitsMatter == False,Return[0]];
  If[xt < xmin, Return[-AvoidLikePlague]];  (* Extremely high marginal utility should make them avoid forbidden x's *)
  If[xt == xmin && etZeroProbVals[[AggState,EmpState]]>0,Return[-Infinity]];
  If[LifePosn == LifeLength,
      (* then *) Return[dudht[xt,htStart]]];
  If[LifePosn < EarliestVtSolved,PeriodOfOptimalRule = EarliestVtSolved,PeriodOfOptimalRule = LifePosn]; 
  Return[
    Min[0,(VhInvtListInterpFunc[[LifePosn]][xt,htStart,AggState,EmpState,AggK]^(1/VhtToVhInvtPower))/VhtToVhInvtMultiply]
  ] (* End Return[] *)
]; (* End of Block *)


ClearAll[VhtListRaw];
VhtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[HabitsMatter == False,Return[0]];
  If[LifePosn>LifeLength  ,Return[0]];
  If[xt<xmin,Return[-AvoidLikePlague]];
  ctVht = ctList[xt,htStart,AggState,EmpState,AggK,LifePosn];
  If[Chop[ctVht] == 0,Return[-Infinity]];
  stVht = Chop[xt-ctVht];
  If[Chop[stVht] <= 0 && LifePosn<LifeLength,
      If[etZeroProbVals[[AggState,EmpState]]>0,
        Print["Error: ctList>=xt in VhtRaw."];Interrupt[]]];
  htEndVht = htStart(1-catchup) + catchup ctVht;
  If[LifePosn == LifeLength,Return[((ctVht htStart^-gamma)^-rho) (-gamma)(ctVht)htStart^(-gamma-1)]];
  If[ctVht < 0,Return[-AvoidLikePlague]];
  Print["Need to define AggS properly in VhtListRaw."];Interrupt[];
  Return[
    dudht[ctVht,htStart]+
    (1-catchup) beta[[LifePosn+1,EmpState]] OmegahtList[stVht,htEndVht,AggState,EmpState,AggS,LifePosn]
  ];
];

ClearAll[VhInvtListRaw];
VhInvtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[Length[hListGrid] == 1,Return[Infinity]];
  If[LifePosn>LifeLength,Return[Infinity]];
  Return[
    (VhtToVhInvtMultiply*VhtListRaw[xt,htStart,AggState,EmpState,AggK,LifePosn])^(VhtToVhInvtPower)
  ];
];
VhInvtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_] := VhInvtListRaw[xt,htStart,AggState,EmpState,AggK,LifePosn];

ClearAll[ctListRaw];
ctListRaw[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,Return[xt]];
  If[xt<xmin,Return[0.]];
  If[xt==xmin && etZeroProbVals[[AggState,EmpState]] >0,Return[0]];
  If[xt==xmin && etUnempProb>0,Return[UnempWage]];
  If[xt==xmin,Return[xmin]];
  CertEquivMultiplyFactor = (htStart^(gamma (1-1/rho)))*
                            (beta[[LifePosn+1,EmpState]])^(-1/rho);
  ctp1Val  = ctList[xt,htStart,AggState,EmpState,AggK,LifePosn+1];
  ctSearchMax = Max[Min[ctp1Val + (xmax-ctp1Val)*.01,xt],CertEquivList[0,1,1,1,AggK,LifePosn]];
  ctSearchMin = .9*ctp1Val;
  If[LifePosn == LifeLength-1,ctSearchMin = .49 xt;ctSearchMax = .51 xt];
  AggS = AggSList[xt,htStart,AggState,EmpState,AggK,LifePosn];
  FOCResults = 
    Check[FindRoot[
      c == CertEquivMultiplyFactor*CertEquivList[xt-c,htStart(1-catchup)+catchup c,AggState,EmpState,AggS,LifePosn]
    ,{c,{ctSearchMin,ctSearchMax}},MaxIterations->50],err,FindRoot::frsec];
  If[FOCResults == err,
      Print["Error in FOC.  Values:"
            ,{xt,htStart,AggState,EmpState,AggS,LifePosn}];
      Interrupt[]];
  If[Length[FOCResults]>2,Print["FOCResults of improper form; probably FindRoot failed."];Interrupt[]];
  ctVal = (c /. FOCResults);
  If[ctVal<0,ctVal=xt];
  If[ctVal>xt,ctVal=xt];  
  If[Not[NumberQ[ctVal]], (* then *) Print["FindViaFOC failed."];Print[##];Interrupt[]]; 
  st = xt-ctVal;If[st < 0,st=0;ctVal=xt;(*#Print["Error in ChoiceBasic: st < 0"];Print[##];Interrupt[]#*)];
  Return[ctVal];
]; (* End *)
ctListRaw[xt_,htStart_,AggState_,EmpState_,AggK_] := ctListRaw[xt,htStart,AggState,EmpState,AggK,LifePosn];
  
ctList[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := ctListInterpFunc[[LifePosn]][xt,htStart,AggState,EmpState,AggK];
ctList[xt_,htStart_,AggState_,EmpState_,AggK_          ] := ctListInterpFunc[[LifePosn]][xt,htStart,AggState,EmpState,AggK];


stList[x_,h_,AggState_,EmpState_,AggK_,LifePosn_] := x - ctList[x,h,AggState,EmpState,AggK,LifePosn];

htEndList[x_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := (1-catchup)htStart + catchup*ctList[x,htStart,AggState,EmpState,AggK,LifePosn];


OmegatList[st_,htEnd_,AggState_,EmpState_,AggS_,LifePosn_] := 
Block[{},
  If[LifePosn>=LifeLength,Return[0]];
  If[st<smin             ,Return[-Infinity]];
  PeriodOfOptimalRule=LifePosn;If[LifePosn<EarliestOmegatSolved,(* then *) PeriodOfOptimalRule = EarliestOmegatSolved];
  Return[
    Min[0,
      ((OmegaInvtListInterpFunc[[PeriodOfOptimalRule]][st,htEnd,AggState,EmpState])^
      (1/OmegatToOmegaInvtPower)
      )/OmegatToOmegaInvtMultiply
    ] (* End Min[] *)
      (* The raw data from OmegatRaw are raised to the power (1/(1-rho)) to make the function nearly linear before interpolating *)
      (* The exponentiation here undoes that to recover the function in levels *)
  ]  (* End Return[] *)
];


OmegatListRaw[st_,htEnd_,AggState_,EmpState_,AggS_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,(* then *) Return[0]];
  If[st == smin && etZeroProbVals[[AggState,EmpState]] > 0,Return[-Infinity]];
  OmegatRiskyShare = RiskySharetList[st,htEnd,AggState,EmpState,AggS,LifePosn];
  If[OmegatRiskyShare>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
(*
          AggKtp1 = AggkSSByAggState[[AggState]] + kAR1ByAggState[[AggState]]*(AggK-AggkSSByAggState[[AggState]]);
          AggKtp1 = AggKtp1/(GrowthByAggState[[LoopOverAggStates]]);
*)
             AggKtp1 = AggkSSByAggState[[AggState]] 
                     + kAR1ByAggState[[AggState]]*
                       (AggS*(1-Depreciation)/(GrowthByAggState[[LoopOverAggStates]])
                        -AggkSSByAggState[[AggState]]
                       );
          ROmegast = 
            (((R[[LoopOverAggStates,LoopOverRVals]]*
             OmegasInvtRiskyShare+Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1])*(1-OmegasInvtRiskyShare))/
            (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
            )));(*Print[ROmegast]*);
            ListOfArgs = 
             {ROmegast*st
               +wFunc[AggKtp1]*etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-OneIfRisky*RiskyTimeCost
             ,htEnd
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,AggKtp1
             ,LifePosn+1};
             (*Print["Stocks:",ListOfArgs];*)
            (G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]*
             epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]])^((1-gamma)(1-rho))
            *Apply[Vt,ListOfArgs]
          *etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
         *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
        *Rprob[[LoopOverAggStates,LoopOverRVals]]
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
       *EmpStateProb[LoopOverAggStates,EmpState,LoopOverEmpStates]
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
      *AggStateProb[AggState,LoopOverAggStates]
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]
];

Clear[OmegaInvtListRaw];
OmegaInvtListRaw[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,Return[Infinity]];
  Return[
    (OmegatToOmegaInvtMultiply*OmegatListRaw[st,htEnd,AggState,EmpState,AggK,LifePosn])^OmegatToOmegaInvtPower
  ];
];
OmegaInvtListRaw[st_,htEnd_,AggState_,EmpState_] := OmegaInvtListRaw[st,htEnd,AggState,EmpState,AggK,LifePosn] ;


ClearAll[VtList];
VtList[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[xt   < xmin         ,Return[-Infinity]];  (* Extremely high negative utility should make them avoid forbidden x's *)
  If[LifePosn>= LifeLength,Return[u[xt,htStart]]];
  If[LifePosn <EarliestVtSolved,PeriodOfOptimalRule = EarliestVtSolved,PeriodOfOptimalRule = LifePosn]; 
  Return[
    Min[0,
      (VInvtListInterpFunc[[PeriodOfOptimalRule]][xt,htStart,AggState,EmpState,AggK]^(1/VtToVInvtPower))/VtToVInvtMultiply
      ] (* End Max[] *)
  ];    (* End Return[] *)
]; (* End of Block *)


ClearAll[VtListRaw];
VtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[0]];
  If[xt <  xmin,Return[-Infinity]];
  If[xt == xmin && etZeroProbVals[[AggState,EmpState]]>0,Return[-Infinity]];
  If[xt>xmax, 
      (* then use the interpolatingfunction, which is actually more accurate outside the grid *)
        Return[VtList[xt,htStart,AggState,EmpState,AggK,LifePosn]]];
  xtLeft = xt;
  ctVt = ctList[xtLeft,htStart,AggState,EmpState,AggK,LifePosn];
  stVt = xtLeft-ctVt;
  If[LifePosn==LifeLength,Return[u[xtLeft,htStart]]];
  htEndVt = htStart(1-catchup) + catchup ctVt;
  If[ctVt < 0,Return[AvoidLikePlague]];
  If[Chop[stVt] <= 0 && LifePosn<LifeLength,
      If[etZeroProbVals[[AggState,EmpState]]>0,
        Print["Error: ctList>=xtLeft in VtListRaw."];Print[{xtLeft,htStart,AggState,EmpState,epVals[[LifePosn,AggState,EmpState,AggK]]}];Interrupt[];,
        Return[u[ctVt,htStart]]]];
  VtOut = u[ctVt,htStart]+beta[[LifePosn+1,EmpState]] OmegatList[stVt,htEndVt,AggState,EmpState,AggK,LifePosn];
  If[VtOut>0,
    (* then *) 
      Print["Warning: VtListRaw[",xtLeft,",",htStart,",",AggState,",",EmpState,",",AggK,",",LifePosn,"]=",VtOut,">0."];
  ]; (* End If VtOut > 0 *)
Return[VtOut]
]
VtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_] := VtListRaw[xt,htStart,AggState,EmpState,AggK,LifePosn]


ClearAll[VInvtListRaw];
VInvtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[Infinity]];
  Return[
    (VtToVInvtMultiply*VtListRaw[xt,htStart,AggState,EmpState,AggK,LifePosn])^(VtToVInvtPower)
  ];
];
VInvtListRaw[xt_,htStart_,AggState_,EmpState_,AggK_] := VInvtListRaw[xt,htStart,AggState,EmpState,AggK,LifePosn];

MPCList[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
  (ctList[xt+.001,htStart,AggState,EmpState,AggK,LifePosn]-
   ctList[xt-.001,htStart,AggState,EmpState,AggK,LifePosn])/.002;

MPCList[xt_,htStart_,AggState_,EmpState_,AggK_] := MPCList[xt,htStart,AggState,EmpState,AggK,LifePosn];


ClearAll[EtMPCPermList];
EtMPCPermList[st_,htEnd_,AggState_,EmpState_,AggK_,LifePosn_] := EtMPCPermListInterpFunc[[LifePosn]][st,htEnd,AggState,EmpState,AggK];
  
EtMPCPermList[st_,htEnd_,AggState_,EmpState_,AggK_] := EtMPCPermList[st,htEnd,AggState,EmpState,AggK,LifePosn];

{xtp1Pos,htp1StartPos,AggStatetp1Pos,EmpStatetp1Pos,AggKtp1Pos,GNFactortp1Pos} = {1,2,3,4,5,6};
Vtp1ArgListDistGivenOmegatAllArgFunc[st_,htEnd_,AggState_,EmpState_,AggS_,RiskySharet_] := 
Block[{(*VtArgListDistArray,WeightedR,xtCalc,ProbOfArgs,ListOfArgs,ProbOfArgs*)},
  {ArgList,ProbList} = {{},{}};
    Do[            (* over possible future aggregate states               *)
     Do[           (* over possible future employment states              *)
      Do[          (* over possible rates of return                       *)
       Do[         (* over possible future values of the permanent shock  *)
       AggKtp1 = AggS*(1-Depreciation)/(GrowthByAggState[[LoopOverAggStates]]);
       GNFactor = epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]*
                  G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]];
       htp1Start = htEnd/GNFactor;
       WeightedR = 
         ((R[[LoopOverAggStates,LoopOverRVals]]        *RiskySharet
          +Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1])*(1-RiskySharet)
          )/GNFactor
         );
        Do[        (* over possible future values of the transitory shock *) 
            xtCalc = WeightedR*st+wFunc[AggKtp1]*etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]];
            ListOfArgs = 
             {xtCalc
             ,htp1Start
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,AggKtp1
             ,GNFactor
             };
            ProbOfArgs = 
              etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
             *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
             *Rprob[[LoopOverAggStates,LoopOverRVals]]
             *EmpStateProb[LoopOverAggStates,EmpState,LoopOverEmpStates]
             *AggStateProb[AggState,LoopOverAggStates];
            If[ProbOfArgs<0,Print[ListOfArgs];Print[ProbOfArgs]];
            If[ProbOfArgs>0,
              (* then *)
                ArgList  = Append[ArgList ,ListOfArgs];
                ProbList = Append[ProbList,ProbOfArgs];
            ];
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over etVals     *)
       ,{LoopOverepVals, Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                       (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                                    (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}];                                                    (* End loop over AggStates  *)
  TrimmedArgList = Trim[ArgList,TrimmingIncrement];
  ArgProbList = Sort[Transpose[{TrimmedArgList,ProbList}]];
  ArgProbListCombined = CombineProbsForIdenticalArgs[ArgProbList];
  SmallestChoppablex = ArgProbListCombined[[1+Round[Length[ArgProbListCombined]/10],1,1]];
  Return[Transpose[ChopElemsWithLowProb[ArgProbListCombined,CutoffProb,SmallestChoppablex]]];
];

{xtp1Pos,htp1StartPos,AggStatetp1Pos,EmpStatetp1Pos,AggKtp1Pos,GNFactortp1Pos,RScaledtp1Pos} = {1,2,3,4,5,6,7};
RScaledTimesVxtArgListDistGivenOmegatAllArgFunc[st_,htEnd_,AggState_,EmpState_,AggS_,RiskySharet_] := 
Block[{(*VtArgGridDistArray,WeightedR,xtCalc,ProbOfArgs,ListOfArgs,ProbOfArgs*)},
  {ArgList,ProbList} = {{},{}};
    Do[            (* over possible rates of return                       *)
     Do[           (* over possible future employment states              *)
      Do[          (* over possible future aggregate states               *)
       Do[         (* over possible future values of the permanent shock  *)
(*       AggKtp1 = AggkSSByAggState[[AggState]] + kAR1ByAggState[[AggState]]*(AggK-AggkSSByAggState[[AggState]]);
         AggKtp1 = AggKtp1/(GrowthByAggState[[LoopOverAggStates]]);
*)       
       AggKtp1 = AggS*(1-Depreciation)/(GrowthByAggState[[LoopOverAggStates]] );
       stVal=st;
       GNFactor = epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]*
                  (G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]);
       htp1Start = htEnd/GNFactor;
       RScaled = 
         ((R[[LoopOverAggStates,LoopOverRVals]]         *RiskySharet
          +Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1]) *(1-RiskySharet)
          )/GNFactor
         );(*Print[WeightedR]*);
        Do[        (* over possible future values of the transitory shock *) 
            xtCalc = RScaled*stVal+wFunc[AggKtp1]*etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]];
            ListOfArgs = 
             {xtCalc
             ,htp1Start
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,AggKtp1
             ,GNFactor
             ,RScaled};
            ProbOfArgs = 
              etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
             *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
             *Rprob[[LoopOverAggStates,LoopOverRVals]]
             *EmpStateProb[LoopOverAggStates,EmpState,LoopOverEmpStates]
             *AggStateProb[AggState,LoopOverAggStates];
            If[ProbOfArgs<0,Print[ListOfArgs];Print[ProbOfArgs]];
            If[ProbOfArgs>0,
              (* then *)
                ArgList  = Append[ArgList, ListOfArgs];
                ProbList = Append[ProbList,ProbOfArgs];
              ];
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}];                                         (* End loop over AggStates  *)
  TrimmedArgList = Trim[ArgList,TrimmingIncrement];
  ArgProbList = Sort[Transpose[{TrimmedArgList,ProbList}]];
  ArgProbListCombined = CombineProbsForIdenticalArgs[ArgProbList];
  SmallestChoppablex = ArgProbListCombined[[1+Round[Length[ArgProbListCombined]/10],1,1]];
  Return[Transpose[ChopElemsWithLowProb[ArgProbListCombined,CutoffProb,SmallestChoppablex]]];
];

{xtp1Pos,htp1StartPos,AggStatetp1Pos,EmpStatetp1Pos,AggKtp1Pos,GNFactortp1Pos,RGaptp1Pos} = {1,2,3,4,5,6,7};
RGapTimesVxtArgListDistGivenOmegatAllArgFunc[st_,htEnd_,AggState_,EmpState_,AggS_,RiskySharet_] := 
Block[{(*VtArgGridDistArray,WeightedR,xtCalc,ProbOfArgs,ListOfArgs,ProbOfArgs*)},
  {ArgList,ProbList} = {{},{}};
    Do[            (* over possible rates of return                       *)
     Do[           (* over possible future employment states              *)
      Do[          (* over possible future aggregate states               *)
       Do[         (* over possible future values of the permanent shock  *)
(*       AggKtp1 = AggkSSByAggState[[AggState]] + kAR1ByAggState[[AggState]]*(AggK-AggkSSByAggState[[AggState]]);
         AggKtp1 = AggKtp1/(GrowthByAggState[[LoopOverAggStates]]);
*)       
       AggKtp1 = AggS*(1-Depreciation)/(GrowthByAggState[[LoopOverAggStates]]);
       stVal = st;
       GNFactor = epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]*
                  G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]];
       htp1Start = htEnd/GNFactor;
       WeightedR = 
         ((R[[LoopOverAggStates,LoopOverRVals]]*RiskySharet
          +Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1])*(1-RiskySharet)
          )/GNFactor
         );(*Print[WeightedR]*);
       RGap =(R[[LoopOverAggStates,LoopOverRVals]]-Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1]))/GNFactor;
        Do[        (* over possible future values of the transitory shock *) 
            xtCalc = WeightedR*stVal+wFunc[AggKtp1]*etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]];
            ListOfArgs = 
             {xtCalc
             ,htp1Start
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,AggKtp1
             ,RGap};
            ProbOfArgs = 
              etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
             *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
             *Rprob[[LoopOverAggStates,LoopOverRVals]]
             *EmpStateProb[LoopOverAggStates,EmpState,LoopOverEmpStates]
             *AggStateProb[AggState,LoopOverAggStates];
            If[ProbOfArgs<0,Print[ListOfArgs];Print[ProbOfArgs]];
            If[ProbOfArgs>0,
              (* then *)
                ArgList  = Append[ArgList ,ListOfArgs];
                ProbList = Append[ProbList,ProbOfArgs];
              ];
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                               (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}];                                         (* End loop over AggStates  *)
  TrimmedArgList = Trim[ArgList,TrimmingIncrement];
  ArgProbList = Sort[Transpose[{TrimmedArgList,ProbList}]];
  ArgProbListCombined = CombineProbsForIdenticalArgs[ArgProbList];
  SmallestChoppablex = ArgProbListCombined[[1+Round[Length[ArgProbListCombined]/10],1,1]];
  Return[Transpose[ChopElemsWithLowProb[ArgProbListCombined,CutoffProb,SmallestChoppablex]]];
];

RGapTimesVxtList[xt_,htStart_,AggState_,EmpState_,AggS_,RGap_,LifePosn_] := 
  RGap*VxtList[xt,htStart,AggState,EmpState,AggS,LifePosn];

RGapTimesVxtList[xt_,htStart_,AggState_,EmpState_,AggS_,RGap_] := 
  RGap*VxtList[xt,htStart,AggState,EmpState,AggS,LifePosn];

RScaledTimesVxtList[xt_,htStart_,AggState_,EmpState_,AggS_,RScaled_,LifePosn_] := 
  RScaled*VxtList[xt,htStart,AggState,EmpState,AggS,LifePosn];

RScaledTimesVxtList[xt_,htStart_,AggState_,EmpState_,AggS_,RScaled_] := 
  RScaled*VxtList[xt,htStart,AggState,EmpState,AggS,LifePosn];

RScaledTimesVxtp1[xt_,htStart_,AggState_,EmpState_,AggS_,RScaled_,LifePosn_] := 
  RScaled*VxtList[xt,htStart,AggState,EmpState,AggS,LifePosn+1];

RScaledTimesVxtp1[xt_,htStart_,AggState_,EmpState_,AggS_,RScaled_] := 
  RScaled*VxtList[xt,htStart,AggState,EmpState,AggS,LifePosn+1];

Trim[ArgList_,Increment_] := (Round[ArgList/Increment] *Increment ) //N;

CombineProbsForIdenticalArgs[ArgProbList_] := Block[{},
	NewArgList = {ArgProbList[[1,1]]};
	NewProbList= {ArgProbList[[1,2]]};
	Do[
		If[ArgProbList[[i,1]] == NewArgList[[-1]],
			  (* then it's the same as the previous item, so add prob *)
			NewProbList[[-1]] += ArgProbList[[i,2]],
			  (* else it's different from the previous item, so add *)
			NewArgList = Append[NewArgList,ArgProbList[[i,1]]];
			NewProbList = Append[NewProbList,ArgProbList[[i,2]]];
			];
		,{i,2,Length[ArgProbList]}];
	Return[Transpose[{NewArgList,NewProbList}]]]

		
ClearAll[ChopElemsWithLowProb];
ChopElemsWithLowProb[ArgProbList_,CutoffProb_,SmallestChoppablex_] := Block[{},
	NewArgProbList = {};
	Do[
		If[ArgProbList[[i,-1]] > CutoffProb || ArgProbList[[i,1,1]]<SmallestChoppablex,
			NewArgProbList = Append[NewArgProbList,ArgProbList[[i]]]]
		,{i,Length[ArgProbList]}];
	RemainingArgs = Transpose[NewArgProbList][[1]];
	ProbOfRemainingElements = Transpose[NewArgProbList][[2]];
	CumProbOfRemainingElements = CumulativeSums[ProbOfRemainingElements][[-1]];
	ScaledProbOfRemainingElements = 
     ProbOfRemainingElements/CumProbOfRemainingElements;
	Return[Transpose[{RemainingArgs,ScaledProbOfRemainingElements}]]]
			
			


ClearAll[OmegatArgGridDistGivenVtArgList];
OmegatArgGridDistGivenVtArgList[xt_,htStart_,AggState_,EmpState_,AggK_] := 
Block[{(*VtArgGridDistArray,WeightedR,xtCalc,ProbOfArgs,ListOfArgs,ProbOfArgs*)},
  OmegatArgGridDistArray = OmegatArgArrayEmpty;
        ctValOmegatArg = ctList[xt,htStart,AggState,EmpState,AggK];
        stValOmegatArg = xt - ctValOmegatArg;
        htValOmegatArg = htStart(1-catchup) + catchup ctValOmegatArg;
        OmegatArgGridDistArray[[
          ClosestElementTo[s0MatGrid,stValOmegatArg],
          ClosestElementTo[hMatGrid,htValOmegatArg],
          AggState,
          EmpState,
          AggK]] = 1;
  Return[OmegatArgGridDistArray];
];


ClearAll[EtLogctp1ListGtp1Givenst];
EtLogctp1ListGtp1Givenst[st_,htStart_,AggState_,EmpState_] := 
  EtLogctp1ListGtp1GivenstInterpFunc[[LifePosn]][st,htStart,AggState,EmpState];

ClearAll[EtLogctp1ListGtp1SquaredGivenst];
EtLogctp1ListGtp1SquaredGivenst[st_,htStart_,AggState_,EmpState_] := 
  EtLogctp1ListGtp1SquaredGivenstInterpFunc[[LifePosn]][st,htStart,AggState,EmpState];

ClearAll[EtLogctp1ListGtp1Oct];
EtLogctp1ListGtp1Oct[xt_,htStart_,AggState_,EmpState_,AggK_] := 
Block[{},
  ctVal = ctList[xt,htStart,AggState ,EmpState,AggK];
  stVal = xt-ctVal;
  htEndVal = (1-catchup)htStart + catchup ctVal;
  Return[EtLogctp1ListGtp1Givenst[stVal,htEndVal,AggState,EmpState]-Log[ctVal]]
];

ClearAll[EtLogctp1ListGtp1OctSquared];
EtLogctp1ListGtp1OctSquared[xt_,htStart_,AggState_,EmpState_,AggK_] := 
Block[{},
  ctVal = ctList[xt,htStart,AggState ,EmpState,AggK];
  stVal = xt-ctVal;
  htEndVal = (1-catchup)htStart + catchup ctVal;
  Logctp1Gtp1        = EtLogctp1ListGtp1Givenst[stVal,htEndVal,AggState,EmpState];
  Logctp1Gtp1Squared = EtLogctp1ListGtp1SquaredGivenst[stVal,htEndVal,AggState,EmpState];  
  Return[Logctp1Gtp1Squared - 2 Logctp1Gtp1 Log[ctVal] + (Log[ctVal])^2]
];


ClearAll[EtEulerListApprox];
EtEulerListApprox[xt_,htStart_,AggState_,EmpState_,AggK_] := Block[{},
(*       AggKtp1 = AggkSSByAggState[[AggState]] + kAR1ByAggState[[AggState]]*(AggK-AggkSSByAggState[[AggState]]);
         AggKtp1 = AggKtp1/(GrowthByAggState[[LoopOverAggStates]]);
*)       
       AggKtp1 = AggS*(1-Depreciation)/(GrowthByAggState[[LoopOverAggStates]]);
  Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1]) DiscountFactor[[EmpState]]*
  (1 - rho       EtLogctp1ListGtp1Oct[xt,htStart,AggState,EmpState,AggK] 
     +((rho^2)/2)EtLogctp1ListGtp1OctSquared[xt,htStart,AggState,EmpState,AggK])
];


ClearAll[EtEulerList];
EtEulerList[xt_,htStart_,AggState_,EmpState_,AggK_] := 
Block[{},
  If[LifePosn>=LifeLength,(* then *) Return[0]];
  If[xt == xmin && etZeroProbVals[[AggState,EmpState]] > 0,Return[Infinity]];
  ctVal = ctList[xt,ht,AggState,EmpState,AggK];
  stVal = xt-ctVal;
  htEndVal = htStart(1-catchup) + catchup ctVal;
  RiskyShareVal = RiskySharetList[st,htEndVal,AggState,EmpState,AggK,LifePosn];
  If[RiskyShareVal>0 && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
(*       AggKtp1 = AggkSSByAggState[[AggState]] + kAR1ByAggState[[AggState]]*(AggK-AggkSSByAggState[[AggState]]);
         AggKtp1 = AggKtp1/(GrowthByAggState[[LoopOverAggStates]]);
*)       
          AggKtp1 = AggS*(1-Depreciation)/(GrowthByAggState[[LoopOverAggStates]]);
          Gctp1 = G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]] epVals[[LifePosn,AggState,EmpState,AggK]];
          Rctp1 = 
            (((R[[LoopOverAggStates,LoopOverRVals]]*
             RiskyShareVal+Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1])*(1-RiskyShareVal))/
            (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
            )));(*Print[Rctp1]*);
            ListOfArgs = 
             {Rctp1*stVal
               +wFunc[AggKtp1]*etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-RiskyTimeCost*OneIfRisky
             ,htEndVal
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,AggKtp1
             ,LifePosn+1};
            ProbOfArgs = etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
                        *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
                        *Rprob[[LoopOverAggStates,LoopOverRVals]]
                        *EmpStateProb[LoopOverAggStates,EmpState,LoopOverEmpStates]
                        *AggStateProb[AggState,LoopOverAggStates];
(*            Print[{ListOfArgs,ProbOfArgs,Rctp1*Apply[ctList,ListOfArgs]}];*)
          (DiscountFactor*Rctp1*(Gctp1^(1-rho))*(Apply[ctList,ListOfArgs]/ctVal)^-rho)*ProbOfArgs
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                      (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]; (* End Return[] *)
];


ClearAll[Etctp1List];
Etctp1List[xt_,htStart_,AggState_,EmpState_,AggK_] := 
Block[{},
  If[LifePosn>=LifeLength,(* then *) Return[0]];
  ctVal = ctList[xt,htStart,AggState,EmpState,AggK];
  AggS = AggSList[xt,htStart,AggState,EmpState,AggK];
  stVal = xt-ctVal;
  htEndVal = htStart(1-catchup) + catchup ctVal;
  RiskyShareVal = RiskySharetList[st,htEndVal,AggState,EmpState,AggK,LifePosn];
  If[RiskyShareVal>0 && RiskyTimeCost>0,(*then*) OneIfRisky=1,(*else*) OneIfRisky=0];
(*       AggKtp1 = AggkSSByAggState[[AggState]] + kAR1ByAggState[[AggState]]*(AggK-AggkSSByAggState[[AggState]]);
         AggKtp1 = AggKtp1/(GrowthByAggState[[LoopOverAggStates]]);
*)       
       AggKtp1 = AggS*(1-Depreciation)/(GrowthByAggState[[LoopOverAggStates]]);
  Return[
    Sum[            (* over possible rates of return                       *)
     Sum[           (* over possible future employment states              *)
      Sum[          (* over possible future aggregate states               *)
       Sum[         (* over possible future values of the permanent shock  *)
        Sum[        (* over possible future values of the transitory shock *) 
          Gctp1 = G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]] epVals[[LifePosn,AggState,EmpState,AggK]];
          Rctp1 = 
            (((R[[LoopOverAggStates,LoopOverRVals]]*
             RiskyShareVal+Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1])*(1-RiskyShareVal))/
            (epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]] G[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]
            )));(*Print[Rctp1]*);
            ListOfArgs = 
             {Rctp1*stVal
               +wFunc[AggKtp1]*etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]-RiskyTimeCost*OneIfRisky
             ,htEndVal
             ,LoopOverAggStates
             ,LoopOverEmpStates
             ,AggKtp1
             ,LifePosn+1};
            ProbOfArgs = etProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOveretVals]]
                        *epProb[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates,LoopOverepVals]]
                        *Rprob[[LoopOverAggStates,LoopOverRVals]]
                        *EmpStateProb[LoopOverAggStates,EmpState,LoopOverEmpStates]
                        *AggStateProb[AggState,LoopOverAggStates];
(*            Print[{ListOfArgs,ProbOfArgs,Rctp1*Apply[ctList,ListOfArgs]}];*)
          Apply[ctList,ListOfArgs]*ProbOfArgs
        ,{LoopOveretVals,Length[etVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}] (* End loop over etVals     *)
       ,{LoopOverepVals,Length[epVals[[LifePosn+1,LoopOverAggStates,LoopOverEmpStates]]]}]  (* End loop over epVals     *)
      ,{LoopOverRVals,Length[R[[LoopOverAggStates]]]}]                                      (* End loop over RStates *)
     ,{LoopOverEmpStates,NumOfEmpStates}]                                        (* End loop over EmpStates  *)
    ,{LoopOverAggStates,NumOfAggStates}]                                         (* End loop over AggStates  *)
  ]; (* End Return[] *)
];

FindxTarget := Block[{},
(*       AggKtp1 = AggkSSByAggState[[AggState]] + kAR1ByAggState[[AggState]]*(AggK-AggkSSByAggState[[AggState]]);
         AggKtp1 = AggKtp1/(GrowthByAggState[[LoopOverAggStates]]);
*)       
       AggKtp1 = AggS*(1-Depreciation)/(GrowthByAggState[[LoopOverAggStates]]);
  xTargetResults = 
    FindRoot[ 1 + (Rcertain*(1-Depreciation)*(1+rFunc[AggKtp1])-1)(xSearch - 1) == ctList[xSearch,1,1,1,1,LifePosn]
    ,{xSearch,{1,2}}];
  Return[(xSearch /. xTargetResults)]
];

ClearAll[MPHList];
MPHList[xt_,htStart_,AggState_,EmpState_,AggK_,LifePosn_] := 
  (
   ctList[xt,htStart+.001,AggState,EmpState,AggK,LifePosn]
   -ctList[xt,htStart-.001,AggState,EmpState,AggK,LifePosn]
  )/.002;
  
MPHList[xt_,htStart_,AggState_,EmpState_,AggK_] := MPHList[xt,htStart,AggState,EmpState,AggK,LifePosn];

ExtraDefsFileName = "func_defs_list_"<>ProblemToSolveString<>".m";

If[Length[FileNames[ExtraDefsFileName]]>0,Get[ExtraDefsFileName]];
