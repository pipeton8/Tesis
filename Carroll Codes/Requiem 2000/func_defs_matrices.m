(*

Define the functions necessary for solving the problem using the matrices method

*)

OmegastMat[st_,htEnd_,AggState_,EmpState_,LifePosn_] := 
Block[{},
  If[LifePosn>=LifeLength,Return[0]];
  If[st<smin             ,Return[AvoidLikePlague]];
    PeriodOfOptimalRule=LifePosn;If[LifePosn<EarliestEtSolved,(* then *) PeriodOfOptimalRule = EarliestOmegatSolved];
    Return[
      Max[0,
      ((OmegasInvtMatInterpFunc[[PeriodOfOptimalRule]][st,htEnd,AggState,EmpState])^
      (1/OmegastToOmegasInvtPower)
      )/OmegastToOmegasInvtMultiply] (* End Max[] *)
          (* The raw data from OmegastRaw are raised to the power (1/-rho) to make the function nearly linear before interpolating *)
          (* The exponentiation here undoes that to recover the function in levels *)
    ]  (* End Return[] *)
];


OmegahtMat[st_,htEnd_,AggState_,EmpState_,LifePosn_] := 
Block[{},
  If[Length[hMatGrid] == 1,Return[0]];
  If[LifePosn>=LifeLength,Return[0]];
  If[st<smin             ,Return[AvoidLikePlague]];
    PeriodOfOptimalRule=LifePosn;If[LifePosn<EarliestEtSolved,(* then *) PeriodOfOptimalRule = EarliestOmegatSolved];
    Return[
      Min[0,
      ((OmegahInvtMatInterpFunc[[PeriodOfOptimalRule]][st,htEnd,AggState,EmpState])^
      (1/OmegahtToOmegahInvtPower)
      )/OmegahtToOmegahInvtMultiply] (* End Max[] *)
          (* The raw data from OmegahtRaw are raised to the power (1/-rho) to make the function nearly linear before interpolating *)
          (* The exponentiation here undoes that to recover the function in levels *)
    ]  (* End Return[] *)
];


CertEquivMatRaw[st_,htEnd_,AggState_,EmpState_,LifePosn_] := Block[{},
  If[st < 0,Return[0]];
  If[st == smin && etZeroProbVals[[AggState,EmpState]] > 0,Return[0]];
  Return[
    (OmegastMat[st,htEnd,AggState,EmpState,LifePosn] - catchup OmegahtMat[st,htEnd,AggState,EmpState,LifePosn])^(-1/rho)
  ];
];
CertEquivMatRaw[st_,htEnd_,AggState_,EmpState_] := CertEquivMatRaw[st,htEnd,AggState,EmpState,LifePosn];

ClearAll[CertEquivMat];
CertEquivMat[st_,htEnd_,AggState_,EmpState_,LifePosn_] := CertEquivMatInterpFunc[[LifePosn]][st,htEnd,AggState,EmpState];


FOCwrtRiskySharetMat[s_, h_, AggState_, EmpState_, RiskyShare_, LifePosn_] := 
  FOCwrtRiskySharetMatInterpFunc[[
        Max[Min[EarliestVtSolved, EarliestOmegatSolved], LifePosn]
                             ]][s, h,AggState, EmpState, RiskyShare];

FOCwrtRiskySharetMat[s_, h_, AggState_, EmpState_, RiskyShare_] := FOCwrtRiskySharetMat[s, h, AggState, EmpState, RiskyShare, LifePosn];


ClearAll[RiskySharetMatRaw];
RiskySharetMatRaw[st_,htEnd_,AggState_,EmpState_,LifePosn_] := Block[{},
  If[SolveForRiskyShare == False,Return[0]];
  If[st < smin,Print["Error: RiskySharetMat called with st<smin."];Interrupt[]];
  If[st == smin,
      Return[RiskySharetMatRaw[s0MatGrid[[2]],htEnd,AggState,EmpState,LifePosn]]];
  If[(  FOCwrtRiskySharetMat[st,htEnd,AggState,EmpState,0,LifePosn] > 0
     && FOCwrtRiskySharetMat[st,htEnd,AggState,EmpState,1,LifePosn] > 0),
     (* then *)
     If[RiskyTimeCost == 0, Return[1]];
     If[OmegatRaw[st,htEnd,AggState,EmpState,1,LifePosn]
       >OmegatRaw[st,htEnd,AggState,EmpState,0,LifePosn],
       (*then*)Return[1],
       (*else*)Return[0]
       ](* End If EtVxtp1Raw *);
  ];    (* End if FOC *)
  If[(  FOCwrtRiskySharetMat[st,htEnd,AggState,EmpState,0,LifePosn] < 0
     && FOCwrtRiskySharetMat[st,htEnd,AggState,EmpState,1,LifePosn] < 0),
     Return[0]];
  RiskySharetp1 = RiskySharetMat[st,htEnd,AggState,EmpState,LifePosn+1];
  If[(RiskySharetp1==0) || (RiskySharetp1==1),
    (* then *) RiskyShareSearchMin = 0;RiskyShareSearchMax=1,
    (* else *) RiskyShareSearchMin = RiskySharetp1*.95;RiskyShareSearchMax=RiskySharetp1+.05*(1-RiskySharetp1)];
  RiskyShareResults=
    Check[FindRoot[
      (FOCwrtRiskySharetMat[st,htEnd,AggState,EmpState,RiskyShare,LifePosn]
      /VxtMat[st+UnempWage,htEnd,AggState,EmpState,1,LifePosn+1])==0
    ,{RiskyShare,{RiskyShareSearchMin,RiskyShareSearchMax}},MaxIterations->50],err,FindRoot::frsec];
  If[RiskyShareResults == err,
    RiskyShareResults=
      Print["Error in RiskySharetMatRaw.  Values:"
            ,{st,htEnd,AggState,EmpState,LifePosn}];
      Print["Now trying to use FOCwrtRiskySharetMatRaw."];
      Check[FindRoot[
        (FOCwrtRiskySharetMatRaw[st,htEnd,AggState,EmpState,RiskyShare,LifePosn]
        /VxtMat[st+UnempWage,htEnd,AggState,EmpState,1,LifePosn+1])==0
      ,{RiskyShare,{RiskyShareSearchMin,RiskyShareSearchMax}},MaxIterations->50],err,FindRoot::frsec];
    Interrupt[];
    If[RiskyShareResults == err,    
      Print["Error in RiskySharetMatRaw even using FOCwrtRiskySharetMatRaw.  Values:"
            ,{st,htEnd,AggState,EmpState,LifePosn}];
      Interrupt[]];
  ];
  If[Length[RiskyShareResults]>2,Print["RiskyShareResults of improper form; probably FindRoot failed"];Interrupt[]];
  RiskySharetMatVal = Round[(RiskyShare /. RiskyShareResults)*1000]/1000;
  If[RiskySharetMatVal>1, (* then *) RiskySharetMatVal=1];
  If[RiskySharetMatVal<0, (* then *) RiskySharetMatVal=0];
  If[RiskyTimeCost>0,
    (* then *)
      If[OmegatRaw[st,htEnd,AggState,EmpState,RiskySharetMatVal,LifePosn]
        <OmegatRaw[st,htEnd,AggState,EmpState,0,LifePosn],(*then*)Return[0]]
  ]; (* End If RiskyTimeCost > 0 *)
  Return[RiskySharetMatVal //N];
];
RiskySharetMatRaw[st_,htEnd_,AggState_,EmpState_] := RiskySharetMatRaw[st,htEnd,AggState,EmpState,LifePosn]

ClearAll[RiskySharetMat];
RiskySharetMat[st_,htEnd_,AggState_,EmpState_,LifePosn_] := Max[Min[RiskySharetMatInterpFunc[[LifePosn]][st,htEnd,AggState,EmpState],1],0];
RiskySharetMat[st_,htEnd_,AggState_,EmpState_]           := RiskySharetMatInterpFunc[[LifePosn]][st,htEnd,AggState,EmpState];


ClearAll[VxtMatRaw];
VxtMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[0]];
  If[xt < xmin,Return[Infinity]];
  If[xt>xmax, 
      (* then use the interpolatingfunction, which is actually more accurate outside the grid *)
        Return[VxtMat[xt,htStart,AggState,EmpState,AggKt,LifePosn]]];
  ctVxtMat = ctMat[xt,htStart,AggState,EmpState,AggKt,LifePosn];
  stVxtMat = xt-ctVxtMat;
  GNFactorVxtMat = (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(1-rho);
  If[LifePosn==LifeLength,Return[GNFactorVxtMat*(ctVxtMat^-rho) htStart^(gamma rho - gamma)]];
  htEndVxtMat = htStart(1-catchup)/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]) + catchup ctVxtMat;
  If[ctVxtMat < 0,Return[AvoidLikePlague]];
  If[Chop[stVxtMat] <= 0 && LifePosn<LifeLength,
      If[etZeroProbVals[[AggState,EmpState]]>0,
        Print["Error: ctMat>=xt in VxtMatRaw."];Print[{xt,htStart,AggState,EmpState,AggKt}];Interrupt[];,
        Return[GNFactorVxtMat*dudct[ctVxtMat,htStart]]]];
  VxtMatOut = GNFactorVxtMat*
          ((G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(gamma rho - gamma))*
          beta[[LifePosn+1,EmpState]] OmegastMat[stVxtMat,htEndVxtMat,AggState,EmpState,LifePosn];
  If[VxtMatOut<0,
    (* then *) 
      Print["Warning: VxtMatRaw[",xt,",",htStart,",",AggState,",",EmpState,",",AggKt,",",LifePosn,"]=",VxtMatOut,"<0."];
  ]; (* End If VxtMatOut < 0 *)
Return[VxtMatOut]
]
VxtMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := VxtMatRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn]


ClearAll[VxInvtMatRaw];
VxInvtMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[Infinity]];
  Return[
    VxtToVxInvtMultiply*(VxtMatRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn])^(VxtToVxInvtPower)
  ];
];
VxInvtMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := VxInvtMatRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn];


ClearAll[VxtMat];
VxtMat[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[xt   < xmin         ,Return[AvoidLikePlague]];  (* Extremely high marginal utility should make them avoid forbidden x's *)
  If[LifePosn>= LifeLength,
    (* then *)
    If[xt == xmin && etZeroProbVals[[AggState,EmpState]] >0,Return[Infinity]];
    Return[(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]] htStart^-gamma)^(1-rho)*xt^(-rho)
    ]
  ];
  If[LifePosn <EarliestVtSolved,PeriodOfOptimalRule = EarliestVtSolved,PeriodOfOptimalRule = LifePosn]; 
  Return[
    Max[0,
      (VxInvtMatInterpFunc[[PeriodOfOptimalRule]][xt,htStart,AggState,EmpState,AggKt])^-rho
      ] (* End Max[] *)
  ];    (* End Return[] *)
]; (* End of Block *)


ClearAll[VhtMatRaw];
VhtMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[0]];
  If[xt < xmin,Return[-Infinity]];
  If[xt>xmax, 
      (* then use the interpolatingfunction, which is actually more accurate outside the grid *)
        Return[VhtMat[xt,htStart,AggState,EmpState,AggKt,LifePosn]]];
  ctVhtMat = ctMat[xt,htStart,AggState,EmpState,AggKt,LifePosn];
  stVhtMat = xt-ctVhtMat;
  GNFactorVhtMat = (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(1-rho);
  If[LifePosn==LifeLength,Return[(GNFactorVhtMat*(ctVhtMat htStart^-gamma)^(1-rho))/(1-rho)]];
  htEndVhtMat = htStart(1-catchup)/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]) + catchup ctVhtMat;
  If[ctVhtMat < 0,Return[-AvoidLikePlague]];
  If[Chop[stVhtMat] <= 0 && LifePosn<LifeLength,
      If[etZeroProbVals[[AggState,EmpState]]>0,
        Print["Error: ctMat>=xt in VhtMatRaw."];Print[{xt,htStart,AggState,EmpState,AggKt}];Interrupt[];
        ]];
  VhtMatOut =
    GNFactorVhtMat*
    (dudht[ctVhtMat,htStart] +
    (1-catchup) (G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(rho gamma - gamma - 1)*
    beta[[LifePosn+1,EmpState]] OmegahtMat[stVhtMat,htEndVhtMat,AggState,EmpState,LifePosn]
    );

  If[VhtMatOut>0,
    (* then *) 
      Print["Warning: VhtMatRaw[",xt,",",htStart,",",AggState,",",EmpState,",",AggKt,",",LifePosn,"]=",VhtMatOut,">0."];
  ]; (* End If VhtMatOut > 0 *)
Return[VhtMatOut]
]
VhtMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := VhtMatRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn]


ClearAll[VhInvtMatRaw];
VhInvtMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[LifePosn>LifeLength,Return[Infinity]];
  Return[
    (VhtToVhInvtMultiply*VhtMatRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn])^(VhtToVhInvtPower)
  ];
];
VhInvtMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := VhInvtMatRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn];


ClearAll[VhtMat];
VhtMat[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
Block[{},
  If[xt   < xmin         ,Return[AvoidLikePlague]];  (* Extremely high marginal utility should make them avoid forbidden x's *)
  If[LifePosn>= LifeLength,
    (* then *)
    If[xt == xmin && etZeroProbVals[[AggState,EmpState]] >0,Return[-Infinity]];
    Return[(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])^(1-rho)*dudht[xt,htStart]
    ]
  ];
  If[LifePosn <EarliestVtSolved,PeriodOfOptimalRule = EarliestVtSolved,PeriodOfOptimalRule = LifePosn]; 
  Return[
    Max[0,
      (VhInvtMatInterpFunc[[PeriodOfOptimalRule]][xt,htStart,AggState,EmpState,AggKt])^-rho
      ] (* End Max[] *)
  ];    (* End Return[] *)
]; (* End of Block *)


ClearAll[ctMatRaw];
ctMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := Block[{},
  If[LifePosn>=LifeLength,Return[xt]];
  If[xt<xmin,Return[0.]];
  If[xt==xmin && etZeroProbVals[[AggState,EmpState]] >0,Return[0]];
  If[xt==xmin && etUnempProb>0,Return[UnempWage]];
  OmegaInvMultiplyFactor = (((G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]])/htStart)^(rho gamma - gamma) beta[[LifePosn+1,EmpState]])^(-1/rho);
  htFactor = htStart/(G[[LifePosn,AggState,EmpState]] epVals[[LifePosn,AggState,EmpState,AggKt]]);
  ctp1Val  = ctMat[xt,htStart,AggState,EmpState,AggKt,LifePosn+1];
  ctSearchMax = Max[Min[ctp1Val + (xmax-ctp1Val)*.01,xt],CertEquivMat[0,1,1,1,LifePosn]];
  ctSearchMin = .9*ctp1Val;
  If[LifePosn == LifeLength-1,ctSearchMin = .49 xt;ctSearchMax = .51 xt];
  FOCResults = 
    Check[FindRoot[
      c == OmegaInvMultiplyFactor*CertEquivMat[xt-c,htFactor*(1-catchup)+ catchup c,AggState,EmpState,LifePosn]
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
ctMatRaw[xt_,htStart_,AggState_,EmpState_,AggKt_] := ctMatRaw[xt,htStart,AggState,EmpState,AggKt,LifePosn];
  

ctMat[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := ctMatInterpFunc[[LifePosn]][xt,htStart,AggState,EmpState,AggKt];
ctMat[xt_,htStart_,AggState_,EmpState_,AggKt_          ] := ctMatInterpFunc[[LifePosn]][xt,htStart,AggState,EmpState,AggKt];


stMat[x_,h_,AggState_,EmpState_,AggK_,LifePosn_] := x - ctMat[x,h,AggState,EmpState,AggK,LifePosn];

htMat[x_,h_,AggState_,EmpState_,AggK_,LifePosn_] := (1-catchup)*h/(G[[LifePosn,AggState,EmpState]])+catchup*ctMat[x,h,AggState,EmpState,AggK,LifePosn];

MPCMat[xt_,htStart_,AggState_,EmpState_,AggKt_,LifePosn_] := 
  (ctMat[xt+.001,htStart,AggState,EmpState,AggKt,LifePosn]-
   ctMat[xt-.001,htStart,AggState,EmpState,AggKt,LifePosn])/.002;

MPCMat[xt_,htStart_,AggState_,EmpState_,AggKt_] := MPC[xt,htStart,AggState,EmpState,AggKt,LifePosn];
   
(* 

Now define the functions which calculate the transition probabilities 

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
                ,ClosestElementTo[RGapMatGrid,RGap]
                  ]] += ProbOfArgs;
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


ExtraDefsFileName = "func_defs_matrices_"<>ProblemToSolveString<>".m";

If[Length[FileNames[ExtraDefsFileName]]>0,Get[ExtraDefsFileName]];