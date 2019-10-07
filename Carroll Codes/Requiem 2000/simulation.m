(*

Simulation routines to use once optimal rules have converged 

*)

{BehaviorTiming,ShocksTiming} = {0,0};  (* Variables used to get a sense of why simulation is so slow *)

epVal = etVal = 1; (* Values of transitory and permanent shocks within an employment state; kludge to set them equal to one speeds things *)

DeathProb = 1/50 //N; (* Defunct probability of death for earlier version which was pseudo-overlapping-generations in the Blanchard sense *)

ClearAll[DrawNextAggState];
DrawNextAggState[CurrAggState_] := FirstElementGreaterThan[CumAggStateTransitionMatrix[[CurrAggState]],Random[]];
DrawNextAggState[CurrAggStateList_List] := Table[FirstElementGreaterThan[CumAggStateTransitionMatrix[[CurrAggStateList[[i]]]],Random[]],{i,Length[CurrAggStateList]}];

(* ChopBackToPeriod allows user to abort during simulation and discard the in-progress period to get back to the last completed period *)
ChopBackToPeriod[PeriodToChopTo_] := 
  Block[{}, 
    Do[ToExpression[
        ListOfVarsToMake[[i]] <> "Mean = Take[" <> ListOfVarsToMake[[i]] <> 
          "Mean," <> ToString[PeriodToChopTo] <> "]"], {i, 
        Length[ListOfVarsToMake]}];
    Do[ToExpression[
        ListOfVarsToMake[[i]] <> "Sequence = Take[" <> ListOfVarsToMake[[i]] <>
           "Sequence," <> ToString[PeriodToChopTo] <> "]"], {i, 
        Length[ListOfVarsToMake]}];
    Do[ToExpression[
        ListOfDataVars[[i]] <> " = Take[" <> ListOfDataVars[[i]] <> "," <> 
          ToString[PeriodToChopTo] <> "]"], {i, Length[ListOfDataVars]}];
];

xtValue[xt_,htStart_,AggState_,EmpState_,AggKt_]      := xt;
htStartValue[xt_,htStart_,AggState_,EmpState_,AggKt_] := hStart;
htEndValue[xt_,htEnd_,AggState_,EmpState_,AggKt_]     := hEnd;
shtValue[xt_,htEnd_,AggState_,EmpState_,AggKt_]       := ht;
stValue[st_,htEnd_,AggState_,EmpState_,AggKt_]        := st;
Gt[xt_,htStart_,AggState_,EmpState_,AggKt_]           := G[[LifePosn,AggState,EmpState]];
epValue[xt_,htEnd_,AggState_,EmpState_,AggKt_]        := 1;

Simulate[NumToSimulate_,PeriodsToSimulate_] := Block[{},

SaveFigs = False;
NumOfPeriods=PeriodsToSimulate;
NumOfPeople =NumToSimulate;

ListOfxtStateVars   = {"xt","htStart","AggState","EmpState","AggKt","PermIncShock","PermInc","etVal"};
ListOfstStateVars   = {"st","htEnd"  ,"AggState","EmpState","AggKt"};
ListOfxtFuncsToCalc = {"ctList","Gt","epValue","MPCList"};
(*,"EtDLogGtp1"
                      ,"EtDLogGtp2","Gt","EtLogctp1ListGtp1Oct"
                      ,"EtLogctp1ListGtp1OctSquared","EtEulerListApprox"
                      ,"GrossAggSave"
                      };*)
ListOfxtFuncsToAvg = {"xt","htStart","ctList","Gt","AggState","AggKt","MPCList","PermInc"};
(*,"epValue","EtDLogGtp1"
                      ,"EtDLogGtp2","Gt","EtLogctp1ListGtp1Oct"
                      ,"EtLogctp1ListGtp1OctSquared","EtEulerListApprox"
                      ,"GrossAggSave"
                      };*)

ListOfstFuncsToCalc = {};(*"EtMPCPermList";*)
ListOfstFuncsToAvg  = {"st","kt","ktMedian"};(*,"EtMPCPermList";*)
ListOfVarsToMake    = 
  Union[ListOfxtStateVars
       ,ListOfstStateVars
       ,ListOfxtFuncsToCalc
       ,ListOfxtFuncsToAvg
       ,ListOfstFuncsToCalc
       ,ListOfstFuncsToAvg
       ,{"xtStates","stStates"}];

ListOfFuncsToAvg = Union[ListOfxtFuncsToAvg,ListOfstFuncsToAvg];
ListOfDataVars   = {"ktMedian","ktTop1Pct","ktBot1Pct","ktTopFrac","kOwLBot","kOwLTop"};


Do[
  ToExpression[ListOfVarsToMake[[i]]<>"Sequence = {0};"];
  ToExpression[ListOfVarsToMake[[i]]<>"Mean     = {0};"];
,{i,Length[ListOfVarsToMake]}];

ktBotPart = ktTopPart = ktMedian = ktTop1Pct = ktBot1Pct = {0};

(* 
Set up initial values 
*)

hTarget = 1;
CurrentPeriod = 1;

PermIncSequence[[CurrentPeriod]]  = Table[1,{i,NumOfPeople}];
PermIncShockSequence[[CurrentPeriod]] = Table[1,{i,NumOfPeople}];
EmpStateSequence[[CurrentPeriod]] = 
  Table[
    If[i<FracImpatient*NumOfPeople,
      (* then we're still filling up the roster of impatient people *)
        3+If[Random[]<= ug,0,1],
      (* else we've gotten to the patient ones *) 
        1+If[Random[]<= ug,0,1]
    ]
  ,{i,NumOfPeople}];
If[ug == 0,EmpStateSequence[[CurrentPeriod]] = EmpStateSequence[[CurrentPeriod]]-1];

xtSequence[[CurrentPeriod]]       = Table[0,{i,NumOfPeople}];
ktSequence[[CurrentPeriod]]       = Table[0,{i,NumOfPeople}];
AggKtSequence[[CurrentPeriod]]    = Table[0,{i,NumOfPeople}];
etValSequence[[CurrentPeriod]]    = Table[wFunc[kSS],{i,NumOfPeople}];
htStartSequence[[CurrentPeriod]]  = Table[hTarget,{NumOfPeople}];
AggStateSequence[[CurrentPeriod]] = Table[1,{NumOfPeople}];
AggKtMean[[CurrentPeriod]]        = Mean[AggKtSequence[[CurrentPeriod]]];

Which[
  WhichModel == RiskHetero,  
  (* then Both aggregate and idiosyncratic risk *) 
    AggStateNow = 1;Get["ks_targets.m"];
    {kPHatAS1,xPHatAS1,kIHatAS1,xIHatAS1,kHatAS1}={kPHat,xPHat,kIHat,xIHat,kHat};
    AggStateNow = 3;Get["ks_targets.m"];
    {kPHatAS3,xPHatAS3,kIHatAS3,xIHatAS3,kHatAS3}={kPHat,xPHat,kIHat,xIHat,kHat};
    {kPStart,xPStart,kIStart,xIStart,kStart} = 
      ({kPHatAS1,xPHatAS1,kIHatAS1,xIHatAS1,kHatAS1} ptyLevelByAggState[[1]]+
       {kPHatAS3,xPHatAS3,kIHatAS3,xIHatAS3,kHatAS3} ptyLevelByAggState[[3]])/(ptyLevelByAggState[[1]]+ptyLevelByAggState[[3]]);
    Do[
      ktSequence[[CurrentPeriod,i]] = If[EmpStateSequence[[CurrentPeriod,i]] < 3,kPStart,kIStart];
      xtSequence[[CurrentPeriod,i]] = If[EmpStateSequence[[CurrentPeriod,i]] < 3,xPStart,xIStart];
      AggKtSequence[[CurrentPeriod,i]] = kStart;
    ,{i,Length[EmpStateSequence[[CurrentPeriod]]]}];
, WhichModel == Risk,
  (* then it's the version of the model with only aggregate risk *)
    AggStateNow = 1;Get["ks_targets.m"];
    {kHatAS1,xHatAS1,cHatAS1}={kHat,xHat,cHat};
    AggStateNow = 3;Get["ks_targets.m"];
    {kHatAS3,xHatAS3,cHatAS3}={kHat,xHat,cHat};
    {kStart,xStart} = 
      ({kHatAS1,xHatAS1} ptyLevelByAggState[[1]]+
       {kHatAS3,xHatAS3} ptyLevelByAggState[[3]])/(ptyLevelByAggState[[1]]+ptyLevelByAggState[[3]]);
    Do[
      ktSequence[[CurrentPeriod,i]] = kStart;
      xtSequence[[CurrentPeriod,i]] = xStart;
      AggKtSequence[[CurrentPeriod,i]] = kStart;
    ,{i,Length[EmpStateSequence[[CurrentPeriod]]]}];
, WhichModel == RepAgent,
    AggStateNow = 1;Get["ks_targets.m"];
    {kHatAS1,xHatAS1,cHatAS1}={kHat,xHat,cHat};
    AggStateNow = 3;Get["ks_targets.m"];
    {kHatAS3,xHatAS3,cHatAS1}={kHat,xHat,cHat};
    {kStart,xStart} = 
      ({kHatAS1,xHatAS1} ptyLevelByAggState[[1]]+
       {kHatAS3,xHatAS3} ptyLevelByAggState[[3]])/(ptyLevelByAggState[[1]]+ptyLevelByAggState[[3]]);
    Do[
      ktSequence[[CurrentPeriod,i]] = kStart;
      xtSequence[[CurrentPeriod,i]] = xStart;
      AggKtSequence[[CurrentPeriod,i]] = kStart;
    ,{i,Length[EmpStateSequence[[CurrentPeriod]]]}];
];

{ktMedian,ktTop1Pct,ktBot1Pct,ktTopFrac,kOwLBot,kOwLTop} = Table[{0},{6}];

ConstructBehavior[1];  (* Figure out behavior in the first period, given initial state variables *)

KeepSimulating[NumOfPeriods-1];
];

KeepSimulating[PeriodsToKeepSimulating_] := Block[{},

Do[
  CurrentPeriod = Length[xtSequence]+1;
  If[Mod[CurrentPeriod, 10] == 0, (* then *) Print["Current Period = ",CurrentPeriod]];

  Do[
    ToExpression[ListOfVarsToMake[[i]]<>"Sequence = Append["<>ListOfVarsToMake[[i]]<>"Sequence,{}];"];
    ToExpression[ListOfVarsToMake[[i]]<>"Mean = Append["<>ListOfVarsToMake[[i]]<>"Mean,{}];"]
  ,{i,Length[ListOfVarsToMake]}];

  ktMedian = Append[ktMedian,0];
  ktTop1Pct = Append[ktTop1Pct,0];
  ktBot1Pct = Append[ktBot1Pct,0];
  ktTopFrac = Append[ktTopFrac,0];
  kOwLBot   = Append[kOwLBot,0];
  kOwLTop   = Append[kOwLTop,0];

ShocksTiming   += Timing[ DrawRandomShocks[CurrentPeriod]];
BehaviorTiming += Timing[ConstructBehavior[CurrentPeriod]];

,{LoopOverPeriods,PeriodsToKeepSimulating}];

SummarizeResults;

];

(* Now have a function which, given the values of the raw states and shocks, constructs the behavior of the agents *)
ConstructBehavior[CurrentPeriod_] := Block[{},

  xtStatesSequence[[CurrentPeriod]] = 
    Transpose[
      {xtSequence[[CurrentPeriod]]
      ,htStartSequence[[CurrentPeriod]]
      ,AggStateSequence[[CurrentPeriod]]
      ,EmpStateSequence[[CurrentPeriod]]
      ,Table[AggKtMean[[CurrentPeriod]],{i,NumOfPeople}]
      }];

  Do[
    ToExpression[
      ListOfxtFuncsToCalc[[i]] <> "Sequence[[CurrentPeriod]] = " 
        <> "Map[Apply[" <> 
        ListOfxtFuncsToCalc[[i]] <> ", #] &,xtStatesSequence[[CurrentPeriod]]];"]
  ,{i,Length[ListOfxtFuncsToCalc]}];

  stSequence[[CurrentPeriod]] = xtSequence[[CurrentPeriod]]-ctListSequence[[CurrentPeriod]];

  htEndSequence[[CurrentPeriod]] = (1-catchup) htStartSequence[[CurrentPeriod]] 
                  + catchup*ctListSequence[[CurrentPeriod]];

  stStatesSequence[[CurrentPeriod]] = 
    Transpose[
      {stSequence[[CurrentPeriod]]
      ,htEndSequence[[CurrentPeriod]]
      ,AggStateSequence[[CurrentPeriod]]
      ,EmpStateSequence[[CurrentPeriod]]
      ,AggKtSequence[[CurrentPeriod]]
      }];

  Do[
    ToExpression[
      ListOfstFuncsToCalc[[i]] <> "Sequence[[CurrentPeriod]] = " 
        <> "Map[Apply[" <> 
        ListOfstFuncsToCalc[[i]] <> ", #] &,stStatesSequence[[CurrentPeriod]]];"]
  ,{i,Length[ListOfstFuncsToCalc]}];

  Do[
    ToExpression[
      ListOfxtFuncsToAvg[[i]] <> "Mean[[CurrentPeriod]] = "
      <> "Mean["<>ListOfxtFuncsToAvg[[i]]<>"Sequence[[CurrentPeriod]]];"]
  ,{i,Length[ListOfxtFuncsToAvg]}];

  ktMedian[[CurrentPeriod]]  = Median[ktSequence[[CurrentPeriod]]];

  If[WhichModel != RepAgent,  (* the following distributional stuff doesn't make sense with one agent *)
    kAndwL = Transpose[{ktSequence[[CurrentPeriod]],etValSequence[[CurrentPeriod]]}];
    
    kAndwLSortedByk = Sort[kAndwL,#1[[1]] < #2[[1]] &]; (* Sort by the level of capital *)
    
    kAndwLBykQuantile = Transpose[Prepend[Transpose[kAndwLSortedByk], Table[i/Length[kAndwL] //N, {i, Length[kAndwL]}]]];
    QuantilePos = 1;
    kPos = 2;
    wLPos = 3;
    
    kCum    = CumulativeSums[Column[kAndwLBykQuantile,kPos]];
    kAndStuff = AddOutcome[kAndwLBykQuantile,kCum];
    kCumPos = 4;
    
    ClosestQuantileTo99 = FirstElementGreaterThan[Column[kAndStuff,QuantilePos],.99];
    ktTop1Pct[[CurrentPeriod]] = Column[kAndStuff,kPos][[ClosestQuantileTo99]];
    
    ClosestQuantileTo01 = FirstElementGreaterThan[Column[kAndStuff,QuantilePos],.01];
    ktBot1Pct[[CurrentPeriod]] = Column[kAndStuff,kPos][[ClosestQuantileTo01]];
    
    ktTotal = kCum[[-1]];
    ClosestQuantileToBot = FirstElementGreaterThan[Column[kAndStuff,QuantilePos],.667];
    ktTopFrac[[CurrentPeriod]] = 1-kCum[[ClosestQuantileToBot]]/ktTotal;
    
    wLCum = CumulativeSums[Column[kAndStuff,wLPos]];
    
    kOwLBot[[CurrentPeriod]] = kCum[[ClosestQuantileToBot]]/wLCum[[ClosestQuantileToBot]];
    kOwLTop[[CurrentPeriod]] = (ktTotal-kCum[[ClosestQuantileToBot]])/(wLCum[[-1]]-wLCum[[ClosestQuantileToBot]]);
  ];

  Do[
    ToExpression[
      ListOfstFuncsToAvg[[i]] <> "Mean[[CurrentPeriod]] = "
      <> "Mean["<>ListOfstFuncsToAvg[[i]]<>"Sequence[[CurrentPeriod]]];"];
  ,{i,Length[ListOfstFuncsToAvg]}];
        
  If[LowMem == True && CurrentPeriod>3,
    Do[
      ToExpression[ListOfVarsToMake[[j]]<>"Sequence[["<>ToString[CurrentPeriod-2]<>"]] = {};"];
      ,{j,Length[ListOfVarsToMake]}];
  ]; (* End If LowMem *)
  
]; (* End ConstructBehavior *)


DrawRandomShocks[CurrentPeriod_] := Block[{},
  AggStateVal = DrawNextAggState[AggStateSequence[[CurrentPeriod-1,1]]];
(*
  AggStateVal = 1;
  If[CurrentPeriod==100,AggStateVal=2]; 
  If[CurrentPeriod>=101 && CurrentPeriod <200,AggStateVal=3]; 
  If[CurrentPeriod==200,AggStateVal=4];
*)
  AggStateSequence[[CurrentPeriod]] = Table[AggStateVal,{NumOfPeople}];

  EmpStateSequence[[CurrentPeriod]]     = EmpStateSequence[[CurrentPeriod-1]]; (* Fill with dummy values to make the right length *)
  ktSequence[[CurrentPeriod]]           = ktSequence[[CurrentPeriod-1]]; 
  AggKtSequence[[CurrentPeriod]]        = AggKtSequence[[CurrentPeriod-1]];    (* Fill with dummy values to make the right length *)
  xtSequence[[CurrentPeriod]]           = xtSequence[[CurrentPeriod-1]];       (* Fill with dummy values to make the right length *)
  htStartSequence[[CurrentPeriod]]      = htEndSequence[[CurrentPeriod-1]];    (* Fill with dummy values to make the right length *)
  PermIncShockSequence[[CurrentPeriod]] = PermIncShockSequence[[CurrentPeriod-1]];
  PermIncSequence[[CurrentPeriod]]      = PermIncSequence[[CurrentPeriod-1]];  
  etValSequence[[CurrentPeriod]]        = etValSequence[[CurrentPeriod-1]];

(* Do what's necessary to calculate the aggregate capital stock next period *)
  RandomList = Table[Random[],{NumOfPeople}];
  Do[
(*    EmpStateSequence[[CurrentPeriod]] = EmpStateSequence[[CurrentPeriod-1]]; (* Employment state never changes *)*)

    EmpStateSequence[[CurrentPeriod,i]] =
      FirstElementGreaterThan[
        CumEmpStateTransitionMatrix[[AggStateSequence[[CurrentPeriod,i]],EmpStateSequence[[CurrentPeriod-1,i]]]]
        ,RandomList[[i]]
      ];
(*    Interrupt[];*)
(*
    epVal =  epVals[[LifePosn,AggStateSequence[[CurrentPeriod,i]],EmpStateSequence[[CurrentPeriod,i]],
                FirstElementGreaterThan[
                  epProb[[LifePosn,AggStateSequence[[CurrentPeriod,i]],EmpStateSequence[[CurrentPeriod,i]]]],Random[]]
                ]];
    PermIncShockSequence[[CurrentPeriod,i]] = epVal;
    PermIncSequence[[CurrentPeriod,i]] = PermIncSequence[[CurrentPeriod-1,i]] epVal;              
*)
(*
    If[Random[]<DeathProb,
      ktSequence[[CurrentPeriod,i]] = ktSequence[[CurrentPeriod,i]]*PermIncSequence[[CurrentPeriod,i]]/1;
      PermIncSequence[[CurrentPeriod,i]] = 1;(* If they die, keep AggK contribution the same but rescale kt to PermInc = 1 *)
(*      Print["Death, period ",CurrentPeriod,", person ",i];*)
      ]; 
*)    
  ,{i,NumOfPeople}];    
(*Interrupt[];*)
    ktSequence[[CurrentPeriod]]   = stSequence[[CurrentPeriod-1]](1-Depreciation)/(G[[-1,AggStateVal,EmpStateSequence[[CurrentPeriod]]]] epVal);
    AggKtSequence[[CurrentPeriod]]= ktSequence[[CurrentPeriod]] PermIncSequence[[CurrentPeriod]];

EmpStateMean[[CurrentPeriod]] = Mean[EmpStateSequence[[CurrentPeriod]]] // N;

AggKtMean[[CurrentPeriod]] = Mean[AggKtSequence[[CurrentPeriod]]];

(* Now calculate individual circumstances *)
  Do[

    etValSequence[[CurrentPeriod,i]] = wFunc[AggKtMean[[CurrentPeriod]]]*
              etVals[[LifePosn,AggStateSequence[[CurrentPeriod,i]],EmpStateSequence[[CurrentPeriod,i]],
                FirstElementGreaterThan[
                  etProbCum[[LifePosn,AggStateSequence[[CurrentPeriod,i]],EmpStateSequence[[CurrentPeriod,i]]]],Random[]]
                ]];
    xtSequence[[CurrentPeriod,i]] = (Rcertain*(1+rFunc[AggKtMean[[CurrentPeriod]]])
              )*ktSequence[[CurrentPeriod,i]]+etValSequence[[CurrentPeriod,i]];

    htStartSequence[[CurrentPeriod,i]]=htEndSequence[[CurrentPeriod-1,i]]/(G[[LifePosn,AggStateVal,EmpStateSequence[[CurrentPeriod,i]]]] epVal);
  ,{i,NumOfPeople}];
  
];

SummarizeResults := Block[{},

PeriodsSimulated = Length[xtSequence];
Do[
ToExpression[ListOfFuncsToAvg[[i]]<>"Chop = Take["<>ListOfFuncsToAvg[[i]]<>"Mean,"<>ToString[-(PeriodsSimulated-2)]<>"]"]
,{i,Length[ListOfFuncsToAvg]}];

Do[
ToExpression[ListOfFuncsToAvg[[i]]<>"Tm1 = Take["<>ListOfFuncsToAvg[[i]]<>"Mean,{2,"<>ToString[PeriodsSimulated-1]<>"}]"]
,{i,Length[ListOfFuncsToAvg]}];

Do[
ToExpression[ListOfFuncsToAvg[[i]]<>"Tm2 = Take["<>ListOfFuncsToAvg[[i]]<>"Mean,{1,"<>ToString[PeriodsSimulated-2]<>"}]"]
,{i,Length[ListOfFuncsToAvg]}];


Do[
ToExpression[ListOfDataVars[[i]]<>"Chop = Take["<>ListOfDataVars[[i]]<>","<>ToString[-(PeriodsSimulated-2)]<>"]"]
,{i,Length[ListOfDataVars]}];

Do[
ToExpression[ListOfDataVars[[i]]<>"Tm1 = Take["<>ListOfDataVars[[i]]<>",{2,"<>ToString[PeriodsSimulated-1]<>"}]"]
,{i,Length[ListOfDataVars]}];

Do[
ToExpression[ListOfDataVars[[i]]<>"Tm2 = Take["<>ListOfDataVars[[i]]<>",{1,"<>ToString[PeriodsSimulated-2]<>"}]"]
,{i,Length[ListOfDataVars]}];





Off[NumberForm::sigz];  (* Turn off the error message that warns about padding with zeros *)

FullPageSize = {72. 6.,72. 9.};
HalfPageSize = {72. 6.,72. 6./GoldenRatio};

xtMeanPlot = 
  ListPlot[xtMean
    ,PlotJoined->True
    ,PlotLabel->"Ratio of Cash-On-Hand to Permanent Income \!\(x\_t\)"
    ,PlotRange->All
(*    ,PlotRange->{Automatic,{0,xmax}}*)
    ,AxesLabel->{"Period",""}
    ,Ticks -> (TickFunction[#1, #2, TextFunction -> TrimText] &)    
    ,ImageSize->HalfPageSize
    ];
If[SaveFigs == True,dirfigs;Display["xtMeanPlot_"<>ProblemToSolveString<>".eps",xtMeanPlot,"EPS"]];dirprog;

stMeanPlot =
  ListPlot[stMean
    ,PlotJoined->True
    ,PlotLabel->"Ratio of End-Of-Period Savings to Permanent Income \!\(s\_t\)"
    ,PlotRange->All
(*    ,PlotRange->{Automatic,{0,smax}}*)
    ,AxesLabel->{"Period",""}
    ,Ticks -> (TickFunction[#1, #2, TextFunction -> TrimText] &)    
    ,ImageSize->HalfPageSize
    ];
If[SaveFigs == True,dirfigs;Display["stMeanPlot_"<>ProblemToSolveString<>".eps",stMeanPlot,"EPS"]];dirprog;

AggKtMeanPlot =
  ListPlot[AggKtMean
    ,PlotJoined->True
    ,PlotLabel->"Ratio of Wealth (Capital) to Permanent Income \!\(k\_t\)"
    ,PlotRange->All
(*    ,PlotRange->{Automatic,{0,smax}}*)
    ,AxesLabel->{"Period",""}
    ,Ticks -> (TickFunction[#1, #2, TextFunction -> TrimText] &)    
    ,ImageSize->HalfPageSize
    ];
If[SaveFigs == True,dirfigs;Display["KtMeanPlot_"<>ProblemToSolveString<>".eps",stMeanPlot,"EPS"]];dirprog;

ctMeanPlot = 
  ListPlot[ctListMean
    ,PlotJoined->True
    ,PlotLabel->"Ratio of Consumption to Permanent Income \!\(c\_t\)"
    ,PlotRange->All
(*    ,PlotRange->{Automatic,{0.0,1.35}}*)
    ,AxesLabel->{"Period",""}
    ,Ticks -> (TickFunction[#1, #2, TextFunction -> TrimText] &)    
    ,ImageSize->HalfPageSize
    ];
If[SaveFigs == True,dirfigs;Display["ctMeanPlot_"<>ProblemToSolveString<>".eps",ctMeanPlot,"EPS"]];dirprog;
  
GrossAggSaveVals =
  ((Rcertain*(1+rFunc[AggKtMean]) -1/GtMean)stMean + wFunc[AggKtMean] - ctListMean);
  
GrossAggSaveRate = GrossAggSaveVals / ((Rcertain*(1+rFunc[AggKtMean]))AggKtMean + wFunc[AggKtMean]);
(*
GrossAggSaveRatePlot = ListPlot[GrossAggSaveRate
    ,PlotJoined -> True
    ,PlotLabel -> "Aggregate Gross Saving Rate"
    ,PlotRange->All
(*    ,PlotRange -> {{0, Automatic}, {-.10,.40}}
    ,AxesOrigin -> {0, 0}*)
    ,AxesLabel->{"Period",""}
    ,Ticks -> (TickFunction[#1, #2, TextFunction -> TrimText] &)
    ,ImageSize->HalfPageSize
   ];
If[SaveFigs == True,dirfigs;Display["GrossSavPlot_"<>ProblemToSolveString<>".eps",GrossAggSaveRatePlot,"EPS"]];dirprog;

NetAggSaveVals =
  (Rcertain*(1-Depreciation)*(1+rFunc[AggKtMean])-1/GtMean)stMean + wFunc[AggKtMean] - ctListMean;

NetAggSaveRate = NetAggSaveVals / ((Rcertain*(1+rFunc[AggKtMean]))*(1-Depreciation)*stMean + wFunc[AggKtMean]);
NetAggSaveRate = Round[NetAggSaveRate*100000]/100000;

NetAggSaveRatePlot = ListPlot[NetAggSaveRate
    ,PlotJoined -> True
    ,PlotLabel -> "Aggregate Net Saving Rate"
    ,PlotRange->All
(*    ,PlotRange -> {{0, Automatic}, {-.25,.25}}
    ,AxesOrigin -> {0, 0}*)
    ,Ticks -> (TickFunction[#1, #2, TextFunction -> TrimText] &)
    ,AxesLabel->{"Period",""}
    ,ImageSize->HalfPageSize
    ];
If[SaveFigs == True,dirfigs;Display["NetSavePlot_"<>ProblemToSolveString<>".eps",NetAggSaveRatePlot,"EPS"]];dirprog;
*)
(*
EtMPCPermListPlot = 
  ListPlot[EtMPCPermListMean
    ,PlotJoined->True
    ,PlotLabel->"MPC Out Of Permanent Shocks \!\(\[Chi]\)"
    ,PlotRange->{Automatic,{0,1}}
    ,AxesLabel->{"Period",""}
    ,Ticks -> (TickFunction[#1, #2, TextFunction -> TrimText] &)
    ,ImageSize->HalfPageSize    
    ];
If[SaveFigs == True,dirfigs;Display["MPCPermPlot_"<>ProblemToSolveString<>".eps",EtMPCPermListPlot,"EPS"]];dirprog;
*)

AggState1Dum = Map[If[# == 1, 1, 0] &, AggStateChop];
AggState2Dum = Map[If[# == 2, 1, 0] &, AggStateChop];
AggState3Dum = Map[If[# == 3, 1, 0] &, AggStateChop];
AggState4Dum = Map[If[# == 4, 1, 0] &, AggStateChop];

ktTm1TimesAggState1 = ktTm1*AggState1Dum;
ktTm1TimesAggState2 = ktTm1*AggState2Dum;
ktTm1TimesAggState3 = ktTm1*AggState3Dum;
ktTm1TimesAggState4 = ktTm1*AggState4Dum;

MyReg["ktChop",{"ktTm1","AggState2Dum","ktTm1TimesAggState2","AggState3Dum","ktTm1TimesAggState3","AggState4Dum","ktTm1TimesAggState4"}];

];
