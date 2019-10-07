ClearAll[DrawNextAggState];
DrawNextAggState[CurrAggState_] := FirstElementGreaterThan[CumAggStateTransitionMatrix[[CurrAggState]],Random[]];
DrawNextAggState[CurrAggStateList_List] := Table[FirstElementGreaterThan[CumAggStateTransitionMatrix[[CurrAggStateList[[i]]]],Random[]],{i,Length[CurrAggStateList]}];


Which[
  WhichModel == RiskHetero,
  (* then *)
    AggStateNow = 1;Get["ks_targets.m"];
    {kPHatAS1,xPHatAS1,kIHatAS1,xIHatAS1,kHatAS1,cPHatAS1,cIHatAS1}={kPHat,xPHat,kIHat,xIHat,kHat,cPHat,cIHat};
    AggStateNow = 3;Get["ks_targets.m"];
    {kPHatAS3,xPHatAS3,kIHatAS3,xIHatAS3,kHatAS3,cPHatAS3,cIHatAS3}={kPHat,xPHat,kIHat,xIHat,kHat,cPHat,cIHat};
    {kPStart,xPStart,kIStart,xIStart,kStart,cPStart,cIStart} = 
      ({kPHatAS1,xPHatAS1,kIHatAS1,xIHatAS1,kHatAS1,cPHatAS1,cIHatAS1} ptyLevelByAggState[[1]]+
       {kPHatAS3,xPHatAS3,kIHatAS3,xIHatAS3,kHatAS3,cPHatAS3,cIHatAS3} ptyLevelByAggState[[3]])/(ptyLevelByAggState[[1]]+ptyLevelByAggState[[3]]);
, WhichModel == Risk || WhichModel == RepAgent,
    AggStateNow = 1;Get["ks_targets.m"];
    {kPHatAS1,xPHatAS1,cPHatAS1,kHatAS1}={kHat,xHat,cHat,kHat};{kIHatAS1,xIHatAS1,cIHatAS1}={0,0,0};
    AggStateNow = 3;Get["ks_targets.m"];
    {kPHatAS3,xPHatAS3,cPHatAS3,kHatAS3}={kHat,xHat,cHat,kHat};{kIHatAS3,xIHatAS3,cIHatAS3}={0,0,0};
    {kPStart,xPStart,kIStart,xIStart,kStart,cPStart,cIStart} = 
      ({kPHatAS1,xPHatAS1,kIHatAS1,xIHatAS1,kHatAS1,cPHatAS1,cIHatAS1} ptyLevelByAggState[[1]]+
       {kPHatAS3,xPHatAS3,kIHatAS3,xIHatAS3,kHatAS3,cPHatAS3,cIHatAS3} ptyLevelByAggState[[3]])/(ptyLevelByAggState[[1]]+ptyLevelByAggState[[3]])
];

NumOfPeriods = 500;

AggStateByPeriod = AggState1Dum = AggState2Dum = AggState3Dum = AggState4Dum = kMean = kIMean = kPMean = xMean = xIMean = xPMean = cMean = cPMean = cIMean = sIMean = sPMean = Table[0,{NumOfPeriods}];

{kPMean[[1]],kIMean[[1]],kMean[[1]],xIMean[[1]],xPMean[[1]],cIMean[[1]],cPMean[[1]]} =
  {kPStart,kIStart,kStart,xIStart,xPStart,cIStart,cPStart}


xMean[[1]] = kStart+kStart^alpha;
cMean[[1]] = cIMean[[1]] FracImpatient + cPMean[[1]] FracPatient;

AggStateByPeriod[[1]] = 1;

Do[
  If[VerboseOutput == True,Print["Period: ",LoopOverPeriods]];
  AggStateByPeriod[[LoopOverPeriods]] = DrawNextAggState[AggStateByPeriod[[LoopOverPeriods-1]]];

  AggState1Dum[[LoopOverPeriods]] = If[AggStateByPeriod[[LoopOverPeriods]] == 1,1,0];
  AggState2Dum[[LoopOverPeriods]] = If[AggStateByPeriod[[LoopOverPeriods]] == 2,1,0];
  AggState3Dum[[LoopOverPeriods]] = If[AggStateByPeriod[[LoopOverPeriods]] == 3,1,0];
  AggState4Dum[[LoopOverPeriods]] = If[AggStateByPeriod[[LoopOverPeriods]] == 4,1,0];
  kPMean[[LoopOverPeriods]] = 
      ((1-Depreciation)/G[[LifePosn, AggStateByPeriod[[LoopOverPeriods]], PatientU]])
         *(
           (pigg00/pigg)*ug
            (xPMean[[LoopOverPeriods-1]]-cPMean[[LoopOverPeriods-1]])
          +(pigg10/pigg)*(1-ug)
            (xPMean[[LoopOverPeriods-1]]-cPMean[[LoopOverPeriods-1]])
          )/ug;
  If[FracImpatient>0, 
    (* then *)
    kIMean[[LoopOverPeriods]] = 
      ((1-Depreciation)/G[[LifePosn, AggStateByPeriod[[LoopOverPeriods]], ImpatientU]])
         *(
           (pigg00/pigg)*ug
            (xIMean[[LoopOverPeriods-1]]-cIMean[[LoopOverPeriods-1]])
          +(pigg10/pigg)*(1-ug)
            (xIMean[[LoopOverPeriods-1]]-cIMean[[LoopOverPeriods-1]])
          )/ug;
  ];
  kMean[[LoopOverPeriods]]  = kPMean[[LoopOverPeriods]] FracPatient + kIMean[[LoopOverPeriods]] FracImpatient;
  xPMean[[LoopOverPeriods]] = kPMean[[LoopOverPeriods]](1 + rFunc[kMean[[LoopOverPeriods]]]) + wFunc[kMean[[LoopOverPeriods]]];
  If[FracImpatient>0,
    (* then *)
      xIMean[[LoopOverPeriods]] = kIMean[[LoopOverPeriods]](1 + rFunc[kMean[[LoopOverPeriods]]]) + wFunc[kMean[[LoopOverPeriods]]];
  ];
  xMean[[LoopOverPeriods]]  = xPMean[[LoopOverPeriods]] FracPatient + xIMean[[LoopOverPeriods]] FracImpatient;
  cPMean[[LoopOverPeriods]] = 
          ctList[xPMean[[LoopOverPeriods]],1,AggStateByPeriod[[LoopOverPeriods]],PatientU,kMean[[LoopOverPeriods]],LifePosn] ug
         +ctList[xPMean[[LoopOverPeriods]],1,AggStateByPeriod[[LoopOverPeriods]],PatientE,kMean[[LoopOverPeriods]],LifePosn] (1-ug);
  If[FracImpatient>0,
  (* then *)
     cIMean[[LoopOverPeriods]] = 
          ctList[xIMean[[LoopOverPeriods]],1,AggStateByPeriod[[LoopOverPeriods]],ImpatientU,kMean[[LoopOverPeriods]],LifePosn] ug
         +ctList[xIMean[[LoopOverPeriods]],1,AggStateByPeriod[[LoopOverPeriods]],ImpatientE,kMean[[LoopOverPeriods]],LifePosn] (1-ug);
  ];

,{LoopOverPeriods,2,NumOfPeriods}];

kChop = Take[kMean,-(Length[kMean]-1)];
kTm1  = Take[kMean,{-Length[kMean],Length[kMean]-1}];

AggState1DumChop = Take[AggState1Dum,-(Length[kMean]-1)];
AggState2DumChop = Take[AggState2Dum,-(Length[kMean]-1)];
AggState3DumChop = Take[AggState3Dum,-(Length[kMean]-1)];
AggState4DumChop = Take[AggState4Dum,-(Length[kMean]-1)];

kTm1TimesAggState1Dum = kTm1*AggState1DumChop;
kTm1TimesAggState2Dum = kTm1*AggState2DumChop;
kTm1TimesAggState3Dum = kTm1*AggState3DumChop;
kTm1TimesAggState4Dum = kTm1*AggState4DumChop;

MyReg["kChop", {"kTm1", "AggState2DumChop", "kTm1TimesAggState2Dum","AggState3DumChop", "kTm1TimesAggState3Dum", "AggState4DumChop", "kTm1TimesAggState4Dum"}];

(*
{Intercept,kTm1Coeff,kTm1TimesAggState3DumCoeff,AggState2DumChopCoeff, AggState3DumChopCoeff,AggState4DumChopCoeff} =
(BestFitParameters /. Outstuff);

kTargetAS14 = Intercept/(1-kTm1Coeff);
kTargetAS23 = (Intercept+AggState3DumChopCoeff)/(1-(kTm1Coeff+kTm1TimesAggState3DumCoeff));

AggkSSByAggState = {kTargetAS14,kTargetAS23,kTargetAS23,kTargetAS14};
kAR1ByAggState   = {kTm1Coeff,kTm1Coeff+kTm1TimesAggState3DumCoeff,kTm1Coeff+kTm1TimesAggState3DumCoeff,kTm1Coeff};
*)
