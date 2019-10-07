(* 
% last_period solves for Vt in the last period of life for the given set of grid points and 
% for all possible states 
*)

If[VerboseOutput == True,Print["Running last_period.m"]];

LifePosn = LifeLength;
EarliestVtSolved = EarliestOmegatSolved = LifePosn+1;

If[DirectSolve == True,
  RiskySharetDirectInterpData[[LifePosn]] = AddOutcome[RiskySharetDirectArgArray,Table[0,{Length[RiskySharetDirectArgArray]}]];
  RiskySharetDirectInterpFunc[[LifePosn]] = Interpolation[RiskySharetDirectInterpData[[LifePosn]],InterpolationOrder->1];

  OmegasInvtDirectInterpData[[LifeLength]] = AddOutcome[OmegasInvtDirectArgArray,Table[Infinity,{Length[OmegasInvtDirectArgArray]}]];
  OmegasInvtDirectInterpFunc[[LifeLength]] = Interpolation[OmegasInvtDirectInterpData[[LifePosn]],InterpolationOrder->1];

  If[HabitsMatter == True,
    OmegahInvtDirectInterpData[[LifeLength]] = AddOutcome[OmegahInvtDirectArgArray,Table[-Infinity,{Length[OmegahInvtDirectArgArray]}]];
    OmegahInvtDirectInterpFunc[[LifeLength]] = Interpolation[OmegahInvtDirectInterpData[[LifePosn]],InterpolationOrder->1];
  ];

  If[ConstructValueFunctions == True,
    OmegaInvtDirectInterpData[[LifeLength]] = AddOutcome[OmegaInvtDirectArgArray,Table[Infinity,{Length[OmegaInvtDirectArgArray]}]];
    OmegaInvtDirectInterpFunc[[LifeLength]] = Interpolation[OmegaInvtDirectInterpData[[LifePosn]],InterpolationOrder->1];
  ];
]; (* End If DirectSolve == True *)

If[MatricesSolve == True,
  RiskySharetMatInterpData[[LifePosn]] = AddOutcome[RiskySharetMatArgArray,Table[0,{Length[RiskySharetMatArgArray]}]];
  RiskySharetMatInterpFunc[[LifePosn]] = Interpolation[RiskySharetMatInterpData[[LifePosn]],InterpolationOrder->1];

  OmegasInvtMatInterpData[[LifeLength]] = AddOutcome[OmegasInvtMatArgArray,Table[Infinity,{Length[OmegasInvtMatArgArray]}]];
  OmegasInvtMatInterpFunc[[LifeLength]] = Interpolation[OmegasInvtMatInterpData[[LifePosn]],InterpolationOrder->1];

  If[HabitsMatter == True,
    OmegahInvtMatInterpData[[LifeLength]] = AddOutcome[OmegahInvtMatArgArray,Table[-Infinity,{Length[OmegahInvtMatArgArray]}]];
    OmegahInvtMatInterpFunc[[LifeLength]] = Interpolation[OmegahInvtMatInterpData[[LifePosn]],InterpolationOrder->1];
  ];

  If[ConstructValueFunctions == True,
    OmegaInvtMatInterpData[[LifeLength]] = AddOutcome[OmegaInvtMatArgArray,Table[Infinity,{Length[OmegaInvtMatArgArray]}]];
    OmegaInvtMatInterpFunc[[LifeLength]] = Interpolation[OmegaInvtMatInterpData[[LifePosn]],InterpolationOrder->1];
  ];
]; (* End If MatricesSolve == True *)

If[ListSolve == True,
  RiskySharetListInterpData[[LifePosn]] = AddOutcome[RiskySharetListArgArray,Table[0,{Length[RiskySharetListArgArray]}]];
  RiskySharetListInterpFunc[[LifePosn]] = Interpolation[RiskySharetListInterpData[[LifePosn]],InterpolationOrder->1];

  OmegasInvtListInterpData[[LifeLength]] = AddOutcome[OmegasInvtListArgArray,Table[Infinity,{Length[OmegasInvtListArgArray]}]];
  OmegasInvtListInterpFunc[[LifeLength]] = Interpolation[OmegasInvtListInterpData[[LifePosn]],InterpolationOrder->1];

  If[HabitsMatter == True,
    OmegahInvtListInterpData[[LifeLength]] = AddOutcome[OmegahInvtListArgArray,Table[-Infinity,{Length[OmegahInvtListArgArray]}]];
    OmegahInvtListInterpFunc[[LifeLength]] = Interpolation[OmegahInvtListInterpData[[LifePosn]],InterpolationOrder->1];
  ];

  If[ConstructValueFunctions == True,
    OmegaInvtListInterpData[[LifeLength]] = AddOutcome[OmegaInvtListArgArray,Table[Infinity,{Length[OmegaInvtListArgArray]}]];
    OmegaInvtListInterpFunc[[LifeLength]] = Interpolation[OmegaInvtListInterpData[[LifePosn]],InterpolationOrder->1];
  ];
]; (* End If ListSolve == True *)

EarliestOmegatSolved = LifePosn;

If[DirectSolve == True,
  ctDirectInterpData[[LifePosn]] = AddOutcome[ctDirectArgArray,Map[#[[xtInPos]] &,ctDirectArgArray]];
  ctDirectInterpFunc[[LifePosn]] = Interpolation[ctDirectInterpData[[LifePosn]],InterpolationOrder->1];

  VxInvtDirectInterpData[[LifePosn]] = AddOutcome[VxInvtDirectArgArray,Map[Apply[VxInvtDirectRaw, #] &,VxInvtDirectArgArray]];
  VxInvtDirectInterpFunc[[LifePosn]] = Interpolation[VxInvtDirectInterpData[[LifePosn]],InterpolationOrder->1];

  If[HabitsMatter == True,
    VhInvtDirectInterpData[[LifePosn]] = AddOutcome[VhInvtDirectArgArray,Map[Apply[VhInvtDirectRaw, #] &,VhInvtDirectArgArray]];
    VhInvtDirectInterpFunc[[LifePosn]] = Interpolation[VhInvtDirectInterpData[[LifePosn]],InterpolationOrder->1];
  ];

  If[ConstructValueFunctions == True,
    VInvtDirectInterpData[[LifePosn]] = AddOutcome[VInvtDirectArgArray,Map[Apply[VInvtDirectRaw, #] &,VInvtDirectArgArray]];
    VInvtDirectInterpFunc[[LifePosn]] = Interpolation[VInvtDirectInterpData[[LifePosn]],InterpolationOrder->1];
  ];
];

If[MatricesSolve == True,
 TimeForArrayArgs += 
  Timing[
    VxtArray[[LifePosn]] = 
     Table[VxtMat[x0MatGrid[[i]],hMatGrid[[j]],k,l,AggKGrid[[m]],LifePosn]
     ,{i,Length[x0MatGrid]}
     ,{j,Length[hMatGrid]}
     ,{k,NumOfAggStates}
     ,{l,NumOfEmpStates}
     ,{m,Length[AggKGrid]}
  ];  (* End Table *)

  VxtVector[[LifePosn]] = Flatten[VxtArray[[LifePosn]]];
 
  RGapTimesVxtArray[[LifePosn]] = 
   Table[RGapTimesVxt[x0MatGrid[[i]],hMatGrid[[j]],k,l,AggKGrid[[m]],RGapMatGrid[[n]],LifePosn]
    ,{i,Length[x0MatGrid]}
    ,{j,Length[hMatGrid]}
    ,{k,NumOfAggStates}
    ,{l,NumOfEmpStates}
    ,{m,Length[AggKGrid]}
    ,{n,Length[RGapMatGrid]}
  ];

  RGapTimesVxtVector[[LifePosn]] = Flatten[RGapTimesVxtArray[[LifePosn]]];

  RScaledTimesVxtArray[[LifePosn]] = 
   Table[VxtMat[x0MatGrid[[i]],hMatGrid[[j]],k,l,AggKGrid[[m]],LifePosn]*RScaledMatGrid[[n]]
    ,{i,Length[x0MatGrid]}
    ,{j,Length[hMatGrid]}
    ,{k,NumOfAggStates}
    ,{l,NumOfEmpStates}
    ,{m,Length[AggKGrid]}
    ,{n,Length[RScaledMatGrid]}
  ];

  RScaledTimesVxtVector[[LifePosn]] = Flatten[RScaledTimesVxtArray[[LifePosn]]];

  VhtArray[[LifePosn]] = 
   Table[VhtMat[x0MatGrid[[i]],hMatGrid[[j]],k,l,AggKGrid[[m]],LifePosn]
    ,{i,Length[x0MatGrid]}
    ,{j,Length[hMatGrid]}
    ,{k,NumOfAggStates}
    ,{l,NumOfEmpStates}
    ,{m,Length[AggKGrid]}
  ];

  VhtVector[[LifePosn]] = Flatten[VhtArray[[LifePosn]]];
 ]; (* End Timing *)
];  (* End If MatricesSolve *)

     
If[ListSolve == True,
  ctListInterpData[[LifePosn]] = AddOutcome[ctListArgArray,
  Map[
(*    (
    xbar = #[[xtInPos]] ptyLevelByAggState[[#[[AggStateInPos]]]];
    APCByEmpState[[#[[EmpStateInPos]]]]*
    (xbar-wSS+wSS/(1-G[[LifePosn,1,#[[EmpStateInPos]]]]/RSS))
    /ptyLevelByAggState[[#[[AggStateInPos]]]]
    )
*)    
    (
    AggKtHat = #[[AggKtInPos]] ptyLevelByAggState[[#[[AggStateInPos]]]]/lbar;
    xhatpredfromk = (#[[AggKtInPos]]+#[[AggKtInPos]]^alpha) ptyLevelByAggState[[#[[AggStateInPos]]]]/lbar;
    xhat = #[[xtInPos]] ptyLevelByAggState[[#[[AggStateInPos]]]]/lbar;
    Max[xhatpredfromk (cSS/xSS)  + APCByEmpState[[#[[EmpStateInPos]]]]*(xhat-xhatpredfromk),0.001]
    (lbar /ptyLevelByAggState[[#[[AggStateInPos]]]])
    )
    &,ctListArgArray]];
  ctListInterpFunc[[LifePosn]] = Interpolation[ctListInterpData[[LifePosn]],InterpolationOrder->1];

  VxInvtListInterpData[[LifePosn]] = AddOutcome[VxInvtListArgArray,Map[Apply[VxInvtListRaw, #] &,VxInvtListArgArray]];
  VxInvtListInterpFunc[[LifePosn]] = Interpolation[VxInvtListInterpData[[LifePosn]],InterpolationOrder->1];

  If[HabitsMatter == True,
    VhInvtListInterpData[[LifePosn]] = AddOutcome[VhInvtListArgArray,Map[Apply[VhInvtListRaw, #] &,VhInvtListArgArray]];
    VhInvtListInterpFunc[[LifePosn]] = Interpolation[VhInvtListInterpData[[LifePosn]],InterpolationOrder->1];
  ];

  If[ConstructValueFunctions == True,
    VInvtListInterpData[[LifePosn]] = AddOutcome[VInvtListArgArray,Map[Apply[VInvtListRaw, #] &,VInvtListArgArray]];
    VInvtListInterpFunc[[LifePosn]] = Interpolation[VInvtListInterpData[[LifePosn]],InterpolationOrder->1];
  ];
];

EarliestVtSolved = LifePosn;