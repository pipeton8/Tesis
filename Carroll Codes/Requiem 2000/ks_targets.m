(*
Find target levels of x,k, and c for patient and impatient consumers 
*)

If[FracPatient < 1,  (* If there are both patient and impatient *)

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

Print["{kPHat,xPHat,kIHat,xIHat,kHat,cPHat,cIHat}=",{kPHat,xPHat,kIHat,xIHat,kHat,cPHat,cIHat}];
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
cPHat = cHat = (cP /. RootResults);
];
Print["{kHat,xHat,cHat}=",{kHat,xHat,cHat}];
];



