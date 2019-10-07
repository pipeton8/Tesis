
(*

Defines a set of parameter values to be used for comparing the
model's solution to the perfect foresight/certainty equivalent
solution.

Note that to eliminate transitory labor income risk the following lines
need to be added to the appropriate point in life_cycle.m

{etValsTemp,etProbTemp} = {{1},{1}};NumOfetVals=1;
{etValsPart3,etProbPart3} = {etValsTemp,etProbTemp};

*)

Print["Reading in the baseline parameter values."];

DiscountByEmpState={0.99,0.99}; (* Time preference factor, by emp status (proxy for age) *)
{PatientPos,ImpatientPos} = {1,2}; (* The patient consumers' preferences will determine agg cap stock *)
{FracPatient,FracImpatient} = {1.000,0.000};

rho = 3.0;           (* Coefficient of relative risk aversion - \sigma in C-O-W                      *)
gamma = 0;           (* Parameter that governs the strength of habit formation in utility *)
catchup = 0;         (* Parameter that governs how quickly habits catch up to consumption *)
HabitsMatter = False;(* Determines whether habits are incorporated in the solution or not *)

(*
Now set up Krusell-Smith parameter values
*)

alpha = 0.36;
Depreciation = .025;


etSig  = .10;        (* Standard deviation of the lognormally distributed component of the transitory earnings shock *)
epSig  = .10;        (* Standard deviation of the lognormally distributed permanent income shock *)
Cutoff = 4;          (* How many standard deviations above and below the mean to truncate the income shock distributions *)
etZeroProb = 0.00;   (* The probability of a zero-income event *)
etUnempProb= 0.01;   (* Probability of an unemployment spell *)
UnempWage = 0.5;     (* Wages during unemployment spell, as a fraction of permanent income *)

NumOfetVals = 3;     (* Number of points in the discrete approximation to the transitory shock *)
NumOfepVals = 1;     (* Number of points in the discrete approximation to the permanent shock *)

Rcertain = 1.00;     (* Rate of return on the riskless asset *)
RSig = .075;         (* The standard deviation of the rate of return on the risky asset *)
RCutoff = 4;         (* How many standard deviations off to cut off the distribution of R *)
NumOfRVals   = 1;    (* Number of points in the discrete approximation to the rate-of-return shock *)
EquityPremium=.03;   (* How much higher is the rate of return on equities than the certain rate? *)

SolveForRiskyShare = False; (* If True, solves version of model with a risky and a riskless asset *)

Rannual = Rprob = {1.};

If[SolveForRiskyShare == True,

(* Now set up the shocks and probabilities for the process for the stock market *)
{Rannual,Rprob} = DiscreteApproxToMeanOneLogNormalWithCutoff[RCutoff,RSig,NumOfRVals];

(* Now blow up the level of the shocks to achieve the desired equity premium *)
Rannual = (Rcertain+EquityPremium) Rannual;

(* Now build in a 1 percent chance of a 90 percent decline in the stock market *)
Rannual = (Rannual-.1*.01)/.99; Rannual = Prepend[Rannual,.1];Rprob = .99 Rprob;Rprob = Prepend[Rprob,.01];NumOfRVals++;

]; (* End If SolveForRiskyShare *)

YearsOfLife    = 300;(* For finite-horizon version, life starts at age 20 and ends at 100 *)
PeriodsPerYear = 1;  (* 1 for annual, 4 for quarterly, 12 for monthly                     *)
FiniteLife = False;  (* If true, read in the mortality risk file *)

LifeLength  = YearsOfLife*PeriodsPerYear;
FirstPeriod = 1;
InterpOrder = 1;     (* % Order of interpolation in the interpolating functions; 1 is linear *)
epsilon=.0001;       (* Useful general purpose 'small number' *)

InterpOrder = 1;     (* % Order of interpolation in the interpolating functions; 1 is linear *)

(*

The value function and marginal value functions need to be normalized 
before the interpolating function is constructed; see the section 
Renormalizations in the mathematical appendix to see why the marginal value 
function with respect to s or x needs to be raised to the 1/-rho and why 
the appropriate transformation for the marginal value functions with 
respect to h is to multiply by (1-rho) then raise to the 1/(1-rho)

*)

OmegastToOmegasInvtMultiply = 1;        OmegastToOmegasInvtPower = (1/-rho);
OmegahtToOmegahInvtMultiply = -gamma;   OmegahtToOmegahInvtPower = (1/(1-rho));
OmegatToOmegaInvtMultiply = (1/(1-rho));OmegatToOmegaInvtPower   = (1/(1-rho));

VxtToVxInvtMultiply = 1;        VxtToVxInvtPower = (1/-rho);
VhtToVhInvtMultiply = -gamma;   VhtToVhInvtPower = (1/(1-rho));
VtToVInvtMultiply = (1/(1-rho));VtToVInvtPower   = (1/(1-rho));


RiskyTimeCost = 0; (* How much time does it take to monitor risky investments? *)

NumOfRiskyShareDirectSteps = 6.;     (* How many possible portfolio shares to consider? *)
NumOfRiskyShareMatSteps    = 6.;     (* How many possible portfolio shares to consider? *)
NumOfRiskyShareListSteps   = 6.;     (* How many possible portfolio shares to consider? *)


(* % The AggStateTransitionMatrix gives the probabilities of moving to 
   % each aggregate state tomorrow given a current aggregate state today
*)


NumOfAggStates = 4; (* State 1 = Currently Boom, Came from Boom; State 2 = Currently Bust, Came from Boom; State 3 = Currently Bust, Came from Bust; State 4 = Currently Boom, Came From Bust *)
AggStateTransitionMatrix = 
    {
     {0.875,0.125,0.000,0.000},
     {0.000,0.000,0.875,0.125},
     {0.000,0.000,0.875,0.125},
     {0.875,0.125,0.000,0.000}
    };

CumAggStateTransitionMatrix = 
    {
     {0.875,1.000,1.000,1.000},
     {0.000,0.000,0.875,1.000},
     {0.000,0.000,0.875,1.000},
     {0.875,1.000,1.000,1.000}
    };

(*
NumOfAggStates = 3;
AggStateTransitionMatrix    = {{.95,0.05,1.00},{0.00,0.00,1.00},{1.00,1.00,1.00}};
*)
(*
NumOfAggStates = 2;
AggStateTransitionMatrix    = {{0.95,0.05},{0.20,0.80}};
CumAggStateTransitionMatrix = {{0.95,1.00},{0.05,1.00}};
*)
(*
NumOfAggStates = 1;
AggStateTransitionMatrix = {{1}};
CumAggStateTransitionMatrix = {{1}};
*)
AggStateGrid = Table[i,{i,NumOfAggStates}];

(*
NumOfEmpStates = 2;  (* State 1 is youth, expected duration 160 quarters = 40 years; State 2 is Old Age, expected duration 100 quarters = 25 years *)
Youth = 1;OldAge = 2;
EmpStateTransitionMatrix    = Table[{{1.000,0.000},{0.000,1.000}} //N,{NumOfAggStates}]; (* Youth lasts 40 years in expectation *)
CumEmpStateTransitionMatrix = Table[{{1.000,1.000},{0.000,1.000}} //N,{NumOfAggStates}]; (* Old age lasts 20 years in expectation *)
*)

pibb = pigg = 7/8 // N;
pigb = (1 - pibb);
pibg = (1 - pigg);

ub = .1;
ug = .04;

EmpStateFraction = 
  {
   {ug,1-ug},
   {ub,1-ub}
  };
  
pigg11 = 0.850694
pigb11 = 0.115885
pigg10 = 0.024306
pigb10 = 0.009115
pibg11 = 0.122917
pibb11 = 0.836111
pibg10 = 0.002083
pibb10 = 0.038889
pigg01 = 0.583333
pigb01 = 0.031250
pigg00 = 0.291667
pigb00 = 0.093750
pibg01 = 0.093750
pibb01 = 0.35
pibg00 = 0.03125
pibb00 = 0.525000


NumOfEmpStates = 2;  
EmpStateTransitionMatrix    = 
  {
   {{pigg00,pigg01},{pigg10,pigg11}}/pigg,
   {{pigb00,pigb01},{pigb10,pigb11}}/pigb,
   {{pibb00,pibb01},{pibb10,pibb11}}/pibb,
   {{pibg00,pibg01},{pibg10,pibg11}}/pibg     
  } //N;

CumEmpStateTransitionMatrix = 
  Table[
      CumulativeSums[
          EmpStateTransitionMatrix[[LoopOverAggStates,LoopOverEmpStates]]
    ]
  ,{LoopOverAggStates,NumOfAggStates}
  ,{LoopOverEmpStates,NumOfEmpStates}
];

(*
NumOfEmpStates = 4;
EmpStateTransitionMatrix    =
  {
   {{pigg00,pigg01,0,0},{pigg10,pigg11,0,0},{0,0,pigg00,pigg01},{0,0,pigg10,pigg11}}/pigg,
   {{pigb00,pigb01,0,0},{pigb10,pigb11,0,0},{0,0,pigb00,pigb01},{0,0,pigb10,pigb11}}/pigb,
   {{pibb00,pibb01,0,0},{pibb10,pibb11,0,0},{0,0,pibb00,pibb01},{0,0,pibb10,pibb11}}/pibb,
   {{pibg00,pibg01,0,0},{pibg10,pibg11,0,0},{0,0,pibg00,pibg01},{0,0,pibg10,pibg11}}/pibg
  } //N;

CumEmpStateTransitionMatrix =
  Table[
      CumulativeSums[
          EmpStateTransitionMatrix[[LoopOverAggStates,LoopOverEmpStates]]
    ]
  ,{LoopOverAggStates,NumOfAggStates}
  ,{LoopOverEmpStates,NumOfEmpStates}
];
*)

(*
NumOfEmpStates = 1;  (* For the moment, consider without unemployment spells *)

EmpStateTransitionMatrix    = Table[{{1}},{NumOfAggStates}]; (* Turn off the employment transition probability *)
CumEmpStateTransitionMatrix = EmpStateTransitionMatrix;
*)
EmpStateGrid = Table[i,{i,NumOfEmpStates}];

Gannual = Table[{},{NumOfAggStates}];


SameForAllAggEmpStates[arg_] := Table[arg,{NumOfAggStates},{NumOfEmpStates}];

etSigVals       = SameForAllAggEmpStates[etSig];
epSigVals       = SameForAllAggEmpStates[epSig];
(*  epSigVals = {{0.05,0.05},{0.10,0.10}};*)
etZeroProbVals  = SameForAllAggEmpStates[etZeroProb];
etUnempProbVals = SameForAllAggEmpStates[etUnempProb];
UnempWageVals   = SameForAllAggEmpStates[UnempWage];

Cutoff = 4;          (* How many standard deviations above and below the mean to truncate the income shock distributions *)
NumOfetVals = 1;     (* Number of points in the discrete approximation to the transitory shock *)
NumOfepVals = 1;     (* Number of points in the discrete approximation to the permanent shock *)

etValsByAggEmpState = SameForAllAggEmpStates[0];
etProbByAggEmpState = SameForAllAggEmpStates[0];

(*epValsByAggEmpState = {{0.10,0.10},{0.20,0.20}};*)
(*etUnempProbVals = {{.05,.05},{.08,.08}};*)

Do[
  {etValsForThisAggEmpState,etProbForThisAggEmpState} =
      DiscreteApproxToMeanOneLogNormalWithCutoff[Cutoff,etSigVals[[i,j]],NumOfetVals];
  If[etZeroProbVals[[i,j]]>0,
    (* then *)
     etValsForThisAggEmpState = etValsForThisAggEmpState/(1-etZeroProbVals[[i,j]]);
     etValsForThisAggEmpState = Prepend[etValsForThisAggEmpState,0];
     etProbForThisAggEmpState = (1-etZeroProbVals[[i,j]])etProbForThisAggEmpState;
     etProbForThisAggEmpState = Prepend[etProbForThisAggEmpState,etZeroProbVals[[i,j]]]
    ]; (* End If etZeroProbVals[[i,j]]>0 *)
  If[etUnempProbVals[[i,j]]>0,
    (* then *)
     etValsForThisAggEmpState = etValsForThisAggEmpState*(1-etUnempProbVals[[i,j]]*UnempWage)/(1-etUnempProbVals[[i,j]]);
     etValsForThisAggEmpState = Prepend[etValsForThisAggEmpState,UnempWage];
     etProbForThisAggEmpState = (1-etUnempProbVals[[i,j]])etProbForThisAggEmpState;
     etProbForThisAggEmpState = Prepend[etProbForThisAggEmpState,etUnempProbVals[[i,j]]]
  ]; (* End If etUnempProbVals[[i,j]]>0 *)
  etValsByAggEmpState[[i,j]] = etValsForThisAggEmpState;
  etProbByAggEmpState[[i,j]] = etProbForThisAggEmpState;
  ,{i,NumOfAggStates}
  ,{j,NumOfEmpStates}
];

etValsByAggEmpState =
  {
   {{1},{1}},
   {{1},{1}},
   {{1},{1}},
   {{1},{1}}
  };

etValsByAggEmpState =
  {
   {{0},{1/(1-ug)}},
   {{0},{1/(1-ub)}},
   {{0},{1/(1-ub)}},
   {{0},{1/(1-ug)}}
  };
etValsByAggEmpState =
  {
   {{UnempWage},{(1-ug UnempWage)/(1-ug)},{UnempWage},{(1-ug UnempWage)/(1-ug)}},
   {{UnempWage},{(1-ub UnempWage)/(1-ub)},{UnempWage},{(1-ub UnempWage)/(1-ub)}},
   {{UnempWage},{(1-ub UnempWage)/(1-ub)},{UnempWage},{(1-ub UnempWage)/(1-ub)}},
   {{UnempWage},{(1-ug UnempWage)/(1-ug)},{UnempWage},{(1-ug UnempWage)/(1-ug)}}
  };

(*
etValsByAggEmpState =
  {
   {{.93},{.93}},
   {{.93},{.93}},
   {{.93},{.93}},
   {{.93},{.93}}
  };

etValsByAggEmpState =
  {
   {{.96},{.96}},
   {{.90},{.90}},
   {{.90},{.90}},
   {{.96},{.96}}
  };
*)
(*
etValsByAggEmpState = 
  {
   {{.93},{.93}},
   {{.93},{.93}},
   {{.93},{.93}},
   {{.93},{.93}}
  };
*)

etProbByAggEmpState =
  {
   {{1},{1}},
   {{1},{1}},
   {{1},{1}},
   {{1},{1}}
  };

etProbByAggEmpState =
  {
   {{1},{1},{1},{1}},
   {{1},{1},{1},{1}},
   {{1},{1},{1},{1}},
   {{1},{1},{1},{1}}
  };

NumOfetVals=1;
  
epValsByAggEmpState = SameForAllAggEmpStates[0];
epProbByAggEmpState = SameForAllAggEmpStates[0];

Do[
  {epValsForThisAggEmpState,epProbForThisAggEmpState} = 
      DiscreteApproxToMeanOneLogNormalWithCutoff[Cutoff,epSigVals[[i,j]],NumOfepVals];  
  epValsByAggEmpState[[i,j]] = epValsForThisAggEmpState;
  epProbByAggEmpState[[i,j]] = epProbForThisAggEmpState;  
  ,{i,NumOfAggStates}
  ,{j,NumOfEmpStates}
];

epProb = epVals = etProb = etVals = Table[0,{LifeLength}];
Do[epProb[[LoopOverLifePosn]]=epProbByAggEmpState,{LoopOverLifePosn,1,LifeLength}];
Do[epVals[[LoopOverLifePosn]]=epValsByAggEmpState,{LoopOverLifePosn,1,LifeLength}];
Do[etVals[[LoopOverLifePosn]]=etValsByAggEmpState,{LoopOverLifePosn,1,LifeLength}];
Do[etProb[[LoopOverLifePosn]]=etProbByAggEmpState,{LoopOverLifePosn,1,LifeLength}];

epProbCum = epProb;  (* These are only legitimate because there is only one possible realization in each employment state *)
etProbCum = etProb;  

ProbOfDeath  = Table[0,{LifeLength}];
If[FiniteLife==True,dirprog;<<mortality_probs.m];

betaannual = Table[{},{LifeLength+1}];

Do[
betaannual[[(LoopOverYear-1)*PeriodsPerYear+LoopOverPeriodsOfYear]] = 
  (1-ProbOfDeath[[LoopOverYear]]) DiscountByEmpState,   
{LoopOverPeriodsOfYear,PeriodsPerYear},{LoopOverYear,YearsOfLife}
];

ptyLevelByAggState  = {1.01,0.99,0.99,1.01}^(1/(1-alpha));
LLevelByAggState    = {0.96,0.90,0.90,0.96};
ptyGrowthByAggState = {1.00,(0.99/1.01),1.00,(1.01/0.99)}^(1/(1-alpha)) //N; 
LGrowthByAggState   = {1.00, .90/.96, 1.00, .96/.90};
GrowthByAggState    = ptyGrowthByAggState LGrowthByAggState;
LhatByAggState      = ptyLevelByAggState;

GrowthByAggEmpState = 
    Table[GrowthByAggState[[LoopOverAggStates]]
    ,{LoopOverAggStates,NumOfAggStates}
    ,{NumOfEmpStates}
    ];

Gannual = Table[GrowthByAggEmpState,{LifeLength+1}];


Rannual = Table[Rannual,{NumOfAggStates}];
Rprob   = Table[Rprob  ,{NumOfAggStates}];


RStateTransitionMatrix = Rprob;
CumRStateTransitionMatrix = 
  Table[
    CumulativeSums[Rprob[[LoopOverAggStates]]]
  ,{LoopOverAggStates,NumOfAggStates}
];

(* Convert annual flow variables to per-period rates *)
G      = Gannual^(1/PeriodsPerYear);            (* Growth rates by aggregate state                        *)
R      = Rannual^(1/PeriodsPerYear);            (* Interest rates by aggregate state                      *) 
beta   = betaannual^(1/PeriodsPerYear);         (* Time preference discount factor                        *)

(* Useful general purpose 'small number' *)
epsilon=.0001;


(* Set up the limits and the grid density for the state variables *)

(* % cash-on-hand limits *)
(* make xmax  =  8. * PeriodsPerYear if doing annual *)
(*xmin = 0;
xDirectStart = xMatStart = xListStart = .1;*)
xmin = xDirectStart = xMatStart = xListStart = 0;
If[xmin == 0, xDirectStart = xMatStart = xListStart = .5];
xmax = (120+UnempWage) * PeriodsPerYear;
NumOfxMatSteps    = 176;
NumOfxDirectSteps = 15;
NumOfxListSteps   = 20;

(* % savings limits *)
smin  = 0;
sDirectStart = sListStart = .10;
sMatStart    = 0;
smax  = 120;
NumOfsMatSteps = 176;
NumOfsDirectSteps = 15;
NumOfsListSteps   = 20;

(* # habit limits   *)
hmin  = 0.5;
hmax  = 1.5;
NumOfhDirectSteps = NumOfxDirectSteps;
NumOfhMatSteps    = NumOfxMatSteps;
NumOfhListSteps   = NumOfxListSteps;
If[catchup == 0, NumOfhDirectSteps=NumOfhMatSteps=NumOfhListSteps=1];

dirprog;
LowMem = False;<<vars_to_clear.m;KeepInterval=20;  (* Determines whether to economize on memory usage *)
VerboseOutput = True; (* Determines how much printing happens *)

<<clear_times.m

GetMatFile  = False;FileToGet=":Transition Matrices:x8_s6_xN100_sN100_R101_NoR";
GetListFile = False;FileToGet=":Transition Matrices:x8_s6_xN100_sN100_R101_NoR";


(*
average value of labor = .3271 obtained from Krusell;
expansion and contraction values obtained from the assumption
that economy spends half the time in expansions and half in 
contractions and that the ratio of aggregate labor supply
in contractions to expansions is 0.90/0.96 corresponding
to unemployment rates of 10 and 4 percent, respectively.

Solve[{le + lc == .3271*2, lc == le(.9/.96)}]
*)

Labor = .3271;

(*
LaborContraction = .316548;
LaborExpansion   = .337652;
*)

AggLByAggState = {.96,.90,.90,.96};
AggLGrowthByAggState = {1.00,.90/.96,1.00,.96/.90};
(*AggLGrowthByAggState = {1.00,1.00,1.00,1.00};*)

lbar = 0.93;
RSS = (1+alpha kSS^(alpha-1))(1-Depreciation);
wSS = wFunc[kSS];
APCByEmpState= (1-((RSS DiscountByEmpState)^(1/rho))/RSS);
ptyLevelByAggState = {1.01,0.99,0.99,1.01}^(1/(1-alpha)) AggLByAggState;


AggKOverlbarSSByAggState = 
  ((1/alpha)(Table[1,{NumOfAggStates}]^rho/(DiscountByEmpState[[1]]
             *(1-Depreciation))-1))^(1/(alpha-1));

AggkSSByAggState = AggKOverlbarSSByAggState lbar / ptyLevelByAggState;

(*AggkSSByAggState = {23.2};*)
kAR1ByAggState = Table[0.968,{NumOfAggStates}];

rFunc[kAgg_] := alpha kAgg^(alpha - 1);
wFunc[kAgg_] := (1 - alpha) kAgg^alpha;
