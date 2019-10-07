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

YearsOfLife    = 300;(* For finite-horizon version, life starts at age 20 and ends at 100 *)
PeriodsPerYear = 1;  (* 1 for annual, 4 for quarterly, 12 for monthly                     *)
FiniteLife = False;  (* If true, read in the mortality risk file *)

LifeLength  = YearsOfLife*PeriodsPerYear;
FirstPeriod = 1;
InterpOrder = 1;     (* % Order of interpolation in the interpolating functions; 1 is linear *)
epsilon=.0001;       (* Useful general purpose 'small number' *)


(* Set up the limits and the grid density for the state variables *)

(* % cash-on-hand limits *)
(* make xmax  =  8. * PeriodsPerYear if doing annual *)
(*xmin = 0;
xDirectStart = xMatStart = xListStart = .1;*)
xDirectStart = xMatStart = xListStart = UnempWage;
xmin = 0;
xmax = (7.5+UnempWage) * PeriodsPerYear;
NumOfxMatSteps    = 24;
NumOfxDirectSteps = 12;
NumOfxListSteps   = 12;

(* % savings limits *)
smin  = 0;
sDirectStart = sListStart = .05;
sMatStart    = 0;
smax  = 6.5;
NumOfsMatSteps = 24;
NumOfsDirectSteps = 12;
NumOfsListSteps   = 12;

(* # habit limits   *)
hmin  = 0.5;
hmax  = 1.5;
NumOfhDirectSteps = NumOfxDirectSteps;
NumOfhMatSteps    = NumOfxMatSteps;
NumOfhListSteps   = NumOfxListSteps;
If[catchup == 0, NumOfhDirectSteps=NumOfhMatSteps=NumOfhListSteps=1];



(*
NumOfEmpStates = 3;
EmpStateTransitionMatrix    = Table[{{0.20,0.60,0.20},{0.20,0.60,0.20},{0.20,0.60,0.20}} //N,{NumOfAggStates}]; (* Youth lasts 40 years in expectation *)
CumEmpStateTransitionMatrix = Table[{{0.20,0.80,1.00},{0.20,0.80,1.00},{0.20,0.80,1.00}} //N,{NumOfAggStates}]; 
*)

NumOfEmpStates = 1;  (* For the moment, consider without unemployment spells *)

EmpStateTransitionMatrix    = Table[{{1}},{NumOfAggStates}]; (* Turn off the employment transition probability *)
CumEmpStateTransitionMatrix = EmpStateTransitionMatrix;

EmpStateGrid = Table[i,{i,NumOfEmpStates}];

etSig  = .10;           (* Standard deviation of the lognormally distributed component of the transitory earnings shock *)
epSig  = .10;           (* Standard deviation of the lognormally distributed permanent income shock *)
etZeroProb    = 0.02;   (* The probability of a zero-income event *)
etUnempProb= 0.00;      (* Probability of an unemployment spell *)
UnempWage = .05;        (* Wages during unemployment spell, as a fraction of permanent income *)

SameForAllAggEmpStates[arg_] := Table[arg,{NumOfAggStates},{NumOfEmpStates}];

etSigVals       = SameForAllAggEmpStates[etSig];
epSigVals       = SameForAllAggEmpStates[epSig];
etZeroProbVals  = SameForAllAggEmpStates[etZeroProb];
etUnempProbVals = SameForAllAggEmpStates[etUnempProb];
UnempWageVals   = SameForAllAggEmpStates[UnempWage];

Cutoff = 4;          (* How many standard deviations above and below the mean to truncate the income shock distributions *)
NumOfetVals = 5;     (* Number of points in the discrete approximation to the transitory shock *)
NumOfepVals = 3;     (* Number of points in the discrete approximation to the permanent shock *)

etValsByAggEmpState = SameForAllAggEmpStates[0];
etProbByAggEmpState = SameForAllAggEmpStates[0];

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



(* Set up the interest rate process *)

Rcertain = 1.03;     (* Net rate of return on the riskless asset *)
Depreciation = 0.08; (* Aggregate depreciation rate *)
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

RStateTransitionMatrix = Rprob;
CumRStateTransitionMatrix = 
  Table[
    CumulativeSums[Rprob[[LoopOverAggStates]]]
  ,{LoopOverAggStates,NumOfAggStates}
];

]; (* End If SolveForRiskyShare *)

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

GrowthByAggState = {1.01,1.06};
GrowthByAggEmpState = Table[GrowthByAggState[[LoopOverAggStates]],{LoopOverAggStates,NumOfAggStates},{NumOfEmpStates}]


Gannual = Table[GrowthByAggEmpState,{LifeLength+1}];

Rannual = Table[Rannual,{NumOfAggStates}];
Rprob   = Table[Rprob  ,{NumOfAggStates}];


(* Convert annual flow variables to per-period rates *)
G      = Gannual^(1/PeriodsPerYear);            (* Growth rates by aggregate state                        *)
R      = Rannual^(1/PeriodsPerYear);            (* Interest rates by aggregate state                      *) 
beta   = betaannual^(1/PeriodsPerYear);         (* Time preference discount factor                        *)


dirprog;
LowMem = True;<<vars_to_clear.m;KeepInterval=40;  (* Determines whether to economize on memory usage *)
VerboseOutput = True; (* Determines how much printing happens *)

dirprog;<<clear_times.m

GetMatFile  = False;MatFileToGet=":TransitionStuff:tempshock.sav";
GetListFile = False;ListFileToGet=":TransitionStuff:tempshock.sav";

