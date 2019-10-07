(*
Solves for behavior of representative agent with only aggregate shocks
*)

ProblemToSolveString = "KS_RepAgent";

<<setup_everything.m;

SolvePeriods[2];
LowMem = True;VerboseOutput=False;

KeepSolvingPeriods[100];

dirparams; Get["params_KS_RepAgent_fine.m"]; dirprog;
<<construct_grids_list.m;
MakeArgArrays[FunctionList];           
MakeArgArrays[ArgsForMatrices];
<<setup_list.m;
LowMem = True;VerboseOutput=False;
KeepSolvingPeriods[100];

dirparams; Get["params_KS_RepAgent_superfine.m"]; dirprog;
<<construct_grids_list.m;
MakeArgArrays[FunctionList];           
MakeArgArrays[ArgsForMatrices];
<<setup_list.m;
LowMem = True;VerboseOutput=False;
KeepSolvingPeriods[20];

<<simulation.m;<<clear_extraneous.m;

PeopleToSimulate  =  1;
PeriodsToSimulate = 40000;

Simulate[1,PeriodsToSimulate];

PeriodsToSimulate = Length[xtSequence];
PeriodsToUse = Round[.95 PeriodsToSimulate];

ktLast        = Take[ktChop       ,-PeriodsToUse];
ktRatioLast   = ktLast/((1-alpha) ktLast^alpha);
AggStateLast  = Take[AggStateMean ,-PeriodsToUse];
MPCMeanLast   = Take[MPCListMean  ,-PeriodsToUse];
ktMedianLast  = Take[ktMedian     ,-PeriodsToUse];
ktTop1PctLast = Take[ktTop1Pct    ,-PeriodsToUse];

ptyLevelByPeriod = Map[ptyLevelByAggState[[#]] &, AggStateLast];
ktLevel        = Mean[ptyLevelByPeriod ktLast];
ktMedianLevel  = Mean[ptyLevelByPeriod ktMedianLast];
ktTop1PctLevel = Mean[ptyLevelByPeriod ktTop1PctLast];

ktRatio        = Mean[ktRatioLast];

Results = MatrixForm[
{
  {"ktLevel",ktLevel},
  {"ktRatio",ktRatio/4},
  {"MPCMeanQuarterly",Mean[MPCMeanLast]},
  {"MPCMeanAnnual",1-(1-Mean[MPCMeanLast])^4}
}
];


Print[Results];

dirout;
ResultsFile = OpenWrite["KS_RepAgent_results.m"];
Write[ResultsFile,Results];
Close[ResultsFile];
dirprog;
