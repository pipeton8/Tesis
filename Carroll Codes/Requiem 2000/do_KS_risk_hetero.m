(*
Solves for behavior of economy with heterogeneous shocks and both patient and impatient consumers
*)

ProblemToSolveString = "KS_risk_hetero";

<<setup_everything.m;

SolvePeriods[2];

dirparams; Get["params_KS_risk_hetero.m"]; dirprog;
<<construct_grids_list.m;
MakeArgArrays[FunctionList];           
MakeArgArrays[ArgsForMatrices];
<<setup_list.m;

LowMem=True;VerboseOutput=False;
KeepSolvingPeriods[98];


dirparams; Get["params_KS_risk_hetero_fine.m"]; dirprog;
<<construct_grids_list.m;
MakeArgArrays[FunctionList];           
MakeArgArrays[ArgsForMatrices];
<<setup_list.m;
VerboseOutput=False;LowMem=True;
KeepSolvingPeriods[100];


dirparams; Get["params_KS_risk_hetero_superfine.m"]; dirprog;
<<construct_grids_list.m;
MakeArgArrays[FunctionList];           
MakeArgArrays[ArgsForMatrices];
<<setup_list.m;
VerboseOutput=False;LowMem=True;
KeepSolvingPeriods[20];

<<clear_future.m;<<clear_extraneous.m;

<<simulation.m;


PeopleToSimulate  =  200;
PeriodsToSimulate = 40000;
LowMem = True;

Simulate[PeopleToSimulate,PeriodsToSimulate];

PeriodsToSimulate = Length[xtSequence];
PeriodsToUse = Round[.95 PeriodsToSimulate];

ktLast         = Take[ktChop                                ,-PeriodsToUse];
ktRatioLast    = ktLast/((1-alpha) ktLast^alpha);
AggStateLast   = Take[AggStateMean                          ,-PeriodsToUse];
MPCMeanLast    = Take[MPCListMean                           ,-PeriodsToUse];
ktMedianLast   = Take[ktMedian                              ,-PeriodsToUse];
ktMedianRatio  = Take[ktMedianLast/((1-alpha) ktLast^alpha) ,-PeriodsToUse];
ktTop1PctLast  = Take[ktTop1Pct                             ,-PeriodsToUse];
ktTop1PctRatio = Take[ktTop1PctLast/((1-alpha) ktLast^alpha),-PeriodsToUse];
ktBot1PctLast  = Take[ktBot1Pct                             ,-PeriodsToUse];
ktBot1PctRatio = Take[ktBot1PctLast/((1-alpha) ktLast^alpha),-PeriodsToUse];
ktTopFracLast  = Take[ktTopFrac                             ,-PeriodsToUse];
kOwLTopLast    = Take[kOwLTop                               ,-PeriodsToUse];
kOwLBotLast    = Take[kOwLBot                               ,-PeriodsToUse];

ptyLevelByPeriod = Map[ptyLevelByAggState[[#]] &, AggStateLast];
ktLevel          = Mean[ptyLevelByPeriod ktLast];
ktMedianLevel    = Mean[ptyLevelByPeriod ktMedianLast];
ktTop1PctLevel   = Mean[ptyLevelByPeriod ktTop1PctLast];
ktBot1PctLevel   = Mean[ptyLevelByPeriod ktBot1PctLast];

MPCMean            = Mean[MPCMeanLast];
ktRatioMean        = Mean[ktRatioLast];
ktTop1PctRatioMean = Mean[ktTop1PctRatio];
ktBot1PctRatioMean = Mean[ktBot1PctRatio];
ktMedianRatioMean  = Mean[ktMedianRatio];
ktTopFracMean      = Mean[ktTopFracLast];
kOwLTopMean        = Mean[kOwLTopLast];
kOwLBotMean        = Mean[kOwLBotLast];


Results = MatrixForm[{
{"ktLevel"           ,ktLevel},
{"ktRatio"           ,ktRatioMean/4},
{"ktBot1PctRatio",    ktBot1PctRatioMean/4},
{"ktMedianRatio",     ktMedianRatioMean/4} ,
{"ktTop1PctRatio",    ktTop1PctRatioMean/4},
{"MPCMeanQuarterly",  MPCMean}             ,
{"MPCMeanAnnual",     1-(1-Mean[MPCMeanLast])^4}, 
{"ktTopFrac",         ktTopFracMean}       ,
{"kOwLTopMean",       kOwLTopMean/4}       ,
{"kOwLBotMean",       kOwLBotMean/4}       
}];

Print[Results];

dirout;
ResultsFile = OpenWrite["KS_risk_hetero_results.m"];
Write[ResultsFile,Results];
Close[ResultsFile];
dirprog;



