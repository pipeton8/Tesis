(*

This program constructs the values for the grids which will be used
to cover the state space for each of the state variables
when using the List method of solution.

*)

Print["Setting up the grids."];

(* % Create the grid points for x, s, and h   
   % by constructing a list of NumOfSteps points spaced evenly in 
   % utility between 0 and Log[max+1] and converting to levels
   % The NumOfSteps, the max's and min's are set in params.m 
*)

LogsGap = Log[smax/sListStart];
LogxGap = Log[xmax/xListStart];

lxListGrid  =Table[LoopOverxGrid,{LoopOverxGrid,Log[xListStart],Log[xmax],LogxGap/(NumOfxListSteps-1)}];
lsListGrid  =Table[LoopOversGrid,{LoopOversGrid,Log[sListStart],Log[smax],LogsGap/(NumOfsListSteps-1)}];


(* Convert the evenly spaced log numbers to level form *)
xListGrid = Exp[lxListGrid];
sListGrid = Exp[lsListGrid];


(* Now construct evenly spaced level numbers *)
xListLevel = Table[LoopOverxGrid,{LoopOverxGrid,xListStart,xmax,(xmax-xListStart)/(NumOfxListSteps-1)}];
sListLevel = Table[LoopOversGrid,{LoopOversGrid,sListStart,smax,(smax-sListStart)/(NumOfsListSteps-1)}];

xListGrid = (xListGrid+xListLevel)/2;
sListGrid = (sListGrid+sListLevel)/2;


xListGrid = Union[xListGrid,Exp[lxListGrid],Flatten[{xSS lbar/ptyLevelByAggState}],{4}];
sListGrid = Union[sListGrid,Exp[lsListGrid],Flatten[{(kSS lbar/ptyLevelByAggState)/(1-Depreciation)}]];
(*
If[ProblemToSolveString == "MPCPerm",

  LogsGap = Log[(smax-sListStart)/(sListStart-smin)];
  LogxGap = Log[(xmax-xListStart)/(xListStart-xmin)];

  lxListGrid  =Table[LoopOverxGrid,{LoopOverxGrid,Log[xListStart-xmin],Log[xmax-xListStart],LogxGap/(NumOfxListSteps-1)}];
  lsListGrid  =Table[LoopOversGrid,{LoopOversGrid,Log[sListStart-smin],Log[smax-sListStart],LogsGap/(NumOfsListSteps-1)}];

  xListGrid = Exp[lxListGrid]+xmin;xListGrid = Union[xListGrid,{1}];
  sListGrid = Exp[lsListGrid]+smin;sListGrid = Union[sListGrid,{0}];
];
*)

kSS = ((1/alpha)((G[[LifePosn,1,1]]^rho)/
      (DiscountByEmpState[[PatientPos]]*(1-Depreciation)) - 1)
      )^(1/(alpha - 1));
RSS = (1+alpha kSS^(alpha-1))(1-Depreciation);
wSS = wFunc[kSS];

xSS = kSS + kSS^alpha;
sSS = kSS/(1-Depreciation);
cSS = xSS - sSS;
(*
cNouncert[xt_,EmpState_] := (1-((RSS DiscountByEmpState[[EmpState]])^(1/rho))/RSS)(xt-wSS+wSS/(1-G[[LifePosn,1,EmpState]]/RSS));

cCrossesxEmpState1 = (xt /. FindRoot[cNouncert[xt,1] == xt, {xt, {2, 4}}]);
cCrossesxEmpState2 = (xt /. FindRoot[cNouncert[xt,2] == xt, {xt, {2, 4}}]);

xListGrid = Union[xListGrid,
  {0.5,cCrossesxEmpState1,cCrossesxEmpState2,(cCrossesxEmpState1+cCrossesxEmpState2)/2,UnempWage+(cCrossesxEmpState1-UnempWage)/2
  ,0.5 cCrossesxEmpState1,1.5 cCrossesxEmpState2}];

sListGrid = Union[sListGrid,
  {1.5, cCrossesxEmpState1, 2.75,1.5 cCrossesxEmpState2} - 1];
*)

(*
In the version of the model with no liquidity constraints, the limit of the 
value function and the expected value function as x->xmin is negative 
infinity.  Hence interpolation will not work well for points between xmin 
and the first interpolation point.  Elsewhere the level of consumption is 
nailed down to be zero when xt = xmin, but in order to figure out how much 
to save at the first interpolation point it is necessary to be able to get 
a good calculation of the value of the function below the first 
interpolation point.  We define here the location of that first 
interpolation point.
*)

(*
FirstGridPointx = .05;
FirstGridPoints = .01;

xListGrid = Prepend[xListGrid,xmin+FirstGridPointx];If[Max[etZeroProbVals]>0,xListGrid = Prepend[xListGrid,xmin+5*FirstGridPointx];(*xListGrid = Prepend[xListGrid,xmin+20*FirstGridPointx];xListGrid = Prepend[xListGrid,xmin+50*FirstGridPointx];xListGrid = Prepend[xListGrid,xmin+300*FirstGridPointx];*)];
sListGrid = Prepend[sListGrid,smin+FirstGridPoints];If[Max[etZeroProbVals]>0,sGrid = Prepend[sGrid,smin+5*FirstGridPoints];(*sGrid = Prepend[sGrid,smin+20*FirstGridPoints];*)];
*)

xListGrid = Sort[Union[Round[xListGrid*1000]/1000]] //N;
sListGrid = Sort[Union[Round[sListGrid*1000]/1000]] //N;


s0ListGrid = Union[Round[1000*Prepend[sListGrid,smin]]/1000] //N;
x0ListGrid = Union[Round[1000*Prepend[xListGrid,xmin]]/1000] //N;


xListGridBaseline = xListGrid;
x0ListGridBaseline = x0ListGrid;


hListGrid = hListGridBaseline = {.05,.25,.50,.70,.85,.90,.95,1.05,1.25,(xmax+1.25)/2,xmax};
h0ListGrid = Prepend[hListGridBaseline,0];
hmin = Min[hListGrid];
(*hListGrid = Union[Round[Join[hListGridBaseline,xListGrid]*1000]/1000 //N];*)
hmin  = Min[hListGrid];
hmax  = Max[hListGrid];

If[catchup == 0,hListGrid = hListGridBaseline = {1.}];

RiskyShareListGrid = {0.};
If[NumOfRiskyShareListSteps>1,RiskyShareListGrid = Flatten[Table[Share,{Share,0,1,1/(NumOfRiskyShareListSteps-1)}]] //N;]

If[SolveForRiskyShare == False,RiskyShareListGrid = {0.}];

NumOfRiskyShareListSteps = Length[RiskyShareListGrid];
RiskyShareMin = 0; RiskyShareMax = 1;

Print["RiskyShareListGrid: "<>ToString[RiskyShareListGrid]];


NumOfhPointsList = Length[hListGrid];
Print["xListGrid: "<>ToString[xListGrid]];
Print["sListGrid: "<>ToString[sListGrid]];
Print["hListGrid: "<>ToString[hListGrid]];

AggKGrid = Union[{0.01 
                 ,0.1  AggkSSByAggState[[1]]
                 ,0.3  AggkSSByAggState[[1]]                 
(*                 ,0.5  AggkSSByAggState[[1]]*)
                 ,0.6  AggkSSByAggState[[1]]                 
(*                 ,0.7  AggkSSByAggState[[1]]*)
                 ,0.8  AggkSSByAggState[[1]]
(*                 ,0.9  AggkSSByAggState[[1]]                 *)
                 ,1.0  AggkSSByAggState[[1]]
(*                 ,1.1  AggkSSByAggState[[1]]*)
                 ,1.2  AggkSSByAggState[[1]]
(*                 ,1.3  AggkSSByAggState[[1]]*)
(*                 ,1.4  AggkSSByAggState[[1]]                 *)
                 ,1.6  AggkSSByAggState[[1]]
(*                 ,1.8  AggkSSByAggState[[1]]                 *)
                 ,2.0  AggkSSByAggState[[1]]                                  
                 }];

AggKGrid = Union[sListGrid,{Max[AggkSSByAggState],Min[AggkSSByAggState]}] (*,0.8 AggkSSByAggState, 1.2 AggkSSByAggState];*)
AggSGrid = AggKGrid/(1-Depreciation);
                 
(*                 (AggkSSByAggState[[1]]+AggkSSByAggState[[2]])/2,AggkSSByAggState[[2]]}];*)
