(*

This program constructs the values for the grids which will be used
to cover the state space for each of the state variables
when using the Direct method of solution.

*)

Print["Setting up the Direct grids."];

(* % Create the grid points for x, s, and h   
   % by constructing a list of NumOfSteps points spaced evenly in 
   % utility between 0 and Log[max+1] and converting to levels
   % The NumOfSteps, the max's and min's are set in params.m 
*)

LogsGap = Log[smax/sDirectStart];
LogxGap = Log[xmax/xDirectStart];

lxDirectGrid  =Table[LoopOverxGrid,{LoopOverxGrid,Log[xDirectStart],Log[xmax],LogxGap/(NumOfxDirectSteps-1)}];
lsDirectGrid  =Table[LoopOversGrid,{LoopOversGrid,Log[sDirectStart],Log[smax],LogsGap/(NumOfsDirectSteps-1)}];

(* Convert the evenly spaced log numbers to level form *)
xDirectGrid = Exp[lxDirectGrid];
sDirectGrid = Exp[lsDirectGrid];

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

xDirectGrid = Prepend[xDirectGrid,xmin+FirstGridPointx];If[Max[etZeroProbVals]>0,xDirectGrid = Prepend[xDirectGrid,xmin+5*FirstGridPointx];(*xDirectGrid = Prepend[xDirectGrid,xmin+20*FirstGridPointx];xDirectGrid = Prepend[xDirectGrid,xmin+50*FirstGridPointx];xDirectGrid = Prepend[xDirectGrid,xmin+300*FirstGridPointx];*)];
sDirectGrid = Prepend[sDirectGrid,smin+FirstGridPoints];If[Max[etZeroProbVals]>0,sGrid = Prepend[sGrid,smin+5*FirstGridPoints];(*sGrid = Prepend[sGrid,smin+20*FirstGridPoints];*)];
*)

xDirectGrid = Union[Round[xDirectGrid*1000]/1000] //N;
sDirectGrid = Union[Round[sDirectGrid*1000]/1000] //N;

s0DirectGrid = Union[Round[1000*Prepend[sDirectGrid,smin]]/1000] //N;
x0DirectGrid = Union[Round[1000*Prepend[xDirectGrid,xmin]]/1000] //N;


xDirectGridBaseline = xDirectGrid;
x0DirectGridBaseline = x0DirectGrid;


hDirectGrid = hDirectGridBaseline = {.60,.75,.90,.975,1.,1.025,1.10,1.3,xmax};
h0DirectGrid = Prepend[hDirectGridBaseline,0];
(*hDirectGrid = Union[Round[Join[hDirectGridBaseline,xDirectGrid]*1000]/1000 //N];*)
hmin  = Min[hDirectGrid];
hmax  = Max[hDirectGrid];

If[catchup == 0,hDirectGrid = hDirectGridBaseline = {1.}];



RiskyShareDirectGrid = {0.};
If[NumOfRiskyShareDirectSteps>1,RiskyShareDirectGrid = Flatten[Table[Share,{Share,0,1,1/(NumOfRiskyShareDirectSteps-1)}]] //N;]

If[SolveForRiskyShare == False,RiskyShareDirectGrid = {0.}];

NumOfRiskyShareDirectSteps = Length[RiskyShareDirectGrid];
RiskyShareMin = 0; RiskyShareMax = 1;

Print["RiskyShareDirectGrid: "<>ToString[RiskyShareDirectGrid]];


NumOfhPointsDirect = Length[hDirectGrid];
Print["xDirectGrid: "<>ToString[xDirectGrid]];
Print["sDirectGrid: "<>ToString[sDirectGrid]];
Print["hDirectGrid: "<>ToString[hDirectGrid]];

