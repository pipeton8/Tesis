(*

This program constructs the values for the grids which will be used
to cover the state space for each of the state variables
when using the Matrices method of solution.

*)

Print["Setting up the Mat grids."];

(* % Create the grid points for x, s, and h   
   % by constructing a list of NumOfSteps points spaced evenly in 
   % utility between 0 and Log[max+1] and converting to levels
   % The NumOfSteps, the max's and min's are set in params.m 
*)

sGap = smax-sMatStart;
xGap = xmax-xMatStart;

xMatGrid  =Table[LoopOverxGrid //N,{LoopOverxGrid,xMatStart,xmax,xGap/(NumOfxMatSteps-1)}];
sMatGrid  =Table[LoopOversGrid //N,{LoopOversGrid,sMatStart,smax,sGap/(NumOfsMatSteps-1)}];

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

xMatGrid = Prepend[xMatGrid,xmin+FirstGridPointx];If[Max[etZeroProbVals]>0,xMatGrid = Prepend[xMatGrid,xmin+5*FirstGridPointx];(*xMatGrid = Prepend[xMatGrid,xmin+20*FirstGridPointx];xMatGrid = Prepend[xMatGrid,xmin+50*FirstGridPointx];xMatGrid = Prepend[xMatGrid,xmin+300*FirstGridPointx];*)];
sMatGrid = Prepend[sMatGrid,smin+FirstGridPoints];If[Max[etZeroProbVals]>0,sGrid = Prepend[sGrid,smin+5*FirstGridPoints];(*sGrid = Prepend[sGrid,smin+20*FirstGridPoints];*)];
*)

xMatGrid = Sort[Union[Round[xMatGrid*1000]/1000]] //N;
sMatGrid = Sort[Union[Round[sMatGrid*1000]/1000]] //N;


s0MatGrid = Union[Round[1000*Prepend[sMatGrid,smin]]/1000] //N;
x0MatGrid = Union[Round[1000*Prepend[xMatGrid,xmin]]/1000] //N;


xMatGridBaseline = xMatGrid;
x0MatGridBaseline = x0MatGrid;


hMatGrid = hMatGridBaseline = {.50,.65,.75,.80,.85,.90,.95,1.,1.05,1.10,1.15,xmax};
h0MatGrid = Prepend[hMatGridBaseline,0];
(*hMatGrid = Union[Round[Join[hMatGridBaseline,xMatGrid]*1000]/1000 //N];*)
hmin  = Min[hMatGrid];
hmax  = Max[hMatGrid];

If[catchup == 0,hMatGrid = hMatGridBaseline = {1.}];



RiskyShareMatGrid = {0.};
If[NumOfRiskyShareMatSteps>1,RiskyShareMatGrid = Flatten[Table[Share,{Share,0,1,1/(NumOfRiskyShareMatSteps-1)}]] //N;]

If[SolveForRiskyShare == False,RiskyShareMatGrid = {0.}];

NumOfRiskyShareMatSteps = Length[RiskyShareMatGrid];
RiskyShareMin = 0; RiskyShareMax = 1;

Print["RiskyShareMatGrid: "<>ToString[RiskyShareMatGrid]];


NumOfhPointsMat = Length[hMatGrid];
Print["xMatGrid: "<>ToString[xMatGrid]];
Print["sMatGrid: "<>ToString[sMatGrid]];
Print["hMatGrid: "<>ToString[hMatGrid]];


RList  = Union[Flatten[R]];
GList  = Union[Flatten[G]];
epList = Union[Flatten[epVals]];
RGapMatGrid = Flatten[
  Table[
    (RList[[i]]-Rcertain)/(GList[[j]] epList[[k]])
    ,{i,Length[RList]}
    ,{j,Length[GList]}
    ,{k,Length[epList]}]
  ];
  
RScaledMatGrid = 
Union[
 Round[
  Flatten[
   Table[
    (RList[[i]]*RiskyShareMatGrid[[l]]+Rcertain*(1-RiskyShareMatGrid[[l]]))
    /(GList[[j]] epList[[k]])
    ,{i,Length[RList]}
    ,{j,Length[GList]}
    ,{k,Length[epList]}
    ,{l,Length[RiskyShareMatGrid]}
   ]
  ]
  *50]/50 //N
];
