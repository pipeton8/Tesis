
(* Clear out largest data structures that will not be needed for simulations *)


<<simulation_matrices_routines.m;

Print["Making sTransMatrixByAggState."];
sTransMatrixByAggStateMake[Verbose];

Print["Making xTransMatrixByAggState."];
xTransMatrixByAggStateMake[Verbose];

(*  Old method of finding transitions. *)
(*
Print["Making SelectsAggTransitions."];
SelectsAggTransitions = Table[
  DiagonalMatrix[
    Flatten[    
	  Table[
	    Table[
          If[Mod[i-LoopOverAggStates,Length[AggStateGrid]] == 0,1,0]
        ,{j,Length[EmpStateGrid]}]
      ,{i,NumOfsGridStates/Length[EmpStateGrid]}
	  ]
    ]
  ]
  ,{LoopOverAggStates,Length[AggStateGrid]}
];

Print["Constructing sTransMatrixByAggState."];

sTransMatrixByAggState = Table[
  SelectsAggTransitions[[LoopOverResult]] 
  . sTransMatrix 
  . SelectsAggTransitions[[LoopOverSource]]
  / AggStateProb[LoopOverSource,LoopOverResult]
  ,{LoopOverSource,Length[AggStateGrid]}
  ,{LoopOverResult,Length[AggStateGrid]}];

Remove[SelectsAggTransitions];


Print["Constructing xTransMatrixByAggState."];


SelectxAggTransitions = Table[
  DiagonalMatrix[
    Flatten[    
	  Table[
	    Table[
          If[Mod[i-LoopOverAggStates,Length[AggStateGrid]] == 0,1,0]
        ,{j,Length[EmpStateGrid]*Length[AggKGrid]}]
      ,{i,NumOfxGridStates/(Length[EmpStateGrid]*Length[AggKGrid])}
	  ]
    ]
  ]
  ,{LoopOverAggStates,Length[AggStateGrid]}
];
  
xTransMatrixByAggState = Table[
  SelectxAggTransitions[[LoopOverResult]] 
  . xTransMatrix 
  . SelectxAggTransitions[[LoopOverSource]]
  / AggStateProb[LoopOverSource,LoopOverResult]
  ,{LoopOverSource,Length[AggStateGrid]}
  ,{LoopOverResult,Length[AggStateGrid]}];

Remove[SelectxAggTransitions];

*)


(* Even older code 
sTransMatrixByAggStateNew = 
Table[
  NewMatrix = sTransMatrix;
  Do[
    ColIndex = 
    1+(LoopOverColEmpStates-1)+(LoopOverColAggStates-1)*
        Length[EmpStateGrid]+(LoopOverColhStates-1)*Length[EmpStateGrid]*
        Length[AggStateGrid]+(LoopOverColsStates-1)*Length[AggStateGrid]*
        Length[EmpStateGrid]*Length[hMatGrid];
    RowIndex = 
    1+(LoopOverRowEmpStates-1)+(LoopOverRowAggStates-1)*
        Length[EmpStateGrid]+(LoopOverRowhStates-1)*Length[EmpStateGrid]*
        Length[AggStateGrid]+(LoopOverRowsStates-1)*Length[AggStateGrid]*
        Length[EmpStateGrid]*Length[hMatGrid];
    If[LoopOverColAggStates == AggStateFrom,
      (* then *)
      NewMatrix[[RowIndex,ColIndex]] = 
      sTransMatrix[[RowIndex,ColIndex]]/
        AggStateProb[AggStateFrom,AggStateTo],
      (* else *)
		NewMatrix[[RowIndex,ColIndex]] = 0.];
    If[LoopOverRowAggStates != AggStateTo,NewMatrix[[RowIndex,ColIndex]] = 0.];
,{LoopOverRowsStates,Length[s0MatGrid]}
,{LoopOverRowhStates,Length[hMatGrid]}
,{LoopOverRowAggStates,Length[AggStateGrid]}
,{LoopOverRowEmpStates,Length[EmpStateGrid]}
,{LoopOverColsStates,Length[s0MatGrid]}
,{LoopOverColhStates,Length[hMatGrid]}
,{LoopOverColAggStates,Length[AggStateGrid]}
,{LoopOverColEmpStates,Length[EmpStateGrid]}
];
NewMatrix
,{AggStateFrom,Length[AggStateGrid]}
,{AggStateTo,Length[AggStateGrid]}
];

*)