(*

This file prepares the Mathematica environment for
either solving for or loading of an optimal decision rule 

*)

<<setup_workspace.m;

programs = Directory[];

dirfigs := SetDirectory[programs<>SubDirectoryString<>"Figures"];

dirprog := SetDirectory[programs];

dirparams := SetDirectory[programs<>SubDirectoryString<>"params"];

dirout := SetDirectory[programs<>SubDirectoryString<>"results"];

<<params.m;

<<vars_to_clear.m;

<<clear_times.m;

<<iteration_routines.m;

<<func_defs.m;

<<impatience.m;

ConstructValueFunctions = False;           (* Only need to construct value functions if discontinuous choices are made *)
DirectSolve   = False;                     (* DirectSolve is the simplest method, and also slow *)
MatricesSolve = False;MatricesExist=False; (* MatricesSolve constructs transition matrices and solves using them *)
ListSolve     = True;ListsExist    =False; (* ListSolve constructs the lists of arguments once, then reuses them *)
  TrimmingIncrement = .00001;CutoffProb = 0;(* A large value of TrimmingIncrement merges 'close' values of arguments to reduce list size *)
VerboseOutput = False;
ListPlusMatricesSolve = False;

If[DirectSolve == True,
  <<construct_grids_direct.m;
  <<clear_functions_direct.m;
  <<func_defs_direct.m;
];

If[MatricesSolve == True,
  <<construct_grids_matrices.m;
  <<clear_functions_matrices.m;
  <<func_defs_matrices.m;
];

If[ListSolve == True,
  <<construct_grids_list.m;
  <<clear_functions_list.m;
  <<func_defs_list.m;
  If[ListPlusMatricesSolve == True && MatricesSolve == False,
      <<construct_grids_matrices.m;
      <<clear_functions_matrices.m;
      <<func_defs_matrices.m;
  ];
];

LifePosn = LifeLength;

	
