(*

Defines a set of parameter values to be used.

The string variable ProblemToSolveString must have been
defined before this program is run.

*)

{RepAgent,Risk,RiskHetero} = {2,3,4};


SetupParams[ProblemToSolveString_] := Block[{},
  dirparams;
  Get["params_"<>ProblemToSolveString<>".m"];
  dirprog;

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
  
  ListsExist = False;
];

LifePosn = LifeLength;

];

SetupParams[ProblemToSolveString];
