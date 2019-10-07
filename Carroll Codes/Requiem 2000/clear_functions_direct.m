(*
This file constructs a variety of data structures that need to be created
or maintained for every function that is created by the program 
*)

NamePosInFunctionList      = 1; (* Position of the name of the function in the list *)
ArgListPosInFunctionList   = 2; (* Position of the list of arguments that the function will take*)

(* sDirectFunctions is the list of functions that are constructed in order to solve the first order conditions *)

sDirectFunctions= {{"FOCwrtRiskySharetDirect",{"s0Direct" ,"hDirect" ,"AggState","EmpState","RiskyShareDirect"}}
                  ,{"RiskySharetDirect"      ,{"s0Direct" ,"hDirect" ,"AggState","EmpState"             }}
                  ,{"OmegasInvtDirect"       ,{"s0Direct" ,"hDirect" ,"AggState","EmpState"             }}
                  ,{"OmegahInvtDirect"       ,{"s0Direct" ,"hDirect" ,"AggState","EmpState"             }}
                  ,{"OmegaInvtDirect"        ,{"s0Direct" ,"hDirect" ,"AggState","EmpState"             }}
                  ,{"CertEquivDirect"        ,{"s0Direct" ,"hDirect" ,"AggState","EmpState"             }}
                   };

(* Positions of the various arguments in sDirectFunctions *)
{stInPosFOC,htInPosFOC,AggStateInPosFOC,EmpStateInPosFOC,RiskySharetInPosFOC} = {1,2,3,4,5};

xDirectFunctions = {{"VxInvtDirect"      ,{"x0Direct","hDirect","AggState","EmpState","AggK"}}
                   ,{"VhInvtDirect"      ,{"x0Direct","hDirect","AggState","EmpState","AggK"}}
                   ,{"VInvtDirect"       ,{"x0Direct","hDirect","AggState","EmpState","AggK"}}
                   ,{"ctDirect"          ,{"x0Direct","hDirect","AggState","EmpState","AggK"}}
                   ,{"stDirect"          ,{"x0Direct","hDirect","AggState","EmpState","AggK"}}              
                   };
                   
{xtInPos,htInPos,AggStateInPos,EmpStateInPos,AggKtInPos} = {1,2,3,4,5};

FunctionList  = 
  Join[sDirectFunctions,xDirectFunctions];     (* List of all functions to be created *)
FunctionNames = 
  Transpose[FunctionList][[NamePosInFunctionList]]; (* Names of all functions *)

(* For every function, create a variable length LifePosn named FunctionNameInterpData *)
MakeNewVarByAppendingStringToName[FunctionNames,"InterpData"];

(* For every function, create a variable length LifePosn named FunctionNameInterpFunc *)
MakeNewVarByAppendingStringToName[FunctionNames,"InterpFunc"];

MakeListFunctions[FunctionList];       (* Create a function which maps f[{a,b,c}] to f[a,b,c] *)
MakeFunctionsFromInterp[FunctionList]; (* Create a function which automatically extracts values from interpolating functions *)
MakeArgArrays[FunctionList];           (* Make a variable which corresponds to an array of all permutations of the arguments to the function *)

