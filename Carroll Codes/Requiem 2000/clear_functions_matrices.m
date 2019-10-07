(*
This file constructs a variety of data structures that need to be created
or maintained for every function that is created by the program 
*)

NamePosInFunctionList      = 1; (* Position of the name of the function in the list *)
ArgListPosInFunctionList   = 2; (* Position of the list of arguments that the function will take*)

(* sMatFunctions is the list of functions that are constructed in order to solve the first order conditions *)

sMatFunctions= {{"FOCwrtRiskySharetMat",{"s0Mat" ,"hMat" ,"AggState","EmpState","RiskyShareMat"}}
                  ,{"RiskySharetMat"      ,{"s0Mat" ,"hMat" ,"AggState","EmpState"             }}
                  ,{"OmegasInvtMat"       ,{"s0Mat" ,"hMat" ,"AggState","EmpState"             }}
                  ,{"OmegahInvtMat"       ,{"s0Mat" ,"hMat" ,"AggState","EmpState"             }}
                  ,{"OmegaInvtMat"        ,{"s0Mat" ,"hMat" ,"AggState","EmpState"             }}
                  ,{"CertEquivMat"        ,{"s0Mat" ,"hMat" ,"AggState","EmpState"             }}
                   };

(* Positions of the various arguments in sMatFunctions *)
{stInPosFOC,htInPosFOC,AggStateInPosFOC,EmpStateInPosFOC,RiskySharetInPosFOC} = {1,2,3,4,5};

xMatFunctions = {{"VxInvtMat"      ,{"x0Mat","hMat","AggState","EmpState","AggK"}}
                   ,{"VhInvtMat"      ,{"x0Mat","hMat","AggState","EmpState","AggK"}}
                   ,{"VInvtMat"       ,{"x0Mat","hMat","AggState","EmpState","AggK"}}
                   ,{"ctMat"          ,{"x0Mat","hMat","AggState","EmpState","AggK"}}
                   ,{"stMat"          ,{"x0Mat","hMat","AggState","EmpState","AggK"}}              
                   };
                   
{xtInPos,htInPos,AggStateInPos,EmpStateInPos,AggKtInPos} = {1,2,3,4,5};

FunctionList  = 
  Join[sMatFunctions,xMatFunctions];     (* List of all functions to be created *)
FunctionNames = 
  Transpose[FunctionList][[NamePosInFunctionList]]; (* Names of all functions *)

(* For every function, create a variable length LifePosn named FunctionNameInterpData *)
MakeNewVarByAppendingStringToName[FunctionNames,"InterpData"];

(* For every function, create a variable length LifePosn named FunctionNameInterpFunc *)
MakeNewVarByAppendingStringToName[FunctionNames,"InterpFunc"];

MakeListFunctions[FunctionList];       (* Create a function which maps f[{a,b,c}] to f[a,b,c] *)
MakeFunctionsFromInterp[FunctionList]; (* Create a function which automatically extracts values from interpolating functions *)
MakeArgArrays[FunctionList];           (* Make a variable which corresponds to an array of all permutations of the arguments to the function *)

ArgsForMatrices = 
    {{"RGapTimesVxt"   ,{"x0Mat","hMat","AggState","EmpState","AggK","RGapMat"}}
    ,{"RScaledTimesVxt",{"x0Mat","hMat","AggState","EmpState","AggK","RScaledMat"}}
    };

MakeArgArrays[ArgsForMatrices];

RScaledTimesVxtArgGridDistGivenOmegatArgMatrix =
VxtArray = 
VxtVector = 
Omegatp1ArgGridDistGivenOmegatArgMat = 
OmegatArgGridDistGivenVtArgMatMatrix = 
Vtp1ArgGridDistGivenOmegatArgMatMatrix = 
Vtp1ArgGridDistGivenVtArgMat = 
VhtArray = 
VhtVector = 
RScaledTimesVxtArray = 
RScaledTimesVxtVector = 
RiskySharetGivenOmegatArgArray = 
RiskySharetGivenOmegatArgMatrix = 
RGapTimesVxtArray = 
RGapTimesVxtVector = Table[0,{LifeLength}];
