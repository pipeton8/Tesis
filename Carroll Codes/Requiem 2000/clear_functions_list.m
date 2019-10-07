(*
This file constructs a variety of data structures that need to be created
or maintained for every function that is created by the program 
*)

NamePosInFunctionList      = 1; (* Position of the name of the function in the list *)
ArgListPosInFunctionList   = 2; (* Position of the list of arguments that the function will take*)

(* sListFunctions is the list of functions that are constructed in order to solve the first order conditions *)

sListFunctions=   {{"FOCwrtRiskySharetList"          ,{"s0List" ,"hList" ,"AggState","EmpState","AggS","RiskyShareList"}}
                  ,{"RiskySharetList"                ,{"s0List" ,"hList" ,"AggState","EmpState","AggS"                 }}
                  ,{"OmegasInvtList"                 ,{"s0List" ,"hList" ,"AggState","EmpState","AggS"                 }}
                  ,{"OmegahInvtList"                 ,{"s0List" ,"hList" ,"AggState","EmpState","AggS"                 }}
                  ,{"OmegaInvtList"                  ,{"s0List" ,"hList" ,"AggState","EmpState","AggS"                 }}
                  ,{"CertEquivList"                  ,{"s0List" ,"hList" ,"AggState","EmpState","AggS"                 }}
                  ,{"EtLogctp1ListGtp1Givenst"       ,{"s0List" ,"hList", "AggState","EmpState","AggS"                 }}
                  ,{"EtLogctp1ListGtp1SquaredGivenst",{"s0List" ,"hList", "AggState","EmpState","AggS"                 }}                  
                  ,{"EtMPCPermList"                  ,{"s0List" ,"hList", "AggState","EmpState","AggS"                 }}                  
                  };

(* Positions of the various arguments in sListFunctions *)
{stInPosFOC,htInPosFOC,AggStateInPosFOC,EmpStateInPosFOC,RiskySharetInPosFOC} = {1,2,3,4,5};

xListFunctions = {{"VxInvtList"                  ,{"x0List","hList","AggState","EmpState","AggK"}}
                  ,{"VhInvtList"                 ,{"x0List","hList","AggState","EmpState","AggK"}}
                  ,{"VInvtList"                  ,{"x0List","hList","AggState","EmpState","AggK"}}
                  ,{"ctList"                     ,{"x0List","hList","AggState","EmpState","AggK"}}
                  ,{"stList"                     ,{"x0List","hList","AggState","EmpState","AggK"}}              
                  ,{"EtLogctp1ListGtp1Oct"       ,{"x0List","hList","AggState","EmpState","AggK"}}              
                  ,{"EtLogctp1ListGtp1OctSquared",{"x0List","hList","AggState","EmpState","AggK"}}              
                  };
                   
{xtInPos,htInPos,AggStateInPos,EmpStateInPos,AggKtInPos} = {1,2,3,4,5};

FunctionList  = 
  Join[sListFunctions,xListFunctions];     (* List of all functions to be created *)
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
    {{"RGapTimesVxtArgListDistGivenOmegatAllArg"
                       ,{"s0List","hList","AggState","EmpState","AggS","RiskyShareList"}}
    ,{"RScaledTimesVxtArgListDistGivenOmegatAllArg"
                       ,{"s0List","hList","AggState","EmpState","AggS","RiskyShareList"}}
    ,{"Vtp1ArgListDistGivenOmegatAllArg"
                       ,{"s0List","hList","AggState","EmpState","AggS","RiskyShareList"}}
    };

MakeArgArrays[ArgsForMatrices];

Omegatp1ArgGridDistGivenOmegatArgList = 
OmegatArgGridDistGivenVtArgListMatrix = 
Vtp1ArgGridDistGivenOmegatArgListMatrix = 
Vtp1ArgGridDistGivenVtArgList = 
 Table[0,{LifeLength}];
