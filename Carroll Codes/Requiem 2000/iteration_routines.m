(* 
The routines in this file define the iteration procedure for
finding the converged solution.  SolvePeriods[NumOfPeriods_] starts
at the last period and solves backwards.  KeepSolvingPeriods[NumOfPeriods_]
starts wherever the current LifePosn is and continues solving from there.


% KeepSolvingPeriods[] assumes that there is a partial solution 
% already resident in memory, and continues solving from the 
% current LifePosn for NumOfPeriods
*)

Clear[SolvePeriods];
SolvePeriods[NumOfPeriods_] := Block[{},

<<last_period.m;		           (* % Solve for the last period of life *)
<<impatience.m;
EarliestVtSolved = LifePosn;


KeepSolvingPeriods[NumOfPeriods-1] (* % Then keep solving for (NumOfPeriods-1) periods *)
];


ClearAll[KeepSolvingPeriods];
KeepSolvingPeriods[NumOfPeriods_] := Block[{},
Do[
LifePosn--;  			                      (* % Decrement the period of life *)
Print["Solving Period "<>ToString[LifePosn]];

If[(MatricesSolve == True || ListPlusMatricesSolve) && MatricesExist != True,TimeForMatricesSetup += Timing[<<setup_matrices.m;]];
If[ListSolve      == True && ListsExist    != True,TimeForListsSetup += Timing[<<setup_list.m;]];

(* Calculate the value of the FOC wrt the risky share for the set of gridpoints *) 

(* Commented out because I'm not sure it is faster to create this thing 
than not to; To reverse this you have to change the call in 
RiskySharetDirectRaw to dump the Raw on the call of FOCwrtRiskySharet *) 

If[DirectSolve == True,
  TimeForFOCwrtRiskySharetDirect += Timing[MakeInterpGivenFunctionName["FOCwrtRiskySharetDirect",InterpOrder]];
  ];

If[MatricesSolve == True,
  If[VerboseOutput == True, Print["Creating FOCwrtRiskySharetMat."]];
  TimeForFOCwrtRiskySharetMat += Timing[
    FOCwrtRiskySharetMatResults = FOCwrtRiskySharetMatArgGridDistGivenOmegatAllArgMatrix 
                     . RGapTimesVxtVector[[LifePosn+1]];
    FOCwrtRiskySharetMatInterpData[[LifePosn]] = AddOutcome[FOCwrtRiskySharetMatArgArray,FOCwrtRiskySharetMatResults];
    FOCwrtRiskySharetMatInterpFunc[[LifePosn]] = Interpolation[FOCwrtRiskySharetMatInterpData[[LifePosn]],InterpolationOrder->InterpOrder];
 ]  (* End Timing *)
];

If[ListSolve == True,
  If[VerboseOutput == True, Print["Creating FOCwrtRiskySharetList."]];
  TimeForFOCwrtRiskySharetList += Timing[
   If[SolveForRiskyShare == True,
    FOCwrtRiskySharetListResults =
     Flatten[
      Table[
        {ArgList,ProbList} =
        RGapTimesVxtArgListDistGivenOmegatAllArgArray[[i,j,k,l,m]];
         Map[ #[[RGaptp1Pos]]*(#[[GNFactortp1]]^(rho gamma - gamma - rho))
           (
           VxtList[#[[xtp1Pos]],#[[htp1StartPos]],#[[AggStatetp1Pos]],#[[EmpStatetp1Pos]],#[[AggKtp1Pos]],LifePosn+1]
           ) &
          ,ArgList] . ProbList
        ,{i,Length[s0ListGrid]}
        ,{j,Length[hListGrid]}
        ,{k,Length[AggStateGrid]}
        ,{l,Length[EmpStateGrid]}
        ,{m,Length[AggSGrid]}
        ,{n,Length[RiskyShareListGrid]}]
     ];  (* End Flatten *)
  FOCwrtRiskySharetListInterpData[[LifePosn]] = AddOutcome[FOCwrtRiskySharetListArgArray,FOCwrtRiskySharetListResults];
  FOCwrtRiskySharetListInterpFunc[[LifePosn]] = Interpolation[FOCwrtRiskySharetListInterpData[[LifePosn]],InterpolationOrder->InterpOrder];
  ]; (* End If SolveForRiskyShare; if it is false, nothing is done *)
 ]; (* End Timing *)
];
    
EarliestOmegatSolved=LifePosn;  (* Update variable to signal that we have constructed the expectations for next period *)

If[DirectSolve == True,
  TimeForRiskySharetDirect += Timing[MakeInterpGivenFunctionName["RiskySharetDirect",InterpOrder]];
  TimeForOmegasInvtDirect  += Timing[MakeInterpGivenFunctionName["OmegasInvtDirect" ,InterpOrder]];
];
  
If[MatricesSolve == True,
  TimeForRiskySharetMat += Timing[MakeInterpGivenFunctionName["RiskySharetMat",InterpOrder]];
  TimeForOmegasInvtMat  += Timing[
    Print["Constructing Transition Matrices."];
    If[SolveForRiskyShare == False && LifePosn<LifeLength-1, (* If not solving for risky share, trans mats are same for all periods *)
      (* then *) Vtp1ArgGridDistGivenOmegatArgMatMatrix[[LifePosn]]         = Vtp1ArgGridDistGivenOmegatArgMatMatrix[[LifePosn+1]];
                 RScaledTimesVxtArgGridDistGivenOmegatArgMatrix[[LifePosn]] = RScaledTimesVxtArgGridDistGivenOmegatArgMatrix[[LifePosn+1]],
      (* else *)
        Vtp1ArgGridDistGivenOmegatArgMatMatrix[[LifePosn]] = 
        Transpose[
         Partition[
          Flatten[
            Table[
            Flatten[
              Vtp1ArgGridDistGivenOmegatAllArgArray[[i,j,k,l,
                  ClosestElementTo[RiskyShareMatGrid,
                    RiskySharetMat[s0MatGrid[[i]],hMatGrid[[j]],k,l]]
            ]]]
            ,{i,Length[s0MatGrid]}
            ,{j,Length[hMatGrid]}
            ,{k,Length[AggStateGrid]}
            ,{l,Length[EmpStateGrid]}]
            ]
          ,Length[x0MatGrid]*Length[hMatGrid]*Length[AggStateGrid]*Length[EmpStateGrid]*Length[AggSGrid]
        ]];
        
        Print["Constructing RScaledTimesVxtArgGridDistGivenOmegatArgMatrix."];
        RScaledTimesVxtArgGridDistGivenOmegatArgMatrix[[LifePosn]] = 
        Transpose[
         Partition[
          Flatten[
            Table[
            Flatten[
              RScaledTimesVxtArgGridDistGivenOmegatAllArgArray[[i,j,k,l,
                  ClosestElementTo[RiskyShareMatGrid,
                    RiskySharetMat[s0MatGrid[[i]],hMatGrid[[j]],k,l]]
            ]]]
            ,{i,Length[s0MatGrid]}
            ,{j,Length[hMatGrid]}
            ,{k,Length[AggStateGrid]}
            ,{l,Length[EmpStateGrid]}]
            ]
          ,Length[x0MatGrid]*Length[hMatGrid]*Length[AggStateGrid]*Length[EmpStateGrid]*Length[AggSGrid]*Length[RScaledMatGrid]
        ]];
      ]; (* End If SolveForRiskyShare *)
      
    Print["Creating OmegasInvtMat."];
    OmegastMatResults    = RScaledTimesVxtVector[[LifePosn+1]] . RScaledTimesVxtArgGridDistGivenOmegatArgMatrix[[LifePosn]];
    OmegasInvtMatResults = (OmegastToOmegasInvtMultiply*OmegastMatResults)^OmegastToOmegasInvtPower;

    OmegasInvtMatInterpData[[LifePosn]] = AddOutcome[OmegasInvtMatArgArray,OmegasInvtMatResults];
    OmegasInvtMatInterpFunc[[LifePosn]] = Interpolation[OmegasInvtMatInterpData[[LifePosn]],InterpolationOrder->1];  

    If[LifePosn < LifeLength-1, 
      (* then it makes sense to talk about s transitions *)
      Omegatp1ArgGridDistGivenOmegatArgMat[[LifePosn]] = OmegatArgGridDistGivenVtArgMatMatrix[[LifePosn+1]] 
                                                . Vtp1ArgGridDistGivenOmegatArgMatMatrix[[LifePosn]];
      ]; (* end if *)

  ]; (* end Timing *)
]; (* End If MatricesSolve == True *)

If[ListSolve == True,
    TimeForRiskySharetList += Timing[MakeInterpGivenFunctionName["RiskySharetList",InterpOrder]];
    TimeForOmegasInvtList  += Timing[
      If[VerboseOutput == True,Print["Creating OmegastList."]];

      OmegastListResults = 
        Flatten[Table[{ArgList,ProbList} =
          RScaledTimesVxtArgListDistGivenOmegatAllArgArray[[i,j,k,l,m,
              ClosestElementTo[RiskyShareListGrid,
                RiskySharetList[s0ListGrid[[i]],hListGrid[[j]],k,l,AggSGrid[[m]]]]
                ]];
          Map[
            (
            #[[RScaledtp1Pos]] #[[GNFactortp1Pos]]^(1+rho gamma - rho - gamma)*
            VxtList[
                #[[xtp1Pos]],
                #[[htp1StartPos]],
                #[[AggStatetp1Pos]],
                #[[EmpStatetp1Pos]],
                #[[AggKtp1Pos]],
                LifePosn+1
                ]
            )  &,ArgList] . ProbList
        ,{i,Length[s0ListGrid]}
        ,{j,Length[hListGrid]}
        ,{k,Length[AggStateGrid]}
        ,{l,Length[EmpStateGrid]}
        ,{m,Length[AggSGrid]}]
        ]; (* End Flatten *)
    ];   (* End Timing *)

    OmegasInvtListResults = (OmegastToOmegasInvtMultiply*OmegastListResults)^OmegastToOmegasInvtPower;
    OmegasInvtListInterpData[[LifePosn]] = AddOutcome[OmegasInvtListArgArray,OmegasInvtListResults];
    OmegasInvtListInterpFunc[[LifePosn]] = Interpolation[OmegasInvtListInterpData[[LifePosn]],InterpolationOrder->1];  

    If[LifePosn < LifeLength-1 && ListPlusMatricesSolve == True,
       If[VerboseOutput==True,Print["Making OmegaArg Transition Matrices."]];
       If[VerboseOutput==True,Print["Making Vtp1ArgGridDistGivenOmegatArgListMatrix."]];     
       Vtp1ArgGridDistGivenOmegatArgListMatrix[[LifePosn]] = 
        Transpose[
         Partition[
          Flatten[
           Table[
            Flatten[
            Vtp1ArgGridDistGivenOmegatAllArgArray[[i,j,k,l,m,
                 ClosestElementTo[RiskyShareMatGrid,
                   RiskySharetList[s0MatGrid[[i]],hMatGrid[[j]],k,l,m]]
          ]]]
        ,{i,Length[s0MatGrid]}
        ,{j,Length[hMatGrid]}
        ,{k,Length[AggStateGrid]}
        ,{l,Length[EmpStateGrid]}
        ,{m,Length[AggSGrid]}]
        ]
       ,Length[x0MatGrid]*Length[hMatGrid]*Length[AggStateGrid]*Length[EmpStateGrid]*Length[AggSGrid]
       ]];
     ]; (* End If LifePosn<LifeLength-1 *)
     
     If[FancyStuff == True,
     
       If[VerboseOutput==True,Print["Making EtLogctp1ListGtp1Givenst."]];          
       EtLogctp1ListGtp1GivenstResults = 
         Flatten[
          Table[
           {ArgList,ProbList} =
           Vtp1ArgListDistGivenOmegatAllArgArray[[i,j,k,l,m,
               ClosestElementTo[RiskyShareListGrid,RiskySharetList[s0ListGrid[[i]],hListGrid[[j]],k,l,m]]
               ]];
           Map[
             Log[
                 #[[GNFactortp1Pos]]
                 *ctListInterpFunc[[LifePosn+1]][#[[xtp1Pos]],#[[htp1StartPos]],#[[AggStatetp1Pos]],#[[EmpStatetp1Pos]],#[[AggKtp1Pos]]]]
             &,ArgList] . ProbList
         ,{i,Length[s0ListGrid]}
         ,{j,Length[hListGrid]}
         ,{k,Length[AggStateGrid]}
         ,{l,Length[EmpStateGrid]}
         ,{m,Length[AggKGrid]}]
         ]; (* End Flatten *)
        
       EtLogctp1ListGtp1GivenstInterpData[[LifePosn]] = AddOutcome[EtLogctp1ListGtp1GivenstArgArray,EtLogctp1ListGtp1GivenstResults];
       EtLogctp1ListGtp1GivenstInterpFunc[[LifePosn]] = Interpolation[EtLogctp1ListGtp1GivenstInterpData[[LifePosn]],InterpolationOrder->1];  
    
       If[VerboseOutput==True,Print["Making EtMPCPermListResults."]];

       EtMPCPermListResults = 
         Flatten[
          Table[
           {ArgList,ProbList} =
           RScaledTimesVxtArgListDistGivenOmegatAllArgArray[[i,j,k,l,m,
               ClosestElementTo[RiskyShareListGrid,RiskySharetList[s0ListGrid[[i]],hListGrid[[j]],k,l,m]]
               ]];
           Map[
               (ctListInterpFunc[[LifePosn+1]][#[[xtp1Pos]],#[[htp1StartPos]],#[[AggStatetp1Pos]],#[[EmpStatetp1Pos]],#[[AggKtp1Pos]]]                (*  c_{t+1}             *)
                -MPCList[#[[xtp1Pos]],#[[htp1StartPos]],#[[AggStatetp1Pos]],#[[EmpStatetp1Pos]],#[[AggKtp1Pos]],LifePosn+1]*#[[RScaledtp1Pos]]*s0ListGrid[[i]]   (* -c^x_{t+1}(R/GN)st   *)
                -MPHList[#[[xtp1Pos]],#[[htp1StartPos]],#[[AggStatetp1Pos]],#[[EmpStatetp1Pos]],#[[AggKtp1Pos]],LifePosn+1]*hListGrid[[j]]/#[[GNFactortp1Pos]]   
               )
               &,ArgList] . ProbList
         ,{i,Length[s0ListGrid]}
         ,{j,Length[hListGrid]}
         ,{k,Length[AggStateGrid]}
         ,{l,Length[EmpStateGrid]}
         ,{m,Length[AggKGrid]}]
         ]; (* End Flatten *)

       EtMPCPermListInterpData[[LifePosn]] = AddOutcome[EtMPCPermListArgArray,EtMPCPermListResults];
       EtMPCPermListInterpFunc[[LifePosn]] = Interpolation[EtMPCPermListInterpData[[LifePosn]],InterpolationOrder->1];  
     
       If[VerboseOutput==True,Print["Making EtLogctp1ListGtp1SquaredGivenstResults."]];          
    
       EtLogctp1ListGtp1SquaredGivenstResults = 
         Flatten[
          Table[
           {ArgList,ProbList} =
           Vtp1ArgListDistGivenOmegatAllArgArray[[i,j,k,l,m,
               ClosestElementTo[RiskyShareListGrid,RiskySharetList[s0ListGrid[[i]],hListGrid[[j]],k,l,m]]
               ]];
           Map[
             (Log[#[[GNFactortp1Pos]]*ctListInterpFunc[[LifePosn+1]][#[[xtp1Pos]],#[[htp1StartPos]],#[[AggStatetp1Pos]],#[[EmpStatetp1Pos]],#[[AggKtp1Pos]]]])^2
             &,ArgList] . ProbList
         ,{i,Length[s0ListGrid]}
         ,{j,Length[hListGrid]}
         ,{k,Length[AggStateGrid]}
         ,{l,Length[EmpStateGrid]}
         ,{m,Length[AggKGrid]}]
         ]; (* End Flatten *)
       
       EtLogctp1ListGtp1SquaredGivenstInterpData[[LifePosn]] = AddOutcome[EtLogctp1ListGtp1GivenstArgArray,EtLogctp1ListGtp1SquaredGivenstResults];
       EtLogctp1ListGtp1SquaredGivenstInterpFunc[[LifePosn]] = Interpolation[EtLogctp1ListGtp1SquaredGivenstInterpData[[LifePosn]],InterpolationOrder->1];  
       
       
      If[LifePosn < LifeLength-1 && Length[Dimensions[OmegatArgGridDistGivenVtArgListMatrix[[LifePosn+1]]]]>0 && ListPlusMatricesSolve == True, 
        (* then it makes sense to talk about s transitions *)
        If[VerboseOutput==True,Print["Making Omegat Transition Matrix."]];          
        Omegatp1ArgGridDistGivenOmegatArgList[[LifePosn]] = OmegatArgGridDistGivenVtArgListMatrix[[LifePosn+1]] 
                                                          . Vtp1ArgGridDistGivenOmegatArgListMatrix[[LifePosn]];
      ]; (* end If LifePosn < LifeLength-1 *)
  ];     (* end If FancyStuff *)  
]; (* End If ListSolve *)

If[HabitsMatter == True,
  If[MatricesSolve == True,
    Print["Creating OmegahInvtMat."];
    OmegahtMatResults  = VhtVector[[LifePosn+1]] 
                       . Vtp1ArgGridDistGivenOmegatArgMatMatrix[[LifePosn]];
                       
    OmegahInvtMatResults = (OmegahtToOmegahInvtMultiply*OmegahtMatResults)^OmegahtToOmegahInvtPower;

    OmegahInvtMatInterpData[[LifePosn]] = AddOutcome[OmegahInvtMatArgArray,OmegahInvtMatResults];
    OmegahInvtMatInterpFunc[[LifePosn]] = Interpolation[OmegahInvtMatInterpData[[LifePosn]],InterpolationOrder->1];  
  ]; (* End If Solutionmethod *)
  
  If[DirectSolve == True,
    TimeForOmegahInvtDirect     += Timing[MakeInterpGivenFunctionName["OmegahInvtDirect"    ,InterpOrder]];
  ];

  If[ListSolve == True,
      If[VerboseOutput == True,Print["Creating OmegahInvtList."]];
      OmegahtListResults = 
        Flatten[Table[{ArgList,ProbList} =
          Vtp1ArgListDistGivenOmegatAllArgArray[[i,j,k,l,m,
              ClosestElementTo[RiskyShareListGrid,
                RiskySharetList[s0ListGrid[[i]],hListGrid[[j]],k,l,m]]
                ]];
          Map[
            VhtList[#[[xtp1Pos]],#[[htp1StartPos]],#[[AggStatetp1Pos]],#[[EmpStatetp1Pos]],#[[AggKtp1Pos]],LifePosn+1] &,ArgList] 
            . ProbList
        ,{i,Length[s0ListGrid]}
        ,{j,Length[hListGrid]}
        ,{k,Length[AggStateGrid]}
        ,{l,Length[EmpStateGrid]}
        ,{m,Length[AggSGrid]}]
        ]; (* End Flatten *)

    OmegahInvtListResults = (OmegahtToOmegahInvtMultiply*OmegahtListResults)^OmegahtToOmegahInvtPower;
    OmegahInvtListInterpData[[LifePosn]] = AddOutcome[OmegahInvtListArgArray,OmegahInvtListResults];
    OmegahInvtListInterpFunc[[LifePosn]] = Interpolation[OmegahInvtListInterpData[[LifePosn]],InterpolationOrder->1];  

  ];
]; (* End If HabitsMatter *)

If[DirectSolve == True,
  TimeForCertEquivDirect+= Timing[MakeInterpGivenFunctionName["CertEquivDirect",InterpOrder]]
  ];

If[MatricesSolve == True,
  TimeForCertEquivMat   += Timing[MakeInterpGivenFunctionName["CertEquivMat"  ,InterpOrder]];
  ];

If[ListSolve == True,
  TimeForCertEquivList  += Timing[MakeInterpGivenFunctionName["CertEquivList"  ,InterpOrder]];
  ];

If[ConstructValueFunctions,
  TimeForOmegaInvt      += Timing[MakeInterpGivenFunctionName["OmegaInvt"     ,InterpOrder]]];

(*
(* 
Now find the points at which the liquidity constraint becomes binding
and augment the x0DirectGrid to contain those points
*)

If[LifePosn < LifeLength && HabitsMatter,
  If[Max[etZeroProbVals] > 0,SavToMatch = FirstGridPoints,SavToMatch=0];  (* If not liquidity constrained, augment grid close to zero saving *)
  hCenter=hDirectGrid[[Round[Length[hDirectGrid]/2]]];
  hList = {Min[hDirectGrid],hCenter,Max[hDirectGrid]};
(*AggSGrid = Union[Flatten[epVals[[LifeLength]]]];
  If[Length[AggKGrid]<3,epList= {AggKGrid[[1]]},epList={Min[AggKGrid],Max[AggKGrid]}];
*)
  BindingPoints = Flatten[
  Table[
    (x /. FindRoot[
       x==(((G[[LifePosn,1,1]] epList[[LoopOverepVals]])/
          hList[[LoopOverhVals]])^(gamma rho - gamma)*
          beta[[LifePosn+1,EmpState]])^(-1/rho)*
          CertEquiv[SavToMatch,
            (hList[[LoopOverhVals]]/(G[[LifePosn,1,1]] epList[[LoopOverepVals]]))*
            (1-catchup)+catchup x,1,1,LifePosn]
       ,{x,{.01,.99 hList[[LoopOverhVals]]}}])
  ,{LoopOverhVals,Length[hList]}
  ,{LoopOverepVals,Length[epList]}]
  ];
  BindingPoints = Union[Round[BindingPoints*10000]/10000] //N;
  x0DirectGrid = Union[BindingPoints,x0DirectGridBaseline];  (* Add the point to the gridpoints *)
  xGrid  = Union[BindingPoints,xGridBaseline]; 
  MakeArgArrays[xFunctions];                (* Reconstruct the ArgArrays *)
];

If[StageOfLife > LastElementLessThan[StartOfLifeStage,LifePosn],
  (* then we have moved into a new stage of life *)
    StageOfLife--;
(*    AggKGrid = Union[Flatten[epVals[[LifePosn]]]];*)
    MakeArgArrays[xFunctions];       (* When we change the set of possible values for AggK, need to redo matrices *)
];
*)

If[DirectSolve == True,
  TimeForctDirect     += Timing[MakeInterpGivenFunctionName["ctDirect"    ,InterpOrder]];
  TimeForVxInvtDirect += Timing[MakeInterpGivenFunctionName["VxInvtDirect",InterpOrder]];
  If[HabitsMatter==True,
    TimeForVhInvtDirect += Timing[MakeInterpGivenFunctionName["VhInvtDirect",InterpOrder]];
  ];
];

If[MatricesSolve == True,
  TimeForctMat  += Timing[MakeInterpGivenFunctionName["ctMat" ,InterpOrder]];
  
  stMatInterpData[[LifePosn]] = 
      AddOutcome[
        stMatArgArray,
        Transpose[ctMatInterpData[[LifePosn]]][[1]]-Transpose[ctMatInterpData[[LifePosn]]][[-1]]
      ];
  stMatInterpFunc[[LifePosn]] = Interpolation[stMatInterpData[[LifePosn]],InterpolationOrder->InterpOrder];
  
  OmegatArgGridDistGivenVtArgMatMatrix[[LifePosn]] = Transpose[Map[Flatten[Apply[OmegatArgGridDistGivenVtArgMat,#]] &,VxInvtMatArgArray]];

  Vtp1ArgGridDistGivenVtArgMat[[LifePosn]] = Vtp1ArgGridDistGivenOmegatArgMatMatrix[[LifePosn]] 
                                    . OmegatArgGridDistGivenVtArgMatMatrix[[LifePosn]] ;

  TimeForVxInvtMat += Timing[MakeInterpGivenFunctionName["VxInvtMat",InterpOrder]];
  If[HabitsMatter == True,
    (* then *) TimeForVhInvtMat += Timing[MakeInterpGivenFunctionName["VhInvtMat",InterpOrder]]
    ];   (* End If HabitsMatter *)
];

If[ListSolve == True,
  TimeForctList     += Timing[MakeInterpGivenFunctionName["ctList"    ,InterpOrder]];
  TimeForVxInvtList += Timing[MakeInterpGivenFunctionName["VxInvtList",InterpOrder]];
  TimeForVhInvtList += Timing[MakeInterpGivenFunctionName["VhInvtList",InterpOrder]];

  If[ListPlusMatricesSolve == True,
    If[VerboseOutput == True,Print["Creating OmegatArgGridDistGivenVtArgListMatrix."]];  
    OmegatArgGridDistGivenVtArgListMatrix[[LifePosn]] = 
      Transpose[
        Map[
          Flatten[
            Apply[OmegatArgGridDistGivenVtArgList,#]] &
          ,VxInvtMatArgArray]
      ];

    Vtp1ArgGridDistGivenVtArgList[[LifePosn]] = 
        Vtp1ArgGridDistGivenOmegatArgListMatrix[[LifePosn]] 
        . OmegatArgGridDistGivenVtArgListMatrix[[LifePosn]];
        
        
  ]; (* End If ListPlusMatricesSolve *)
];   (* End If ListSolve *)


If[ConstructValueFunctions,
  TimeForVInvt+= Timing[MakeInterpGivenFunctionName["VInvt",InterpOrder]]];

EarliestVtSolved = LifePosn;

If[MatricesSolve == True,
  TimeForArrayArgs += Timing[
    VxtVector[[LifePosn]] = ((Transpose[VxInvtMatInterpData[[LifePosn]]][[-1]])^(1/VxtToVxInvtPower))/VxtToVxInvtMultiply;
    If[HabitsMatter == True,
      (* then *) VhtVector[[LifePosn]] = ((Transpose[VhInvtMatInterpData[[LifePosn]]][[-1]])^(1/VhtToVhInvtPower))/VhtToVhInvtMultiply;
      ]; (* End If HabitsMatter *)    
    RGapTimesVxtVector[[LifePosn]] = Flatten[Table[RGapMatGrid*VxtVector[[LifePosn,i]],{i,Length[VxtVector[[LifePosn]]]}]];
    RScaledTimesVxtVector[[LifePosn]] = Flatten[Table[RScaledMatGrid*VxtVector[[LifePosn,i]],{i,Length[VxtVector[[LifePosn]]]}]];
  ];
]; (* End If MatricesSolve *)

If[LowMem == True && Mod[LifePosn+2,KeepInterval] != 0 && LifePosn<LifeLength-3,
  Do[
  ToExpression[ListOfNamesToClear[[i]]<>"[[LifePosn+2]] = {};"];
  ,{i,Length[ListOfNamesToClear]}];
]; (* End If LowMem *)


If[  Mod[LifePosn,10] == 0,  (* Then do the experimental iteration *)
(*   LifePosn<LifeLength-10,*)
(*    Get[ProblemToSolveString<>"_steadystate.m"];*)
(*    <<simulation_pseudo.m;*)


If[WhichModel != RepAgent,
  (* then *) 
    If[LifePosn>LifeLength-100,<<simulation_pseudo.m;,<<simulation.m;Simulate[200,500]],
  (* else it's a RepAgent *)
    <<simulation.m;Simulate[1,1000]
]; (* End If WhichModel *)

{Intercept,kTm1Coeff,AggState2DumChopCoeff,kTm1TimesAggState2DumCoeff, AggState3DumChopCoeff,kTm1TimesAggState3DumCoeff,AggState4DumChopCoeff,kTm1TimesAggState4DumCoeff} =
(BestFitParameters /. Outstuff);

kTargetAS14 = Intercept/(1-kTm1Coeff);
kTargetAS23 = (Intercept+AggState3DumChopCoeff)/(1-(kTm1Coeff+kTm1TimesAggState3DumCoeff));

AggkSSByAggState = {kTargetAS14,kTargetAS23,kTargetAS23,kTargetAS14};
kAR1ByAggState   = {kTm1Coeff,kTm1Coeff+kTm1TimesAggState3DumCoeff,kTm1Coeff+kTm1TimesAggState3DumCoeff,kTm1Coeff};

    Print["{kSS,kAR1} = ",{AggkSSByAggState,kAR1ByAggState}];
  ]; (* End If LifePosn < LifeLength-50 *)

,{LoopOverPeriods,1,NumOfPeriods}]; (* end If *)

]; (* end KeepSolvingPeriods *)



