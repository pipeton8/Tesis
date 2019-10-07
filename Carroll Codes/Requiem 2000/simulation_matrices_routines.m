
sTransMatrixByAggStateMake[QuietOrVerbose_] := Block[{},
  Numx   = Length[xMatGrid];
  Numh   = Length[hMatGrid];
  NumAgg = Length[AggStateGrid];
  NumEmp = Length[EmpStateGrid];
  Numep  = Length[AggKGrid];

  sTransMatrixByAggState =
  Table[
    sTransMatrix
  ,{ComingFrom,Length[AggStateGrid]}
  ,{GoingTo,Length[AggStateGrid]}];

Do[
  Print["GoingTo: ",GoingTo,", ComingFrom: ",ComingFrom];
  Do[
  If[QuietOrVerbose == Verbose,Print["LoopOverxRows:",LoopOverxRows,", LoopOverhRows:",LoopOverhRows]];
  Do[
  xColOffset   = (LoopOverxCols  -1);
  hColOffset   = (LoopOverhCols  -1);
  AggColOffset = (LoopOverAggCols-1);
  EmpColOffset = (LoopOverEmpCols-1);
  epColOffset  = (LoopOverepCols -1);
  xRowOffset   = (LoopOverxRows  -1);
  hRowOffset   = (LoopOverhRows  -1);
  AggRowOffset = (LoopOverAggRows-1);
  EmpRowOffset = (LoopOverEmpRows-1);
  epRowOffset  = (LoopOverepRows -1);

  RowOffset =     
    xRowOffset*Numh*NumAgg*NumEmp*Numep+
    hRowOffset     *NumAgg*NumEmp*Numep+
    AggRowOffset          *NumEmp*Numep+
    EmpRowOffset                 *Numep+
    epRowOffset+1;
  
  ColOffset = 
    xColOffset*Numh*NumAgg*NumEmp*Numep+
    hColOffset     *NumAgg*NumEmp*Numep+
    AggColOffset          *NumEmp*Numep+
    EmpColOffset                 *Numep+
    epColOffset+1;

(*  Print["{RowOffset,ColOffset} = ",{RowOffset,ColOffset}];*)
  sTransMatrixByAggState[[ComingFrom,GoingTo,RowOffset,ColOffset]]
    = If[LoopOverAggCols == ComingFrom && LoopOverAggRows == GoingTo,
         (* then *) sTransMatrix[[RowOffset,ColOffset]]/AggStateProb[ComingFrom,GoingTo],
         (* else *) 0];

    ,{LoopOverxCols  ,Numx}
    ,{LoopOverhCols  ,Numh}
    ,{LoopOverAggCols,NumAgg}
    ,{LoopOverEmpCols,NumEmp}
    ,{LoopOverepCols ,Numep}
        
    ,{LoopOverAggRows,NumAgg}
    ,{LoopOverEmpRows,NumEmp}
    ,{LoopOverepRows ,Numep}
    ]; (* End Do after print *)
    ,{LoopOverxRows  ,Numx}
    ,{LoopOverhRows  ,Numh}
  ]; (* End Do before print *)
,{GoingTo,Length[AggStateGrid]}
,{ComingFrom,Length[AggStateGrid]}
]; (* End Do *)
]; (* End Block *)


xTransMatrixByAggStateMake[QuietOrVerbose_] := Block[{},
  Numx   = Length[xMatGrid];
  Numh   = Length[hMatGrid];
  NumAgg = Length[AggStateGrid];
  NumEmp = Length[EmpStateGrid];
  Numep  = Length[AggKGrid];

  xTransMatrixByAggState =
  Table[
    xTransMatrix
  ,{ComingFrom,Length[AggStateGrid]}
  ,{GoingTo,Length[AggStateGrid]}];

Do[
  Print["GoingTo: ",GoingTo,", ComingFrom: ",ComingFrom];
  Do[
  If[QuietOrVerbose == Verbose,
    Print["LoopOverxRows:",LoopOverxRows,", LoopOverhRows:",LoopOverhRows]];
  Do[

  xColOffset   = (LoopOverxCols  -1);
  hColOffset   = (LoopOverhCols  -1);
  AggColOffset = (LoopOverAggCols-1);
  EmpColOffset = (LoopOverEmpCols-1);
  epColOffset  = (LoopOverepCols -1);
  xRowOffset   = (LoopOverxRows  -1);
  hRowOffset   = (LoopOverhRows  -1);
  AggRowOffset = (LoopOverAggRows-1);
  EmpRowOffset = (LoopOverEmpRows-1);
  epRowOffset  = (LoopOverepRows -1);

  RowOffset =     
    xRowOffset*Numh*NumAgg*NumEmp*Numep+
    hRowOffset     *NumAgg*NumEmp*Numep+
    AggRowOffset          *NumEmp*Numep+
    EmpRowOffset                 *Numep+
    epRowOffset+1;
  
  ColOffset = 
    xColOffset*Numh*NumAgg*NumEmp*Numep+
    hColOffset     *NumAgg*NumEmp*Numep+
    AggColOffset          *NumEmp*Numep+
    EmpColOffset                 *Numep+
    epColOffset+1;

(*  Print["{RowOffset,ColOffset} = ",{RowOffset,ColOffset}];*)
  xTransMatrixByAggState[[ComingFrom,GoingTo,RowOffset,ColOffset]]
    = If[LoopOverAggCols == ComingFrom && LoopOverAggRows == GoingTo,
         (* then *) xTransMatrix[[RowOffset,ColOffset]]/AggStateProb[ComingFrom,GoingTo],
         (* else *) 0];

    ,{LoopOverxCols  ,Numx}
    ,{LoopOverhCols  ,Numh}
    ,{LoopOverAggCols,NumAgg}
    ,{LoopOverEmpCols,NumEmp}
    ,{LoopOverepCols ,Numep}
        
    ,{LoopOverAggRows,NumAgg}
    ,{LoopOverEmpRows,NumEmp}
    ,{LoopOverepRows ,Numep}
    ]; (* End Do after print *)
    ,{LoopOverxRows  ,Numx}
    ,{LoopOverhRows  ,Numh}
  ]; (* End Do before print *)
,{GoingTo,Length[AggStateGrid]}
,{ComingFrom,Length[AggStateGrid]}
]; (* End Do *)
]; (* End Block *)
