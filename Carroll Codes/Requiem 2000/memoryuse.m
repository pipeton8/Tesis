
zzzzFunc := Block[{},
  zzzz    = Names["Global`*"];
  zzzzMem = Table[ToExpression["ByteCount["<>zzzz[[zz]]<>"]"],{zz,Length[zzzz]-4}];
  Print[MatrixForm[Sort[Transpose[{zzzzMem,Take[zzzz,Length[zzzz]-4]}]]]];
];

zzzzFunc;

(*
MemUseFunc := Block[{VarsInUse= Names["Global`*"]},
Table[
  Print[
  {i,VarsInUse[[i]],ByteCount[ToExpression[VarsInUse[[i]]]]}]
  ,{i,Length[VarsInUse]}];

(*Print[MatrixForm[Sort[Transpose[{MemoryUsedByVar,VarsInUse}]]]]*)
];

(*  Map[Apply[ByteCount[ToExpression[#]],#] &,Take[zzzz,Length[zzzz]-3]];*)

MemUseFunc;

*)
