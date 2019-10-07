
(*
To free up memory, dump some of the data used to construct behavior
*)

InterpDataNames = Names["Global`*InterpData"];

Table[ToExpression["Remove["<>InterpDataNames[[i]]<>"]"],{i,Length[InterpDataNames]}];

Remove[VhInvtListInterpFunc];


NamesToRemove = Names["Global`*AllArg*"];
NamesToRemove = Union[NamesToRemove,Names["Global`*ArgArray"]];

Table[ToExpression["Remove["<>NamesToRemove[[i]]<>"]"],{i,Length[NamesToRemove]}];

