newSave[file_String,sym_Symbol]:=
	Module[{str},
	str=OpenAppend[file];
	WriteString[str,
	ToString[Unevaluated[sym]]<>" = "<>ToString[InputForm[sym]]<>"\n"];
		Close[str]]/;$UseNewSave

Attributes[newSave]={HoldRest};

$UseNewSave = True;