
ListOfPrefixes =   {"FOCwrtRiskySharet","RiskySharet","OmegasInvt","OmegahInvt","OmegaInvt"
		    ,"CertEquiv"(*,"EtLogctp1ListGtp1Givenst","EtLogctp1ListGtp1SquaredGivenst","EtMPCPermList"*)
                    ,"VxInvt","VhInvt","VInvt","st","ct"(*,"EtLogctp1ListGtp1Oct","EtLogctp1ListGtp1OctSquared"*)};
(*  "Omegatp1ArgGridDistGivenOmegatArgList","Vtp1ArgGridDistGivenVtArgList"};*)

ListOfSuffixes = {};  

If[DirectSolve == True,
  ListOfSuffixes = Append[ListOfSuffixes,"Direct"];
  ];
  
If[MatricesSolve == True,
  ListOfSuffixes = Append[ListOfSuffixes,"Mat"];
  ];

If[ListSolve == True,
  ListOfSuffixes = Append[ListOfSuffixes,"List"];
  ];

If[ListSolve == True && ListPlusMatricesSolve == True && MatricesSolve == False,
  ListOfSuffixes = Append[ListOfSuffixes,"Mat"];
  ];
    
  
ListOfObjects = {"InterpData","InterpFunc"};
  
ListOfNamesToClear = Flatten[
  Table[
    ListOfPrefixes[[i]]<>ListOfSuffixes[[j]]<>ListOfObjects[[k]]
    ,{i,Length[ListOfPrefixes]}
    ,{j,Length[ListOfSuffixes]}
    ,{k,Length[ListOfObjects]}
  ]  
];

ListOfNamesToClear = Join[ListOfNamesToClear,{"Vtp1ArgGridDistGivenVtArgList",
  "Omegatp1ArgGridDistGivenOmegatArgList"}];

EmptyRemainder = Table[{},{LifeLength-LifePosn-1}];
Do[
  ToExpression[ListOfNamesToClear[[i]]<>" = Join[Take["<>ListOfNamesToClear[[i]]<>
      ",LifePosn+1],EmptyRemainder];"]
  ,{i,Length[ListOfNamesToClear]}];
