
ListOfPrefixes = 
  {"FOCwrtRiskySharet","RiskySharet","OmegasInvt","OmegahInvt","OmegaInvt"
  ,"CertEquiv","VxInvt","VhInvt","VInvt","st","ct"};
  
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
