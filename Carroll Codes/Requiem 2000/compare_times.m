
FixedTimeCostMat = TimeForMatricesSetup;

VariableTimeCostMat = TimeForArrayArgs+
                      TimeForFOCwrtRiskySharetMat+
                      TimeForRiskySharetMat+
                      TimeForCertEquivMat+
                      TimeForctMat+
                      TimeForVxInvtMat+
                      TimeForOmegahInvtMat
                      TimeForOmegasInvtMat;                      
VariableTimeCostDirect= 
                      TimeForFOCwrtRiskySharetDirect+
                      TimeForRiskySharetDirect+
                      TimeForCertEquivDirect+
                      TimeForctDirect+
                      TimeForVxInvtDirect+
                      TimeForOmegahInvtDirect+
                      TimeForOmegasInvtDirect;
VariableTimeCostList= 
                      TimeForFOCwrtRiskySharetList+
                      TimeForRiskySharetList+
                      TimeForCertEquivList+
                      TimeForctList+
                      TimeForVxInvtList+
                      TimeForOmegahInvtList+
                      TimeForOmegasInvtList;
                      
Print["FixedTimeCostMat:  ",FixedTimeCostMat];
Print["FixedTimeCostList: ",FixedTimeCostList];

Print["{VariableTimeCostDirect,VariableTimeCostMat,VariableTimeCostList} = ",{VariableTimeCostDirect,VariableTimeCostMat,VariableTimeCostList}];

Print["Time for constructing arg arrays:",TimeForArrayArgs];

Print[
  MatrixForm[
    {
     {FOCwrtRiskySharet,TimeForFOCwrtRiskySharetDirect,TimeForFOCwrtRiskySharetMat,TimeForFOCwrtRiskySharetList}
    ,{RiskySharet      ,TimeForRiskySharetDirect      ,TimeForRiskySharetMat,TimeForRiskySharetList}
    ,{OmegasInv        ,TimeForOmegasInvtDirect       ,TimeForOmegasInvtMat,TimeForOmegasInvtList}
    ,{CertEquiv        ,TimeForCertEquivDirect        ,TimeForCertEquivMat,TimeForCertEquivList}
    ,{ct               ,TimeForctDirect               ,TimeForctMat,TimeForctList}
    ,{VxInvt           ,TimeForVxInvtDirect           ,TimeForVxInvtMat,TimeForVxInvtList}
    }
    ,TableHeadings->{"Constructed","Standard Method","Matrix Method","List Method"}
  ]
];