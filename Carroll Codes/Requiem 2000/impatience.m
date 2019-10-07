
ImpatienceRcertain = (beta[[LifeLength]] Rcertain)^(1/(rho+gamma(1-rho)));

ImpatienceRrisky   = 
  Sum[((beta[[LifeLength]] R[[1,i]])^(1/(rho+gamma(1-rho))))*Rprob[[1,i]]
  ,{i,Length[Rprob[[1]]]}];
  
Print["Impatience for certain interest rate: ",ImpatienceRcertain];
Print["Impatience for risky   interest rate: ",ImpatienceRrisky];
