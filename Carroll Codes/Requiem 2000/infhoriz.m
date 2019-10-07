
MPC = (1-(Rcertain beta[[LifePosn]])^(1/(rho+gamma*(1-rho)))/Rcertain)
MPCFunc[Rcertain_,DiscRate_,rho_,gamma_] := (1-(Rcertain beta[[LifePosn,EmpState]])^(1/(rho+gamma*(1-rho)))/Rcertain)

HumWealth = 1/(1-G[[LifePosn,1,1]]/Rcertain)
HumWealthFunc[GrowthRate_,Rcertain_] := 1/(1-GrowthRate/Rcertain)

cGrowth = (Rcertain beta[[LifePosn,EmpState]])^(1/(rho+gamma*(1-rho)))

cOh = 1+(cGrowth-1)/catchup

hSS = MPC(HumWealth+x)*catchup/(catchup+cGrowth-1)

Print["{cGrowth,Rcertain,G,MPC,(R-1)/R} = ",{cGrowth,Rcertain,G[[LifePosn,1,1]],MPC,(Rcertain-1)/Rcertain}];
Print["c[0] = ",MPC HumWealth];