
FigureSize = {72. 5.5,72. 5.5/GoldenRatio};

ConsFuncScaled[x_] := If[x wFunc[kSS]<.5,x wFunc[kSS],ctList[x wFunc[kSS],1,1,1,kSS,LifePosn]]/wFunc[kSS];

ConsFunc = 
  Plot[
  ConsFuncScaled[xt]
  ,{xt,0,20}
  ,AxesLabel->{"x","c[x]"}
  ,Ticks -> (TickFunction[#1, #2, TextFunction -> TrimText,MinorLength->{0,0}] &)    
  ,TextStyle->{FontSize->16,FontFamily->"Times"}
  ,ImageSize->FigureSize
  ,PlotRange->All
  ,PlotStyle->{Thickness[.004]}
  ,AxesStyle->{Thickness[.004]}
];

Export["A:ConsFunc.eps",ConsFunc];
Save["A:ConsFunc.sav",ConsFunc];


