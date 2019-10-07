(* :Name: Ticks *)

(* :Title: Ticks *)

(* :Author: Tom Wickham-Jones*)

(* :Package Version: 1.0 *)

(* :Mathematica Version: 2.2 *)

(*:Summary:
        This package provides functions for using the
        Ticks option of graphics objects.  It allows the
        length and style of tick marks and the text labels 
        to be altered.
*)


(* :History:
        Created summer 1992 by Tom Wickham-Jones.

        This package is described in the book
        Mathematica Graphics: Techniques and Applications.
        Tom Wickham-Jones, TELOS/Springer-Verlag 1994.

*)



BeginPackage[ "ExtendGraphics`Ticks`", "Utilities`FilterOptions`"]



TickPosition::usage = 
        "TickPosition[ min, max, num] returns a list of at most num nicely
        rounded positions between min and max.  These can be used for
        tick mark positions."


TickFunction::usage =
        "TickFunction is a function that can be used with the Ticks option
        to alter the appearance of tick marks."

MajorLength::usage =
        "MajorLength is an option of TickFunction that specifies the length
         of major tick marks.   MajorLength -> {0.00625, 0} specifies that
         major ticks should extend 0.00625 inside the axis and not outside."

MinorLength::usage =
        "MinorLength is an option of TickFunction that specifies the length
         of minor tick marks.   MinorLength -> {0.003125, 0} specifies that
         minor ticks should extend 0.003125 inside the axis and not outside."

MajorStyle::usage =
        "MajorStyle is an option of TickFunction that specifies the style
         in which major tick marks should be rendered."

MinorStyle::usage =
        "MinorStyle is an option of TickFunction that specifies the style
         in which minor tick marks should be rendered."

TextFunction::usage =
        "TextFunction is an option of TickFunction that specifies a function
         to process tick mark labels."

TrimText::usage =
        "TrimText is a function that can be used as the value of the
         TextFunction option of TickFunction to make sure that all 
         tick mark labels are the same length."

TickLabels::usage =
        "TickLabels is an option of TickFunction that gives the 
         positions and text to use for tick-labels.  The value of 
         TickLabels must be {{pos1, lab1}, {pos2, lab2}, ..}."
         
TickNumbers::usage =
        "TickNumbers is an option of TickFunction that gives the 
         number of major and minor tick marks."

         
         


Begin["`Private`"]

Options[ TickFunction] =
        {
        MajorLength -> {0.00625, 0},
        MinorLength -> {0.003125, 0},
        MajorStyle -> {Thickness[0.002]},
        MinorStyle -> {Thickness[0.001]},
        TextFunction -> Automatic,
        TickLabels -> Automatic,
        TickNumbers -> {8, 32}
        }

CheckNumbers[ {x_Integer, y_Integer}] :=
        If[ x > 0 && y > 0, {x,y}, CheckNumbers[ x]]
        
CheckNumbers[ _] := {8,32}

TickFunction[ x0_, x1_, opts___] :=
    Block[{maj, min, majlen, minlen, opt, tnums,
               majstyle, minstyle, textfun, labs},
                opt = Join[ {opts}, Options[ TickFunction]] ;
        majlen = MajorLength /. opt ;
        minlen = MinorLength /. opt ;
        majstyle = MajorStyle /. opt ;
        minstyle = MinorStyle /. opt ;
        textfun = TextFunction /. opt ;
        labs = TickLabels /. opt ; 
        tnums = CheckNumbers[ TickNumbers /. opt] ; 
                If[ textfun === Automatic, textfun = TrimDecimal] ;
                maj = TickPosition[ x0, x1, First[ tnums]] ;
                min = TickPosition[ x0, x1, Last[ tnums]] ;
                min = Complement[ min, maj] ;
                maj = If[ MatrixQ[ labs],
                                        Map[ {#, ""}&, maj], 
                                        Transpose[ {maj, textfun[ maj]}]] ;
                maj = Map[ {#[[1]], #[[2]], majlen, majstyle}&, maj] ;
                If[ Apply[ Plus, minlen] =!= 0,
                        min = Map[ {#, "", minlen, minstyle}&, min] ;
            maj = Join[ maj, min]] ;
                If[ MatrixQ[ labs], 
                        maj = Join[ maj, Map[ Join[#, {{0,0}}]&, labs]]] ;
                maj
        ]


TickPosition[ x0_Real, x1_Real, num_Integer?Positive] :=
    Block[{ dist, scale, min, max, i, delta, space},
                space = {1., 2., 2.5, 5., 10.} ;
        dist = (x1 - x0)/num ;
        scale = 10.^Floor[ Log[ 10, dist]] ;
        dist = dist / scale ;
        If[ dist < 1., dist *= 10.; scale /= 10.] ;
        If[ dist >= 10., dist /= 10.; scale *= 10.] ;
        delta = First[ Select[ space, (# >= dist)&]] scale;
        min = Ceiling[ x0/delta]*delta ; 
                Table[ Floor[ x/delta + 0.5]*delta, {x, min, x1, delta}]
        ]

TrimText[ x_ /; VectorQ[ x, (NumberQ[#] && Head[#] =!= Complex)&]] :=
        Block[{min, max, test, res},
                test = DeleteCases[ Chop[x], 0 | 0.] ;
                max = Max[ Map[ TotalDigits, test]] ;
                min = Max[ Map[ FractionalDigits, test]] ;
                res = Map[ ToString[ PaddedForm[ #, {max+min, min}]]&, x] ;
                Map[ StringReplace[#, " " -> ""]&, res]
        ]

TotalDigits[ num_] :=
        Block[{digs},
            digs = Drop[ First[ RealDigits[ num]], -1] ;
            Length[ digs //. {x__, 0} -> {x}]
        ]           

FractionalDigits[num_] :=
        Block[{digs, len}, 
            If[IntegerPart[num] == num,Return[0]];  (* This line added by Chris Carroll 10-13-99 to fix bug; original version didn't work with integers ending in 0 *)
            {digs, len} = RealDigits[ num] ;
            Length[ Drop[ digs,-1] //. {x__, 0} -> {x}] - len
        ]


TrimDecimal[ x_List] :=
        Map[ If[ Log[10., Abs[#]] < 5 && # == Round[#] , Round[#], #,#]&, x]


End[]

EndPackage[]

(*

<<Ticks.m

Plot[ x, {x,0,100}, Ticks -> TickFunction]

Plot[ Sin[x], {x,0,2},
        Ticks -> (TickFunction[#1, #2, TextFunction -> TrimText]&)]

Plot[ Sin[x], {x,0,2},
        Ticks -> 
                (TickFunction[#1, #2, 
                        MajorStyle -> {Thickness[0.001]},
                        MajorLength -> {0.004, 0.004},
                        MinorLength -> {0,0},
                        TextFunction -> TrimText]&)]




*)

