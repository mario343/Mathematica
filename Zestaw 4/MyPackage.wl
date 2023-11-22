(* ::Package:: *)

BeginPackage["MyPackage`"];

plotWithExtrema::usage = "plotWithExtrema[f, {xmin, xmax}] plots a function and marks its minima and maxima.";
FindArgMinOrMax::usage = "FindArgMinOrMax[f, {x, xmin, xmax}] finds the minima and maxima of a function.";
plotTangent::usage = "plotTangent[f, {x0, xmin, xmax}] plots the function and its tangent at the specified point.";
areaUnderCurve::usage = "areaUnderCurve[f, {x, xmin, xmax}] calculates the area under the curve of a function.";

Begin["`Private`"];

plotWithExtrema[f_, {xmin_, xmax_}] := 
 Module[{minima, maxima, extrema}, 
  extrema = FindArgMinOrMax[f, {x, xmin, xmax}];
  minima = Select[extrema, Last[#] == "min" &];
  maxima = Select[extrema, Last[#] == "max" &];
  Plot[f, {x, xmin, xmax}, 
   Epilog -> {Red, PointSize[0.02], 
     Tooltip[Point[{#, f /. x -> #}], {#, "Minimum"}] & /@ minima, 
     Blue, Tooltip[Point[{#, f /. x -> #}], {#, "Maximum"}] & /@ 
      maxima}]]

FindArgMinOrMax[f_, {x_, xmin_, xmax_}] := 
 Module[{df, zeros, extremapoints}, df = D[f, x];
  zeros = x /. NSolve[df == 0 && xmin <= x <= xmax, x];
  extremapoints = {{#, "min"} & /@ 
     Select[zeros, D[df, {x, 2}] > 0], {#, "max"} & /@ 
     Select[zeros, D[df, {x, 2}] < 0]};
  Flatten[extremapoints, 1]]

plotTangent[f_, {x0_, xmin_, xmax_}] := 
 Module[{tangent, intersection}, 
  tangent = Normal@Series[f, {x, x0, 1}];
  intersection = {x0, f /. x -> x0};
  Plot[{f, tangent}, {x, xmin, xmax}, 
   Epilog -> {PointSize[0.02], Red, 
     Tooltip[Point[intersection], {"Intersection", intersection}]}]]

areaUnderCurve[f_, {x_, xmin_, xmax_}] := 
 Module[{area}, area = NIntegrate[f, {x, xmin, xmax}];
  Plot[f, {x, xmin, xmax}, Filling -> Axis, 
   PlotLabel -> Row[{"Area = ", area}]]]

End[];

EndPackage[];

