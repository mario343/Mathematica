#!/usr/bin/env wolframscript
(* ::Package:: *)

#!/usr/bin/env wolframscript

number = ToExpression[$ScriptCommandLine[[2]]];
fibonacciFunction[n_Integer] := Module[{fibList},
  fibList = {0, 1}; (* Initialize with 0 and 1 *)
  Do[
    If[i > 1, fibList = Append[fibList, fibList[[-1]] + fibList[[-2]]]],
    {i, 2, n}
  ];
  fibList
]

Print[fibonacciFunction[number]];

