(* -*- wolfram -*- *)
(* ::Package:: *)

(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Rubi (Rule-Based Integrator) Package *)
(* :Context: Rubi` *)
(* :Author: Albert D. Rich, Patrick Scheibe *)

(* :Mathematica Version: 7+ *)
(* :Copyright: (c) 2018-2019 Rule-based Integration Organization (https://rulebasedintegration.org/) *)

(* ::Title:: *)
(*Rubi (Rule-Based Integrator) Package*)

BeginPackage["Rubi`"];

(* Print packages being loaded if $DebugPrint is True*)
If[Not[ValueQ[$DebugPrint]], $DebugPrint = True;]
DebugPrint[x_] := If[$DebugPrint == True, Print[x]];

If[Not[ValueQ[$RubiDir]], $RubiDir = Directory[]];

(* ::Section::Closed:: *)
(* Usage Messages *)

Int::usage = "Int[expn, var] returns the antiderivative (indefinite integral) of <expn> with respect to <var>.\n" <>
    "Int[{expn1, expn2, ...},var] returns a list of the antiderivatives of <expn1>, <expn2>, ... each with respect to <var>.\n" <>
    "Int[expn, {var, a, b}] returns the limit of the antiderivative of <expn> as <var> approaches <b> minus the limit as <var> approaches <a>. " <>
    "Note that this difference will NOT always equal the definite integral of <expn> from <a> to <b>.";

$RubiVersion::usage = "$RubiVersion shows the currently loaded version of Rubi.";

Star::usage = "Star[expn1,expn2] displays as <expn1>*<expn2>, and returns their product with <expn1> distributed over the terms of <expn2>.";
Subst::usage = "Subst[expn1,var,expn2] substitutes <expn2> for <var> in <expn1>.";
Step::usage = "Step[Int[expn, var]] displays the first step in the integration of <expn> with respect to <var> and returns the intermediate result.";
Steps::usage = "Steps[Int[expn, var]] displays all the steps in the integration of <expn> with respect to <var> and returns the antiderivative.";
Stats::usage = "Stats[Int[expn, var]] prints statistical information of the integration before returning the antiderivative <expn> with respect to <var>." <>
    "It consists of (a) the number of steps used to integrate, (b) the number of distinct rules used, (c) is the leaf count size of the input," <>
    "(d) the leaf count size of the antiderivative, and (e) the rule-to-size ratio of the integration (i.e. the quotient of (b) and (c)).";

RubiRule::usage = "RubiRule is a symbolic wrapper that is used when displaying integration steps.";
RubiIntermediateResult::usage = "RubiIntermediateResult is a symbolic wrapper that is used when displaying integration steps.";
RubiStats::usage = "RubiStats is a symbolic wrapper that contains statistical information about an integration." <>
    "It consists of (a) the number of steps used to integrate, (b) the number of distinct rules used, (c) is the leaf count size of the input," <>
    "(d) the leaf count size of the antiderivative, and (e) the rule-to-size ratio of the integration (i.e. the quotient of (b) and (c)).";
RubiPrintInformation::usage = "RubiPrintInformation is an option to Steps and Stats that prints information if set to True and returns as a list otherwise.";
RubiClearMemoryImages::usage = "RubiClearMemoryImages[] deletes the memory files that are created for each system to speed-up the loading time of the package. " <>
    "The memory files are recreated during the next loading of the Rubi package.";

Unintegrable::usage = "Unintegrable[expn,var] indicates <expn> is not integrable with respect to <var> in closed-form.";
CannotIntegrate::usage = "CannotIntegrate[expn,var] indicates Rubi is unable to integrate <expn> with respect to <var>.";
$Unintegrable::usage = "If $Unintegrable is True and <expn> is not integrable with respect to <var> in terms of the functions Rubi uses to express antiderivatives, Int[expn,var] returns Unintegrable[expn,var].";
$StepCounter::usage = "If the ShowSteps package has been loaded and $StepCounter is an integer, it is incremented each time an integration rule is applied.";

sin::usage = "Inert sine function";
cos::usage = "Inert cosine function";
tan::usage = "Inert tangent function";
cot::usage = "Inert cotangent function";
sec::usage = "Inert secant function";
csc::usage = "Inert cosecant function";

(* ::Section::Closed:: *)
(* Implementation *)

Begin["`Private`"];

(* Higher $RecursionLimit needed for FixIntRules[] *)
$RecursionLimit = 512;

$RubiVersion = StringJoin["Rubi ", Version /. List@@Get[FileNameJoin[{$RubiDir, "PacletInfo.m"}]]];
Print["Loading " <> $RubiVersion <> " will take a minute or two. In the future this will take less than a second."];

(* Disable Steps *)
(* $LoadShowSteps = If[Not[ValueQ[Global`$LoadShowSteps]], True, TrueQ[Global`$LoadShowSteps]]; *)
$LoadShowSteps = False

$ruleDir = FileNameJoin[{$RubiDir, "IntegrationRules"}];
$utilityPackage = FileNameJoin[{$RubiDir, "IntegrationUtilityFunctions.m"}];
$stepRoutines = FileNameJoin[{$RubiDir, "ShowStepRoutines.m"}];
$ruleFormatting = FileNameJoin[{$RubiDir, "ShowStepFormatting.m"}];

RubiClearMemoryImages[] :=
  Module[{files = FileNames["*.mx", {FileNameJoin[{$RubiDir, "Kernel"}]}]},
  Map[DeleteFile,files]]

LoadRules::inv = "Could not load file or section: ``";
LoadRules[fileName_String /; FileExtension[fileName] =!= "m"] := LoadRules[FileNameJoin[{$ruleDir, fileName <> ".m"}]];
LoadRules[fileName_String /; FileExistsQ[fileName]] := (
  DebugPrint["Loading " <> FileBaseName@fileName <> ".m..."];
  Get[fileName];
  DebugPrint[""] );
LoadRules[arg___] := Message[LoadRules::inv, {arg}];

(* ::Section::Closed:: *)
(* Load Integration Rules *)
Unprotect[Int];  Clear[Int];  Clear[Unintegrable];  Clear[CannotIntegrate];

(* The order of loading the rule-files below is crucial to ensure a functional Rubi integrator! *)
LoadRules[$utilityPackage];

(* Section 1 *)
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.1 (a+b x)^m"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.2 (a+b x)^m (c+d x)^n"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.3 (a+b x)^m (c+d x)^n (e+f x)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.4 (a+b x)^m (c+d x)^n (e+f x)^p (g+h x)^q"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.1 (a+b x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.2 (c x)^m (a+b x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.3 (a+b x^2)^p (c+d x^2)^q"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.4 (e x)^m (a+b x^2)^p (c+d x^2)^q"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.5 (a+b x^2)^p (c+d x^2)^q (e+f x^2)^r"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.6 (g x)^m (a+b x^2)^p (c+d x^2)^q (e+f x^2)^r"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.7 (c+d x)^n (a+b x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.8 (e x)^m (c+d x)^n (a+b x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.9 (c+d x)^m (e+f x)^n (a+b x^2)^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.1 (a+b x^n)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.2 (c x)^m (a+b x^n)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.3 (a+b x^n)^p (c+d x^n)^q"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.4 (e x)^m (a+b x^n)^p (c+d x^n)^q"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.5 (a+b x^n)^p (c+d x^n)^q (e+f x^n)^r"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.6 (g x)^m (a+b x^n)^p (c+d x^n)^q (e+f x^n)^r"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.1 (a+b x+c x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.2 (d+e x)^m (a+b x+c x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.3 (d+e x)^m (f+g x)^n (a+b x+c x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.4 (a+b x+c x^2)^p (d+e x+f x^2)^q"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.5 (g+h x)^m (a+b x+c x^2)^p (d+e x+f x^2)^q"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.1 (a+b x^2+c x^4)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.2 (d x)^m (a+b x^2+c x^4)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.3 (d+e x^2)^q (a+b x^2+c x^4)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.4 (f x)^m (d+e x^2)^q (a+b x^2+c x^4)^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.1 (a+b x^n+c x^(2 n))^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.2 (d x)^m (a+b x^n+c x^(2 n))^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.3 (d+e x^n)^q (a+b x^n+c x^(2 n))^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.4 (f x)^m (d+e x^n)^q (a+b x^n+c x^(2 n))^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 Improper", "1.1.4.1 (a x^j+b x^n)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 Improper", "1.1.4.2 (c x)^m (a x^j+b x^n)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 Improper", "1.1.4.3 (e x)^m (a x^j+b x^k)^p (c+d x^n)^q"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.1 (a x^q+b x^n+c x^(2 n-q))^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.2 (d x)^m (a x^q+b x^n+c x^(2 n-q))^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.3 (d+e x^(n-q)) (a x^q+b x^n+c x^(2 n-q))^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.4 Improper", "1.2.4.4 (f x)^m (d+e x^(n-q)) (a x^q+b x^n+c x^(2 n-q))^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.4 Miscellaneous", "1.4.1 Algebraic function simplification"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.4 Miscellaneous", "1.4.2 Algebraic function normalization"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.7 P(x) (a+b x)^m (c+d x)^n (e+f x)^p (g+h x)^q"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.6 P(x) (a+b x)^m (c+d x)^n (e+f x)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.1 Linear", "1.1.1.5 P(x) (a+b x)^m (c+d x)^n"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.9 P(x) (a+b x+c x^2)^p (d+e x+f x^2)^q"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.8 P(x) (d+e x)^m (f+g x)^n (a+b x+c x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.7 P(x) (d+e x)^m (a+b x+c x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.1 Quadratic", "1.2.1.6 P(x) (a+b x+c x^2)^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.6 P(x) (d x)^m (a+b x^2+c x^4)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.5 P(x) (a+b x^2+c x^4)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.7 P(x) (d+e x^2)^q (a+b x^2+c x^4)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.2 Quartic", "1.2.2.8 P(x) (d+e x)^q (a+b x^2+c x^4)^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.6 P(x) (d x)^m (a+b x^n+c x^(2 n))^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.2 Trinomial products", "1.2.3 General", "1.2.3.5 P(x) (a+b x^n+c x^(2 n))^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.11 P(x) (c x)^m (a+b x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.10 P(x) (a+b x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.14 P(x) (c+d x)^m (e+f x)^n (a+b x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.13 P(x) (e x)^m (c+d x)^n (a+b x^2)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.2 Quadratic", "1.1.2.12 P(x) (c+d x)^n (a+b x^2)^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.8 P(x) (c x)^m (a+b x^n)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.3 General", "1.1.3.7 P(x) (a+b x^n)^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.1 Binomial products", "1.1.4 Improper", "1.1.4.4 P(x) (c x)^m (a x^j+b x^n)^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Polynomial products", "1.3.3 P(x)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Polynomial products", "1.3.1 u (a+b x+c x^2+d x^3)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Polynomial products", "1.3.2 u (a+b x+c x^2+d x^3+e x^4)^p"}]];
LoadRules[FileNameJoin[{"1 Algebraic functions", "1.3 Polynomial products", "1.3.4 P(x) Q(x)^p"}]];

LoadRules[FileNameJoin[{"1 Algebraic functions", "1.4 Miscellaneous", "1.4.3 Miscellaneous algebraic functions"}]];

(* Required rules from Section 9 *)
LoadRules[FileNameJoin[{"9 Miscellaneous", "9.1 Integrand simplification rules"}]];
LoadRules[FileNameJoin[{"9 Miscellaneous", "9.2 Piecewise linear functions"}]];
LoadRules[FileNameJoin[{"9 Miscellaneous", "9.3 Miscellaneous integration rules"}]];

(* Calculate the rule-count directly after all integration rules, because below there are some more rules added that are not integration rules. *)
$RuleCount = Length[DownValues[Int]];

If[$LoadShowSteps === True, LoadRules[$stepRoutines]];

Print["Modifying " <> ToString[$RuleCount] <> " integration rules to distribute coefficients over sums..."];
FixIntRules[];
Print[""];

(* Fix ReplacePart use *)
Unprotect[ReplacePart];
ReplacePart[x_, pos_, val_] := ReplacePart[x, pos->val];
Protect[ReplacePart];

(* Define a working definition for Refine *)
Refine[x_] := Simplify[x];

(* Fix conditions within With -- needs a full fix within the pattern library *)
Print["Patching With[]..."];
dvFixWith = RuleDelayed[Verbatim[HoldPattern][Verbatim[Condition][lhs_,cond_]],With[vars_,Verbatim[Condition][expr_,wcond_]]] :> \
            RuleDelayed[HoldPattern[Condition[lhs,cond && With[vars,wcond]]],With[vars,expr]];
dvFixWithNoCond = RuleDelayed[Verbatim[HoldPattern][lhs_],With[vars_,Verbatim[Condition][expr_,wcond_]]] :> \
            RuleDelayed[HoldPattern[Condition[lhs,With[vars,wcond]]],With[vars,expr]];
DownValues[Int] = DownValues[Int] /. dvFixWith /. dvFixWithNoCond;
Print[""];

If[$LoadShowSteps === True,
  StatusBarPrint["Modifying " <> ToString[$RuleCount] <> " integration rules to display steps..."];
  StepFunction[Int];
  StatusBarPrint[""]];

(* ::Section:: *)
(* Define Steps, Step and Stats*)

Int::noShowSteps = "To use this function, you need to define $LoadShowSteps=True before loading the Rubi package";
Steps::negSteps = "Number of steps must be a positive integer.";
SetAttributes[Steps, {HoldFirst}];
Options[Steps] = {
  RubiPrintInformation -> True
};
Int::wrngUsage = "Wrong usage of the `1` function. Please use `1`[Int[expr, x]].";
Steps[Int[expr_, x_], opts : OptionsPattern[]] := Steps[Int[expr, x], $IterationLimit, opts];
Steps[Int[expr_, x_], n_Integer, OptionsPattern[]] := Module[{result, steps},
  If[$LoadShowSteps =!= True,
    Message[Int::noShowSteps];
    Return[Int[expr, x]]
  ];
  {result, steps} = Reap@Block[{$ShowSteps = True},
    FixedPoint[
      Function[int,
        With[{held = ReplaceAll[HoldComplete[int], {Defer[Int] -> Int, Defer[Subst] -> Subst}]},
          Sow[RubiIntermediateResult[held]];
          ReleaseHold[held]
        ]
      ], Int[expr, x],
      n - 1
    ]
  ];
  If[OptionValue[RubiPrintInformation] === True,
    PrintRubiSteps[steps];
    result,
    {steps, result}
  ]
] /; Head[x] === Symbol && If[TrueQ[n > 0], True, Message[Steps::negSteps]; False];
Steps[___] := (Message[Int::wrngUsage, Steps]; $Failed);

SetAttributes[Step, {HoldFirst}];
Options[Step] = {
  RubiPrintInformation -> True
};
Step[Int[expr_, x_], OptionsPattern[]] := Module[
  {
    result,
    step
  },
  If[$LoadShowSteps =!= True,
    Message[Int::noShowSteps];
    Return[Int[expr, x]]
  ];
  {result, step} = Reap@Block[{$ShowSteps = True}, Int[expr, x]];
  If[OptionValue[RubiPrintInformation] === True,
    PrintRubiSteps[step];
    result,
    {step, result}
  ]
] /; Head[x] === Symbol;
Step[___] := (Message[Int::wrngUsage, Step]; $Failed);

SetAttributes[Stats, {HoldFirst}];
Options[Stats] = {
  RubiPrintInformation -> True
};
Stats[Int[expr_, x_], OptionsPattern[]] := Block[{$ShowSteps = False, $StepCounter = 0, $RuleList = {}},
  With[{result = Int[expr, x]},
    If[$LoadShowSteps =!= True,
      Message[Int::noShowSteps];
      Return[result]
    ];
    If[OptionValue[RubiPrintInformation] === True,
      Print@RubiStats@{$StepCounter, Length[$RuleList], LeafCount[expr], LeafCount[result], N[Length[$RuleList] / LeafCount[expr], 4], $RuleList};
      result,
      {
        RubiStats@{$StepCounter, Length[$RuleList], LeafCount[expr], LeafCount[result], N[Length[$RuleList] / LeafCount[expr], 4], $RuleList},
        result
      }
    ]
  ]] /; Head[x] === Symbol;
Stats[___] := (Message[Int::wrngUsage, Stats]; $Failed);

(* ::Section:: *)
(* Define Unintegrable and CannotIntegrate*)

Int::definite = "Rubi does not check whether the domain of integration is continuous.";
Int[u_, {x_Symbol, a_, b_}] := With[{result = Int[u, x]},
	Message[Int::definite];
	Limit[result, x -> b, Direction -> 1] - Limit[result, x -> a, Direction -> -1]];

Int[{u__}, x_Symbol] := Map[Function[Int[#, x]], {u}];

Protect[Int];


$Unintegrable = False;
Unintegrable[u_, x_] :=
    If[$Unintegrable === True,
      Defer[Unintegrable][u, x],
      Defer[Int][u, x]
    ];


CannotIntegrate[u_, x_] := Defer[Int][u, x];
LoadRules[$ruleFormatting];
Print[""];

End[];
EndPackage[];
