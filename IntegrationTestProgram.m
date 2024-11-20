(* ::Package:: *)

(* ::Title:: *)
(*Integration Test Program*)


BeginPackage["IntegrationTestProgram`"];


(* ::Section::Closed:: *)
(*Package Usage Messages*)


TestRubi::usage = "If <testSuite> is the name of a file in the integration test suite, TestRubi[testSuite] tests Rubi on the problems in <testSuite> and creates a notebook of the deficient results. If <testSuite> is the name of a directory in the integration test suite, TestRubi[testSuite] tests Rubi on the each of the files in <testSuite>. TestRubi[testSuite,True] saves the deficient results as the file testSuite.nb in a directory named \"Rubi Integration Test Results\".";
TestMathematica::usage = "If <testSuite> is the name of a file in the integration test suite, TestMathematica[testSuite] tests Mathematica's built-in integrator on the problems in <testSuite> and creates a notebook of the deficient results. If <testSuite> is the name of a directory in the integration test suite, TestMathematica[testSuite] tests Mathematica on the each of the files in <testSuite>. TestMathematica[testSuite,True] saves the deficient results as the file testSuite.nb in a directory named \"Mathematica Integration Test Results\".";
TestMathics::usage = "Test the <testSuit>s implemented in Mathics.";

$PrintProblems::usage = "If $PrintProblems is True, problems are displayed before they are integrated. It is False by default.";
$TestOnlyElementary::usage = "If $TestOnlyElementary is True, only problems having elementary integrands AND antiderivatives are tested. It is False by default.";
$PercentToTest::usage = "$PercentToTest is the percent of problem in the test-suite that are tested. It is 100 by default.";
$TimeOutLimit::usage = "$TimeOutLimit is the time limit in seconds allowed to integrate a problem. It is 120 by default.";
$DisplayDeficiencies::usage = "If $DisplayDeficiencies is True, deficient test results are displayed; otherwise, only the test summary is displayed. It is True by default.";
$HideKnownDeficiencies::usage = "If Rubi is being tested and $HideKnownDeficiencies is True, problems on which Rubi is known to be deficient are NOT included in the test results. It is False by default.";


ExpressionType::usage = "ExpressionType[expn,var] returns expn's type number based on the highest level of functions it involves:
  1 = rational function
  2 = algebraic function
  3 = elementary function
  4 = special function
  5 = hyperpergeometric function
  6 = appell function
  7 = open-form function
  8 = integrate function
  9 = unknown function.";


(* ::Section::Closed:: *)
(*Mathematica Test Functions*)


Begin["`Private`"];


$IntegrationTestProgramDir = Directory[];


(* ::Subsection::Closed:: *)
(*TestMathematica[testSuite, saveFlag]*)


TestMathematica[testSuite_String, saveFlag_:False] := 
  With[{path=FileNameJoin[{$IntegrationTestProgramDir, "Integration Test Suite", testSuite}]},
  If[Not[DirectoryQ[path] || FileExistsQ[path] || FileExistsQ[path<>".m"]],
    Print["\""<>testSuite<>"\" is not a test suite file or directory name."],
  Block[{$OptimalCounter=0, $TooLargeCounter=0, $ComplexCounter=0, $CannotIntegrateCounter=0, $TimeoutCounter=0,
    $SizeRatioTotal=0, $SizeRatioCounter=0, $HideKnownDeficiencies=False, $ResultNotebook},
  With[{systemName="Mathematica "<>StringTrim[ToString[$VersionNumber],"."]},
  $ResultNotebook = CreateDocument[{}, WindowTitle->testSuite, WindowSize->Scaled[1]];
  Print[systemName<>" Integration Test Results", "Title", 36, True];
  Print[
    "on "<>If[NumberQ[$PercentToTest] && 0<$PercentToTest<100, ToString[$PercentToTest]<>"% of ", ""]<>"the "<>
    If[TrueQ[$TestOnlyElementary], "elementary ", ""]<>
    "problems in "<>If[testSuite==="", "the entire integration test-suite", "\""<>testSuite<>"\""], "Subtitle", 32, True];
  If[DirectoryQ[path], Map[TestFileMathematica, FileNames["*.m", {path}, Infinity]], TestFileMathematica[If[FileExistsQ[path], path, path<>".m"]]];
  WriteMathematicaTestSummary[];
  If[TrueQ[saveFlag],
    SaveAndCloseNotebook[$ResultNotebook, FileNameJoin[{NotebookDirectory[], systemName<>" Integration Test Results", testSuite}], True],
  Null]]]]]


(* ::Subsection::Closed:: *)
(*TestFileMathematica[filename]*)


TestFileMathematica::usage = "TestFileMathematica[filename] tests Mathematica on the integration problems in filename.";
TestFileMathematica[filename_String] := 
  Module[{problemlist, num, indx},
  problemlist = ReadList[filename];
  If[problemlist===$Failed,
    Print["Test file "<>filename<>" not found."];
    Null,
  problemlist = Select[problemlist,Function[#=!=Null]];
  If[problemlist==={},
    Null,

  ( If[TrueQ[$TestOnlyElementary],
      problemlist = Select[problemlist, Function[ExpressionType[#[[1]],#[[2]]]<=3 && ExpressionType[#[[4]],#[[2]]]<=3]]] );

  num = Length[problemlist];
  ( If[NumberQ[$PercentToTest] && 0<$PercentToTest<100 && num>5,
      SeedRandom[314159265]; RandomInteger[100000]; RandomInteger[100000];
      problemlist = Part[problemlist,Sort[RandomSample[Range[num],Max[5,Ceiling[num*$PercentToTest/100]]]]];
      num = Length[problemlist];
      Print["Test results for " <> ToString[num] <> " problems in \"" <> FileNameTake[filename] <> "\"", "Section", 24];
      Print["Testing Mathematica on " <> ToString[num] <> " problems in \"" <> FileNameTake[filename] <> "\"."],
    Print["Test results for the " <> ToString[num] <> " problems in \"" <> FileNameTake[filename] <> "\"", "Section", 24];
    Print["Testing Mathematica on the " <> ToString[num] <> " problems in \"" <> FileNameTake[filename] <> "\"."]] );

  Monitor[
    Do[TestProblemMathematica[indx, problemlist[[indx]]], {indx, 1, num}],
    ProgressIndicator[indx, {1, num+1}, ImageSize->{1000,20}]];
  Print[""]]]]


(* ::Subsection::Closed:: *)
(*TestProblemMathematica[num, problem]*)


TestProblemMathematica::usage =
 "num is the number of the problem in the file being tested.
  problem is the integration problem in the form of a 4 or 5 element list:
    {integrand, variable, stepsrequired, optimal antiderivative, N/A (optional)}.
  If the result of Mathematica's integration of integrand wrt variable is not optimal,
    TestProblemMathematica[num, problem] explains the deficiency and displays the problem and nonoptimal result.";
TestProblemMathematica[num_, problem_]:=
  With[{integrand=problem[[1]], variable=problem[[2]], stepsrequired=problem[[3]], optimal1=problem[[4]]},
  If[TrueQ[$PrintProblems], DisplayProblem[num, integrand, variable]];
  Block[{integrationtime, result},
    ClearSystemCache[];
    {integrationtime,result} = TimeConstrained[Timing[Integrate[integrand,variable]],$TimeOutLimit,{0,"Timed out"}];

    If[result==="Timed out",
	  $TimeoutCounter++;
      DisplayTestResult["Attempted integration timed out after "<>ToString[$TimeOutLimit]<>" seconds.", 
        num, integrand, variable, stepsrequired, Null, optimal1, "???"],

    With[{resultsize=LeafCount[result], optimalsize=LeafCount[optimal1]},
    If[resultsize>200000,
      $TooLargeCounter++;
      DisplayTestResult["Humongous result has more than 200000 leaves.", 
        num, integrand, variable, stepsrequired, Null, optimal1, result],

    With[{sizeratio=N[resultsize/optimalsize]},

    With[{resulttype=ExpressionType[result,variable], optimaltype=ExpressionType[optimal1,variable]},
    If[resulttype>optimaltype,
      If[resulttype<=6,
        $ComplexCounter++;
        If[resultsize<=2*optimalsize,
          DisplayTestResult["Result unnecessarily involves higher level functions.", 
            num, integrand, variable, stepsrequired, Null, optimal1, result],
        DisplayTestResult["Result unnecessarily involves higher level functions and "<>ToString[NumberForm[sizeratio,{10,2}]]<>" times size of optimal antiderivative.", 
          num, integrand, variable, stepsrequired, Null, optimal1, result]],
	  $CannotIntegrateCounter++;
      DisplayTestResult[If[resulttype==7, "Result is not expressed in closed-form.", "Unable to integrate problem."], 
        num, integrand, variable, stepsrequired, Null, optimal1, result]],

    If[resulttype<optimaltype,
	  $OptimalCounter++;
      If[resulttype<=6 && ExpressionType[integrand,variable]<optimaltype && FreeQ[result,HypergeometricPFQ],
        DisplayTestResult["Mathematica result simpler than optimal antiderivative, IF it can be verified!", 
          num, integrand, variable, stepsrequired, Null, optimal1, result]],

    $SizeRatioCounter++;
    $SizeRatioTotal=sizeratio+$SizeRatioTotal;
    If[Xor[ComplexFreeQ[result], ComplexFreeQ[optimal1]],
      If[ComplexFreeQ[optimal1],
        $ComplexCounter++;
        If[resultsize<=2*optimalsize,
          DisplayTestResult["Result unnecessarily involves imaginary or complex numbers.", 
            num, integrand, variable, stepsrequired, Null, optimal1, result],
        DisplayTestResult["Result unnecessarily involves complex numbers and "<>ToString[NumberForm[sizeratio,{10,2}]]<>" times size of optimal antiderivative.", 
          num, integrand, variable, stepsrequired, Null, optimal1, result]],
	  $OptimalCounter++ (*;
      If[resulttype<=6 && FreeQ[result,HypergeometricPFQ],
        DisplayTestResult["Mathematica result simpler than optimal antiderivative, IF it can be verified!", 
          num, integrand, variable, stepsrequired, Null, optimal1, result]] *)],

    If[resultsize<=2*optimalsize,
	  $OptimalCounter++,
    $TooLargeCounter++;
    DisplayTestResult["Result "<>ToString[NumberForm[sizeratio,{10,2}]]<>" times size of optimal antiderivative.", 
      num, integrand, variable, stepsrequired, Null, optimal1, result]]]]]]]]]]]]


(* ::Subsection::Closed:: *)
(*WriteMathematicaTestSummary[]*)


WriteMathematicaTestSummary[] := (
  Print["Summary of "<>"Integration Test Results", "Section", 28, False, True];
  PrintText[ToString[$OptimalCounter+$TooLargeCounter+$ComplexCounter+$CannotIntegrateCounter+$TimeoutCounter]<>" integration problems", 16];
  Print[PieChart[{$OptimalCounter,$TooLargeCounter,$ComplexCounter,$CannotIntegrateCounter,$TimeoutCounter}, 
		ChartLabels->{"A", "B", "C", "D", "E"}, 
		ChartStyle->{RGBColor[0,0.8,0],RGBColor[0.7,1,0],Yellow,Orange,Red}], "Text", 12];
  PrintText["A - "<>ToString[$OptimalCounter]<>" optimal antiderivatives", 16];
  PrintText["B - "<>ToString[$TooLargeCounter]<>" more than twice size of optimal antiderivatives", 16];
  PrintText["C - "<>ToString[$ComplexCounter]<>" unnecessarily complex antiderivatives", 16];
  PrintText["D - "<>ToString[$CannotIntegrateCounter]<>" unable to integrate problems", 16];
  PrintText["E - "<>ToString[$TimeoutCounter]<>" integration timeouts", 16];
  PrintText["", 16];
  PrintText["Mathematica results "<>ToString[NumberForm[$SizeRatioTotal/$SizeRatioCounter,{10,2}]]<>" times size of optimal antiderivatives on average.", 16];
)


(* ::Section::Closed:: *)
(*Rubi Test Functions*)


(* ::Subsection::Closed:: *)
(*TestRubi[testSuite, saveFlag]*)


TestRubi[testSuite_String, saveFlag_:False] := 
  If[DownValues[Rubi`Int]==={},
    Print["Need to load Rubi before running TestRubi."],
  With[{path=FileNameJoin[{$IntegrationTestProgramDir, "Integration Test Suite", testSuite}]},
  If[Not[DirectoryQ[path] || FileExistsQ[path] || FileExistsQ[path<>".m"]],
    Print["\""<>testSuite<>"\" is not a test suite file or directory name."],
  Block[{$OptimalCounter=0, $SuboptimalCounter=0, $TooLargeCounter=0, $ComplexCounter=0, $CannotIntegrateCounter=0, 
    $TimeoutCounter=0, $InvalidCounter=0, $SizeRatioTotal=0, $SizeRatioCounter=0, Rubi`Unintegrable, Rubi`CannotIntegrate, $ResultNotebook},
  With[{systemName=Rubi`$RubiVersion},
  $ResultNotebook = CreateDocument[{}, WindowTitle->testSuite, WindowSize->Scaled[1]];
  Print[systemName<>" Integration Test Results", "Title", 36, True];
  Print[
    "on "<>If[NumberQ[$PercentToTest] && 0<$PercentToTest<100, ToString[$PercentToTest]<>"% of ", ""]<>"the "<>
    If[TrueQ[$TestOnlyElementary], "elementary ", ""]<>
    "problems in "<>If[testSuite==="", "the entire integration test-suite", "\""<>testSuite<>"\""], "Subtitle", 32, True];
  If[DirectoryQ[path], Map[TestFileRubi, FileNames["*.m", {path}, Infinity]], TestFileRubi[If[FileExistsQ[path], path, path<>".m"]]];
  WriteRubiTestSummary[];
  If[TrueQ[saveFlag],
    SaveAndCloseNotebook[$ResultNotebook, FileNameJoin[{NotebookDirectory[], systemName<>" Integration Test Results", testSuite}], True],
  Null]]]]]]


(* ::Subsection::Closed:: *)
(*TestFileRubi[filename]*)


TestFileRubi::usage = "TestFileRubi[filename] tests Rubi on the integration problems in filename.";
TestFileRubi[filename_String] := 
  Module[{problemlist, num, indx},
  problemlist = ReadList[filename];
  If[problemlist===$Failed,
    Print["Test file " <> filename <> " not found."];
    Null,
  problemlist = Select[problemlist,Function[#=!=Null]];
  If[problemlist==={},
    Null,

  ( If[TrueQ[$TestOnlyElementary],
      problemlist = Select[problemlist, Function[ExpressionType[#[[1]],#[[2]]]<=3 && ExpressionType[#[[4]],#[[2]]]<=3]]] );

  num = Length[problemlist];
  ( If[NumberQ[$PercentToTest] && 0<$PercentToTest<100 && num>5,
      SeedRandom[314159265]; RandomInteger[100000]; RandomInteger[100000];
      problemlist = Part[problemlist,Sort[RandomSample[Range[num],Max[5,Ceiling[num*$PercentToTest/100]]]]];
      num = Length[problemlist];
      Print["Test results for " <> ToString[num] <> " problems in \"" <> FileNameTake[filename] <> "\"", "Section", 24];
      Print["Testing Rubi on " <> ToString[num] <> " problems in \"" <> FileNameTake[filename] <> "\"."],
    Print["Test results for the " <> ToString[num] <> " problems in \"" <> FileNameTake[filename] <> "\"", "Section", 24];
    Print["Testing Rubi on the " <> ToString[num] <> " problems in \"" <> FileNameTake[filename] <> "\"."]] );

  Monitor[
    Do[TestProblemRubi[indx, problemlist[[indx]]], {indx, 1, num}],
    ProgressIndicator[indx, {1, num+1}, ImageSize->{1000,20}]];
  Print[""]]]]


(* ::Subsection::Closed:: *)
(*TestProblemRubi[num, problem]*)


TestProblemRubi::usage =
 "num is the number of the problem in the file being tested.
  problem is the integration problem in the form of a 4 or 5 element list:
    {integrand, variable, stepsrequired, optimal antiderivative, acceptable antiderivative (optional)}.
  If the result of Rubi's integration of integrand wrt variable is not optimal,
    TestProblemRubi[num, problem] explains the deficiency and displays the problem and nonoptimal result.";
TestProblemRubi[num_, problem_]:=
  With[{integrand=problem[[1]], variable=problem[[2]], stepsrequired=problem[[3]], optimal1=problem[[4]], optimal2=If[Length[problem]>=5, problem[[5]], Null]},
  If[TrueQ[$PrintProblems], DisplayProblem[num, integrand, variable]];
  Block[{integrationtime, result, Rubi`$StepCounter=0, $RuleList={}},
    ClearSystemCache[];
    {integrationtime,result} = TimeConstrained[Timing[Rubi`Int[integrand,variable]],$TimeOutLimit,{0,"Timed out"}];
    With[{stepsused=Rubi`$StepCounter},

    If[result==="Timed out",
	  $TimeoutCounter++;
      DisplayTestResult["Attempted integration timed out after "<>ToString[$TimeOutLimit]<>" seconds.", 
        num, integrand, variable, stepsrequired, stepsused, optimal1, "???"],

    With[{resultsize=LeafCount[result], optimalsize=LeafCount[optimal1]},
    If[resultsize>200000,
      $TooLargeCounter++;
      DisplayTestResult["Humongous result has more than 200000 leaves!", 
        num, integrand, variable, stepsrequired, Null, optimal1, result],

    With[{sizeratio=N[resultsize/optimalsize]},
    If[result===optimal1 || result===optimal2,
	  $OptimalCounter++;
      $SizeRatioCounter++;
      $SizeRatioTotal=1+$SizeRatioTotal;
      If[stepsrequired<0,
        DisplayTestResult["Result not only optimal but previously unobtained!", 
          num, integrand, variable, stepsrequired, stepsused, optimal1, result],
      If[stepsused>stepsrequired,
        DisplayTestResult["Result optimal but "<>ToString[stepsused-stepsrequired]<>" more steps used.", 
          num, integrand, variable, stepsrequired, stepsused, optimal1, result],
      If[stepsused<stepsrequired && $VersionNumber<11,
        DisplayTestResult["Result optimal and "<>ToString[stepsrequired-stepsused]<>" fewer steps used.", 
          num, integrand, variable, stepsrequired, stepsused, optimal1, result]]]],

    With[{resulttype=ExpressionType[result,variable], optimaltype=ExpressionType[optimal1,variable]},
    If[resulttype>optimaltype,
      If[resulttype<=6,
        $ComplexCounter++;
        If[resultsize<=2*optimalsize,
          DisplayTestResult["Result unnecessarily involves higher level functions.", 
            num, integrand, variable, stepsrequired, stepsused, optimal1, result],
        DisplayTestResult["Result unnecessarily involves higher level functions and "<>ToString[NumberForm[sizeratio,{10,2}]]<>" times size of optimal antiderivative.", 
          num, integrand, variable, stepsrequired, stepsused, optimal1, result]],
	  $CannotIntegrateCounter++;
      DisplayTestResult[If[resulttype==7, "Result is not expressed in closed-form.", "Unable to integrate problem."], 
        num, integrand, variable, stepsrequired, stepsused, optimal1, result]],

    If[resulttype<optimaltype,
      If[ValidAntiderivative[result,integrand,variable,100,optimal1],
  	  $OptimalCounter++;
        $SizeRatioCounter++;
        $SizeRatioTotal=sizeratio+$SizeRatioTotal;
        DisplayTestResult["Rubi result verified and simpler than optimal antiderivative.", 
          num, integrand, variable, stepsrequired, stepsused, optimal1, result],
  	$InvalidCounter++;
      Beep[];
      DisplayTestResult["Result invalid or unverifable.", 
        num, integrand, variable, stepsrequired, stepsused, optimal1, result]],

    $SizeRatioCounter++;
    $SizeRatioTotal=sizeratio+$SizeRatioTotal;
    If[Xor[ComplexFreeQ[result],ComplexFreeQ[optimal1]],
      If[ComplexFreeQ[optimal1],
        $ComplexCounter++;
        If[resultsize<=2*optimalsize,
          DisplayTestResult["Result unnecessarily involves imaginary or complex numbers.", 
            num, integrand, variable, stepsrequired, stepsused, optimal1, result],
        DisplayTestResult["Result unnecessarily involves complex numbers and "<>ToString[NumberForm[sizeratio,{10,2}]]<>" times size of optimal antiderivative.", 
          num, integrand, variable, stepsrequired, stepsused, optimal1, result]],
      If[ValidAntiderivative[result,integrand,variable,100,optimal1],
  	  $OptimalCounter++;
        DisplayTestResult["Rubi result verified and simpler than optimal antiderivative.", 
          num, integrand, variable, stepsrequired, stepsused, optimal1, result],
  	$InvalidCounter++;
      Beep[];
      DisplayTestResult["Result invalid or unverifable.", 
        num, integrand, variable, stepsrequired, stepsused, optimal1, result]]],

    If[ $VersionNumber>=11 &&  (* Mathematica 11+ simplifies results differently than earlier versions. *)
        stepsused<=stepsrequired && 
        -0.1 <= resultsize/LeafCount[If[optimal2===Null,optimal1,optimal2]]-1 <= 0.1 && 
        Simplify[result-If[optimal2===Null,optimal1,optimal2]]===0,
	  $OptimalCounter++,

    If[ValidAntiderivative[result,integrand,variable,100,optimal1],
      $SuboptimalCounter++;
      DisplayTestResult["Result valid but suboptimal antiderivative.", 
        num, integrand, variable, stepsrequired, stepsused, optimal1, result],
	$InvalidCounter++;
    Beep[];
    DisplayTestResult["Result invalid or unverifable.", 
      num, integrand, variable, stepsrequired, stepsused, optimal1, result]]]]]]]]]]]]]]]


(* ::Subsection::Closed:: *)
(*WriteRubiTestSummary[]*)


WriteRubiTestSummary[] := (
  Print["Summary of "<>"Integration Test Results", "Section", 28, False, True];
  PrintText[ToString[$OptimalCounter+$SuboptimalCounter+$TooLargeCounter+$ComplexCounter+$CannotIntegrateCounter+$TimeoutCounter+$InvalidCounter]<>" integration problems", 16];
  Print[PieChart[{$OptimalCounter,$SuboptimalCounter+$TooLargeCounter,$ComplexCounter,$CannotIntegrateCounter,$TimeoutCounter,$InvalidCounter}, 
		ChartLabels->{"A", "B", "C", "D", "E", "F"}, 
		ChartStyle->{RGBColor[0,0.8,0],RGBColor[0.7,1,0],Yellow,Orange,Magenta,Red}], "Text", 12];
  PrintText["A - "<>ToString[$OptimalCounter]<>" optimal antiderivatives", 16];
  PrintText["B - "<>ToString[$SuboptimalCounter+$TooLargeCounter]<>" valid but suboptimal antiderivatives", 16];
  PrintText["C - "<>ToString[$ComplexCounter]<>" unnecessarily complex antiderivatives", 16];
  PrintText["D - "<>ToString[$CannotIntegrateCounter]<>" unable to integrate problems", 16];
  PrintText["E - "<>ToString[$TimeoutCounter]<>" integration timeouts", 16];
  PrintText["F - "<>ToString[$InvalidCounter]<>" invalid antiderivatives", 16];
  PrintText["", 16];
  PrintText["Rubi results "<>ToString[NumberForm[$SizeRatioTotal/$SizeRatioCounter,{10,2}]]<>" times size of optimal antiderivatives on average.", 16];
)


(* ::Subsection::Closed:: *)
(*ValidAntiderivative[result, integrand, variable, maxtime, optimal]*)


ValidAntiderivative::usage = "If the derivative of result wrt variable equals integrand OR equals the derivative of optimal wrt variable,
   ValidAntiderivative[result,integrand,variable,maxtime,optimal] returns True; else it returns False.";
ValidAntiderivative[result_,integrand_,variable_,maxtime_,optimal_] :=
  If[Not[FreeQ[result,Hold]],
    False,
  If[TrigToExp[result]===result,
    TimeConstrained[ValidAntiderivativeTest[result,integrand,variable,maxtime,optimal],maxtime,False],
  TimeConstrained[
    If[TimeConstrained[ValidAntiderivativeTest[result,integrand,variable,maxtime,optimal],maxtime/2,False],
      True,
    ValidAntiderivativeTest[TrigToExp[result],integrand,variable,maxtime,optimal]],maxtime,False]]]


(* ValidAntiderivativeTest[result_,integrand_,variable_,maxtime_,optimal_] :=
  If[Head[result]===Plus,
    TimeConstrained[PossibleZeroQ[FullSimplify[Map[Function[FullSimplify[Dif[#,variable]]],result]-integrand]],maxtime,False],
  TimeConstrained[PossibleZeroQ[FullSimplify[FullSimplify[Dif[result,variable]]-integrand]],maxtime,False]] *)


ValidAntiderivativeTest[result_,integrand_,variable_,maxtime_,optimal_] :=
  Module[{dif=Dif[result,variable]},
    If[Head[result]===Plus,
      dif=Map[Function[Simplify[Dif[#,variable]]],result]; 
      If[TimeConstrained[PossibleZeroQ[FullSimplify[dif-integrand]],maxtime/4,False],
        True,
      dif=Simplify[dif];
      If[TimeConstrained[PossibleZeroQ[FullSimplify[dif-integrand]],maxtime/4,False],
        True,
      If[optimal=!=0 && 
         TimeConstrained[PossibleZeroQ[Simplify[dif-If[Head[optimal]===Plus, Simplify[Map[Function[Simplify[Dif[#,variable]]],optimal]], Simplify[Dif[optimal,variable]]]]],
           maxtime/4,False],
        True,
      PossibleZeroQ[FullSimplify[FullSimplify[dif]-integrand]]]]],
    If[optimal=!=0 && 
       TimeConstrained[PossibleZeroQ[Simplify[dif-If[Head[optimal]===Plus, Simplify[Map[Function[Simplify[Dif[#,variable]]],optimal]], Simplify[Dif[optimal,variable]]]]],
         maxtime/4,False],
      True,
    PossibleZeroQ[FullSimplify[FullSimplify[dif]-integrand]]]]]


(* ::Subsection::Closed:: *)
(*Dif[expn, x]*)


Dif::usage = "Dif[expn,var] returns the derivative of expn wrt var.  Dif knows how to handle expressions involving Int and Subst.";
Dif[u_,x_Symbol] :=
  If[FreeQ[u,Rubi`Int] && FreeQ[u,Integral] && FreeQ[u,Rubi`Unintegrable] && FreeQ[u,Rubi`CannotIntegrate] && FreeQ[u,Rubi`Subst] && FreeQ[u,SinIntegral],
    D[u,x],
  If[Head[u]===Power,
    If[FreeQ[u[[2]],x],
      u[[2]]*u[[1]]^(u[[2]]-1)*Dif[u[[1]],x],
    If[FreeQ[u[[1]],x],
      u*Log[u[[1]]]*Dif[u[[2]],x],
    Defer[Dif][u,x]]],
  If[Head[u]===Times,
    If[FreeQ[First[u],x],
      First[u]*Dif[Rest[u],x],
    First[u]*Dif[Rest[u],x] + Rest[u]*Dif[First[u],x]],
  If[Head[u]===Plus,
    Map[Function[Dif[#,x]],u],
  If[Head[u]===SinIntegral,
    Sin[u[[1]]]*Dif[u[[1]],x]/u[[1]],
  If[IntegrateFunctionQ[Head[u]] && u[[2]]===x,
    u[[1]],
  If[(Head[u]===Rubi`Subst || Head[u]===Defer[Rubi`Subst]) && u[[2]]===x,
    ReplaceAll[Dif[u[[1]],x],x->u[[3]]] * Dif[u[[3]],x],
  Defer[Dif][u,x]]]]]]]]


(* ::Section::Closed:: *)
(*Package Utility Functions*)


(* ::Subsection::Closed:: *)
(*DisplayTestResult[message, num, integrand, variable, steps, optimal, result]*)


DisplayTestResult::usage = "DisplayTestResult[message, num, integrand, variable, stepsrequired, stepsused, optimal, result] displays an integration test result.";
DisplayTestResult[message_String, num_Integer, integrand_, variable_, stepsrequired_, stepsused_, optimal_, result_] :=
  If[Not[TrueQ[$DisplayDeficiencies]] || TrueQ[$HideKnownDeficiencies] && (stepsused===-stepsrequired || stepsrequired===-1 && stepsused===0),
    Null,
  If[TrueQ[$PrintProblems], 
    PrintText[message], 
    DisplayProblem[num, integrand, variable, message]];

  PrintText[
    "Optimal(type "<>ToString[ExpressionType[optimal,variable]]<>", "<>
    ToString[LeafCount[optimal]]<>" leaves" <>
    If[IntegerQ[stepsrequired], If[stepsrequired<0, ", ? steps", If[stepsrequired==1, ", 1 step", ", "<>ToString[stepsrequired]<>" steps"]], ""] <>
    "):"];
  Print[Style[optimal,Blue], "Output"];

  With[{resultsize=LeafCount[result]},
  PrintText[
    "Result(type "<>If[resultsize<200000,ToString[ExpressionType[result,variable]],"?"]<>", "<>
    ToString[resultsize]<>" leaves" <>
    If[IntegerQ[stepsused], If[stepsused==1, ", 1 step", ", "<>ToString[stepsused]<>" steps"], ""] <>
    "):"<>
    If[resultsize<20000, "", " Display of huge result suppressed!"]];
  If[resultsize<20000, Print[result, "Output"]]]]


(* ::Subsection::Closed:: *)
(*DisplayProblem[num, integrand, variable]*)


DisplayProblem::usage = "DisplayProblem[num, integrand, variable] displays integration problem number num.";
DisplayProblem[num_Integer, integrand_, variable_] := (
  Print["Problem "<>ToString[num]<>":", "Subsection"];
  Print[Defer[Integrate[integrand, variable]], "Output"] )

DisplayProblem[num_Integer, integrand_, variable_, message_String] := (
  Print["Problem "<>ToString[num]<>": "<>message, "Subsection"];
  Print[Defer[Integrate[integrand, variable]], "Output"] )


(* ::Subsection::Closed:: *)
(*Print[expn, style, fontsize, center, pagebreak]*)

(*
Print[expn_, style_String, fontsize_:Null, center_:False, pagebreak_:Automatic] := (
  SelectionMove[$ResultNotebook, After, Notebook];
  With[{contents = If[StringQ[expn], expn, BoxData[MakeBoxes[expn]]]},
  If[IntegerQ[fontsize],
    If[TrueQ[center],
      NotebookWrite[$ResultNotebook, Cell[contents, style, FontSize->fontsize, TextAlignment->Center, PageBreakAbove->pagebreak]],
    NotebookWrite[$ResultNotebook, Cell[contents, style, FontSize->fontsize, PageBreakAbove->pagebreak]]],
  If[TrueQ[center],
    NotebookWrite[$ResultNotebook, Cell[contents, style, TextAlignment->Center, PageBreakAbove->pagebreak]],
  NotebookWrite[$ResultNotebook, Cell[contents, style, PageBreakAbove->pagebreak]]]]] )
*)

(* ::Subsection::Closed:: *)
(*PrintText[text, fontsize]*)


PrintText[text_String, fontsize_:Null] := (
  SelectionMove[$ResultNotebook, After, Notebook];
  If[IntegerQ[fontsize],
    NotebookWrite[$ResultNotebook, Cell[text, "Text", FontFamily->"Helvetica", FontSize->fontsize]],
  NotebookWrite[$ResultNotebook, Cell[Style[text, Larger], "Text", FontFamily->"Helvetica"]]] )


(* ::Subsection::Closed:: *)
(*SaveAndCloseNotebook[notebook, filename, closeFlag]*)


SaveAndCloseNotebook::usage = 
  "SaveAndCloseNotebook[notebook, filename] saves notebook as filename.nb and closes notebook.
   SaveAndCloseNotebook[notebook, filename, True] closes all groups before saving notebook.";

SaveAndCloseNotebook[notebook_, filename_String, closeFlag_:False] := (
  ( If[TrueQ[closeFlag],
      SelectionMove[notebook, All, Notebook];
      FrontEndExecute[FrontEndToken[notebook, "SelectionCloseAllGroups"]];
      FrontEndExecute[FrontEndToken[notebook, "OpenCloseGroup"]]] );
  If[Not[DirectoryQ[FileNameDrop[filename]]], CreateDirectory[FileNameDrop[filename]]];
  NotebookSave[notebook, filename<>".nb"];
  NotebookClose[notebook];
  Null )


(* ::Subsection::Closed:: *)
(*Print[message]*)

(*
Print::usage = "Print[message] displays message on the status bar.";
Print[message_String] := 
  If[TrueQ[$Notebooks], 
    CurrentValue[EvaluationNotebook[], WindowStatusArea] = message;
    CurrentValue[$ResultNotebook, WindowStatusArea] = message]
*)

(* ::Subsection::Closed:: *)
(*ComplexFreeQ[expn]*)


ComplexFreeQ::usage = "If expn is free of explicit complex numbers in rectangular or polar form, ComplexFreeQ[expn] returns True, else it returns False.";
ComplexFreeQ[expn_] := 
  FreeQ[expn,Complex] && FreeQ[expn,(-1)^Rational[_,_]]


(* ::Subsection::Closed:: *)
(*ExpressionType[expn,var]*)


ExpressionType[expn_,var_] :=
  If[AtomQ[expn] || FreeQ[expn,var],
    1,
  If[ListQ[expn],
    Max[Map[Function[ExpressionType[#,var]],expn]],
  If[Head[expn]===Power,
    If[IntegerQ[expn[[2]]],
      ExpressionType[expn[[1]],var],
    If[Head[expn[[2]]]===Rational,
      Max[ExpressionType[expn[[1]],var],2],
    Max[ExpressionType[expn[[1]],var],ExpressionType[expn[[2]],var],3]]],
  If[Head[expn]===Plus || Head[expn]===Times,
    Max[ExpressionType[First[expn],var],ExpressionType[Rest[expn],var]],
  If[ElementaryFunctionQ[Head[expn]],
    Max[3,ExpressionType[expn[[1]],var]],
  If[SpecialFunctionQ[Head[expn]],
    Apply[Max,Append[Map[Function[ExpressionType[#,var]],Apply[List,expn]],4]],
  If[HypergeometricFunctionQ[Head[expn]],
    Apply[Max,Append[Map[Function[ExpressionType[#,var]],Apply[List,expn]],5]],
  If[AppellFunctionQ[Head[expn]],
    Apply[Max,Append[Map[Function[ExpressionType[#,var]],Apply[List,expn]],6]],
  If[OpenFormFunctionQ[Head[expn]],
    Apply[Max,Append[Map[Function[ExpressionType[#,var]],Apply[List,expn]],7]],
  If[IntegrateFunctionQ[Head[expn]],
    Apply[Max,Append[Map[Function[ExpressionType[#,var]],Apply[List,expn]],8]],
  9]]]]]]]]]]


(* ::Subsection::Closed:: *)
(*Function type predicates*)


ElementaryFunctionQ::usage = "ElementaryFunctionQ[func] returns True if func is an elementary function; else it returns False.";
ElementaryFunctionQ[func_] := 
  MemberQ[{
	Exp, Log, 
	Sin, Cos, Tan, Cot, Sec, Csc, 
	ArcSin, ArcCos, ArcTan, ArcCot, ArcSec, ArcCsc, 
	Sinh, Cosh, Tanh, Coth, Sech, Csch, 
	ArcSinh, ArcCosh, ArcTanh, ArcCoth, ArcSech, ArcCsch
},func]


SpecialFunctionQ::usage = "SpecialFunctionQ[func] returns True if func is a special function; else it returns False.";
SpecialFunctionQ[func_] := 
  MemberQ[{
	Erf, Erfc, Erfi, 
	FresnelS, FresnelC, 
	ExpIntegralE, ExpIntegralEi, LogIntegral, 
	SinIntegral, CosIntegral, SinhIntegral, CoshIntegral, 
	Gamma, LogGamma, PolyGamma, 
	Zeta, PolyLog, ProductLog, 
	EllipticF, EllipticE, EllipticPi, EllipticK
},func]


HypergeometricFunctionQ::usage = "HypergeometricFunctionQ[func] returns True if func is a hypergeometric function; else it returns False.";
HypergeometricFunctionQ[func_] := 
  MemberQ[{Hypergeometric1F1, Hypergeometric2F1, HypergeometricPFQ, LerchPhi, HurwitzLerchPhi}, func]


AppellFunctionQ::usage = "AppellFunctionQ[func] returns True if func is a multivariate hypergeometric function; else it returns False.";
AppellFunctionQ[func_] := 
  MemberQ[{AppellF1}, func]


OpenFormFunctionQ::usage = "OpenFormFunctionQ[func] returns True if func is an open-form function; else it returns False.";
OpenFormFunctionQ[func_] := 
  MemberQ[{Root, RootSum, Function, Slot}, func]


IntegrateFunctionQ::usage = "IntegrateFunctionQ[func] returns True if func is an integrate function; else it returns False.";
IntegrateFunctionQ[func_] := 
  Not[FreeQ[func, Integrate]] ||
  Not[FreeQ[func, Integral]] ||
  Not[FreeQ[func, Int]] ||
  Not[FreeQ[func, Unintegrable]] ||
  Not[FreeQ[func, CannotIntegrate]] ||
  Not[FreeQ[func, Rubi`Int]] ||
  Not[FreeQ[func, Rubi`Unintegrable]] ||
  Not[FreeQ[func, Rubi`CannotIntegrate]]


(* ::Section::Closed:: *)
(*Default Values of Control Variables*)

(* Mathics implemented Test Suits *)
TestMathics[] = TestRubi["1 Algebraic functions"];

End [];
EndPackage [];


$PrintProblems = False;
$TestOnlyElementary = False;
$PercentToTest = 100;
$TimeOutLimit = 120;
$DisplayDeficiencies = True;
$HideKnownDeficiencies = False;
