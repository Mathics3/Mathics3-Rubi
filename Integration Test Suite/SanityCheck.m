(* ::Package:: *)

(* ::Title::Closed:: *)
(*Integrands of the form (c x)^m (a+b x)^n*)


(* ::Section::Closed:: *)
(*Integrands of the form (b x)^n*)


(* ::Subsection::Closed:: *)
(*Integrands of the form b*)


{0, x, 1, 0}
{1, x, 1, x}
{5, x, 1, 5*x}
{-2, x, 1, -2*x}
{-3/2, x, 1, -3/2*x}
{Pi, x, 1, Pi*x}
{a, x, 1, a*x}
{3*a, x, 1, 3*a*x}
{Pi/Sqrt[16 - E^2], x, 1, (Pi*x)/Sqrt[16 - E^2]}


(* ::Subsection::Closed:: *)
(*Integrands of the form x^n*)


{x^100, x, 1, x^101/101}
{x^3, x, 1, x^4/4}
{x^2, x, 1, x^3/3}
{x^1, x, 1, x^2/2}
{x^0, x, 1, x}
{1/x^1, x, 1, Log[x]}
{1/x^2, x, 1, -(1/x)}
{1/x^3, x, 1, -(1/(2*x^2))}
{1/x^4, x, 1, -(1/(3*x^3))}
{1/x^100, x, 1, -1/(99*x^99)}


(* ::Subsection::Closed:: *)
(*Integrands of the form (b x)^(n/2)*)


{x^(5/2), x, 1, 2*x^(7/2)/7}
{x^(3/2), x, 1, 2*x^(5/2)/5}
{x^(1/2), x, 1, 2*x^(3/2)/3}
{1/x^(1/2), x, 1, 2*Sqrt[x]}
{1/x^(3/2), x, 1, -2/Sqrt[x]}
{1/x^(5/2), x, 1, -2/(3*x^(3/2))}
