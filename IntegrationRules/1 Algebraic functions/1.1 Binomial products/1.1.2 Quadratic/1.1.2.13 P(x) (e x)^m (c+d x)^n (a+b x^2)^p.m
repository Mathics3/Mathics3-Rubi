(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Code:: *)
Int[Px_*(e_.*x_)^m_.*(c_+d_.*x_)^n_.*(a_+b_.*x_^2)^p_.,x_Symbol] :=
  Int[PolynomialQuotient[Px,c+d*x,x]*(e*x)^m*(c+d*x)^(n+1)*(a+b*x^2)^p,x] /;
FreeQ[{a,b,c,d,e,m,n,p},x] && PolynomialQ[Px,x] && EqQ[PolynomialRemainder[Px,c+d*x,x],0]


(* ::Code:: *)
Int[Px_*(c_+d_.*x_)^n_.*(a_+b_.*x_^2)^p_./x_,x_Symbol] :=
  Int[PolynomialQuotient[Px,x,x]*(c+d*x)^n*(a+b*x^2)^p,x] + 
  PolynomialRemainder[Px,x,x] \[Star] Int[(c+d*x)^n*(a+b*x^2)^p/x,x]/;
FreeQ[{a,b,c,d,n,p},x] && PolynomialQ[Px,x]


(* ::Code:: *)
Int[(e_.*x_)^m_*Px_/(Sqrt[c_+d_.*x_]*Sqrt[a_+b_.*x_^2]),x_Symbol] :=
  With[{Px0=Coefficient[Px,x,0]},
  Px0*(e*x)^(m+1)*Sqrt[c+d*x]*Sqrt[a+b*x^2]/(a*c*e*(m+1)) + 
  1/(2*a*c*e*(m+1)) \[Star] Int[(e*x)^(m+1)/(Sqrt[c+d*x]*Sqrt[a+b*x^2])*
    ExpandToSum[2*a*c*(m+1)*((Px-Px0)/x)-Px0*(a*d*(2*m+3)+2*b*c*(m+2)*x+b*d*(2*m+5)*x^2),x],x]]/;
FreeQ[{a,b,c,d,e},x] && PolynomialQ[Px,x] && LtQ[m,-1]


(* ::Code:: *)
Int[Px_*(e_.*x_)^m_.*(c_+d_.*x_)^n_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  Int[ExpandIntegrand[Px*(e*x)^m*(c+d*x)^n*(a+b*x^2)^p,x],x] /;
FreeQ[{a,b,c,d,e,m,n,p},x] && PolyQ[Px,x] && (IntegerQ[p] || IntegerQ[2*p] && IntegerQ[m] && ILtQ[n,0])


(* ::Code:: *)
Int[Px_*(e_.*x_)^m_*(c_+d_.*x_)^n_.*(a_+b_.*x_^2)^p_.,x_Symbol] :=
  With[{k=Denominator[m]},
  k/e \[Star] Subst[Int[ReplaceAll[Px,x->x^k/e]*x^(k*(m+1)-1)*(c+d*x^k/e)^n*(a+b*x^(2*k)/e^2)^p,x],x,(e*x)^(1/k)]] /;
FreeQ[{a,b,c,d,e,n,p},x] && PolyQ[Px,x] && FractionQ[m]


(* ::Code:: *)
Int[Px_*(e_.*x_)^m_.*(c_+d_.*x_)^n_*(a_+b_.*x_^2)^p_.,x_Symbol] :=
  Int[PolynomialQuotient[Px,c+d*x,x]*(e*x)^m*(c+d*x)^(n+1)*(a+b*x^2)^p,x] + 
  PolynomialRemainder[Px,c+d*x,x] \[Star] Int[(e*x)^m*(c+d*x)^n*(a+b*x^2)^p,x]/;
FreeQ[{a,b,c,d,e,m,p},x] && PolynomialQ[Px,x] && LtQ[n,0]


