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
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_.,x_Symbol] :=
  Int[(d+e*x)^(m+1)*PolynomialQuotient[Pq,d+e*x,x]*(a+b*x^2)^p,x] /;
FreeQ[{a,b,d,e,m,p},x] && PolyQ[Pq,x] && EqQ[PolynomialRemainder[Pq,d+e*x,x],0]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*P2_*(a_+b_.*x_^2)^p_.,x_Symbol] :=
  With[{f=Coeff[P2,x,0],g=Coeff[P2,x,1],h=Coeff[P2,x,2]},
  h*(d+e*x)^(m+1)*(a+b*x^2)^(p+1)/(b*e*(m+2*p+3)) /;
 EqQ[2*d*h*(p+1)-e*g*(m+2*p+3),0] && EqQ[a*h*(m+1)-b*f*(m+2*p+3),0]] /;
FreeQ[{a,b,d,e,m,p},x] && PolyQ[P2,x,2] && NeQ[m+2*p+3,0]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_.,x_Symbol] :=
  Int[ExpandIntegrand[(d+e*x)^m*Pq*(a+b*x^2)^p,x],x] /;
FreeQ[{a,b,d,e,m},x] && PolyQ[Pq,x] && IGtQ[p,-2]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_.,x_Symbol] :=
  d*e \[Star] Int[(d+e*x)^(m-1)*PolynomialQuotient[Pq,a*e+b*d*x,x]*(a+b*x^2)^(p+1),x] /;
FreeQ[{a,b,d,e,m,p},x] && PolyQ[Pq,x] && EqQ[b*d^2+a*e^2,0] && EqQ[PolynomialRemainder[Pq,a*e+b*d*x,x],0]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  With[{Qx=PolynomialQuotient[Pq,a*e+b*d*x,x], R=PolynomialRemainder[Pq,a*e+b*d*x,x]},
  -d*R*(d+e*x)^m*(a+b*x^2)^(p+1)/(2*a*e*(p+1)) + 
  d/(2*a*(p+1)) \[Star] Int[(d+e*x)^(m-1)*(a+b*x^2)^(p+1)*ExpandToSum[2*a*e*(p+1)*Qx+R*(m+2*p+2),x],x]] /;
FreeQ[{a,b,d,e},x] && PolyQ[Pq,x] && EqQ[b*d^2+a*e^2,0] && ILtQ[p+1/2,0] && GtQ[m,0]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  Int[ExpandIntegrand[(a+b*x^2)^p,(d+e*x)^m*Pq,x],x] /;
FreeQ[{a,b,d,e},x] && PolyQ[Pq,x] && EqQ[b*d^2+a*e^2,0] && EqQ[m+Expon[Pq,x]+2*p+1,0] && ILtQ[m,0]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  With[{q=Expon[Pq,x],f=Coeff[Pq,x,Expon[Pq,x]]},
  f*(d+e*x)^(m+q-1)*(a+b*x^2)^(p+1)/(b*e^(q-1)*(m+q+2*p+1)) + 
  1/(b*e^q*(m+q+2*p+1)) \[Star] Int[(d+e*x)^m*(a+b*x^2)^p*
    ExpandToSum[b*e^q*(m+q+2*p+1)*Pq-b*f*(m+q+2*p+1)*(d+e*x)^q-2*e*f*(m+p+q)*(d+e*x)^(q-2)*(a*e-b*d*x),x],x] /;
 NeQ[m+q+2*p+1,0]] /;
FreeQ[{a,b,d,e,m,p},x] && PolyQ[Pq,x] && EqQ[b*d^2+a*e^2,0] && Not[IGtQ[m,0]]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_.,x_Symbol] :=
  Int[(d+e*x)^(m+p)*(a/d+b/e*x)^p*Pq,x] /;
FreeQ[{a,b,d,e,m},x] && PolyQ[Pq,x] && EqQ[b*d^2+a*e^2,0] && IntegerQ[p]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  (a+b*x^2)^FracPart[p]/((d+e*x)^FracPart[p]*(a/d+(b*x)/e)^FracPart[p]) \[Star] Int[(d+e*x)^(m+p)*(a/d+b/e*x)^p*Pq,x] /;
FreeQ[{a,b,d,e,m,p},x] && PolyQ[Pq,x] && EqQ[b*d^2+a*e^2,0] && Not[IntegerQ[p]] && Not[IGtQ[m,0]]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  With[{Qx=PolynomialQuotient[Pq,a+b*x^2,x],
        R=Coeff[PolynomialRemainder[Pq,a+b*x^2,x],x,0],
        S=Coeff[PolynomialRemainder[Pq,a+b*x^2,x],x,1]},
  (d+e*x)^m*(a+b*x^2)^(p+1)*(a*S-b*R*x)/(2*a*b*(p+1)) + 
  1/(2*a*b*(p+1)) \[Star] Int[(d+e*x)^(m-1)*(a+b*x^2)^(p+1)*
    ExpandToSum[2*a*b*(p+1)*(d+e*x)*Qx-a*e*S*m+b*d*R*(2*p+3)+b*e*R*(m+2*p+3)*x,x],x]] /;
FreeQ[{a,b,d,e},x] && PolyQ[Pq,x] && NeQ[b*d^2+a*e^2,0] && LtQ[p,-1] && GtQ[m,0] && 
  Not[IGtQ[m,0] && RationalQ[a,b,d,e] && (IntegerQ[p] || ILtQ[p+1/2,0])]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  With[{Qx=PolynomialQuotient[(d+e*x)^m*Pq,a+b*x^2,x],
        R=Coeff[PolynomialRemainder[(d+e*x)^m*Pq,a+b*x^2,x],x,0],
        S=Coeff[PolynomialRemainder[(d+e*x)^m*Pq,a+b*x^2,x],x,1]},
  (a*S-b*R*x)*(a+b*x^2)^(p+1)/(2*a*b*(p+1)) + 
  1/(2*a*b*(p+1)) \[Star] Int[(d+e*x)^m*(a+b*x^2)^(p+1)*
    ExpandToSum[2*a*b*(p+1)*(d+e*x)^(-m)*Qx+b*R*(2*p+3)*(d+e*x)^(-m),x],x]] /;
FreeQ[{a,b,d,e},x] && PolyQ[Pq,x] && NeQ[b*d^2+a*e^2,0] && LtQ[p,-1] && ILtQ[m,0]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  With[{Qx=PolynomialQuotient[Pq,a+b*x^2,x],
        R=Coeff[PolynomialRemainder[Pq,a+b*x^2,x],x,0],
        S=Coeff[PolynomialRemainder[Pq,a+b*x^2,x],x,1]},
  -(d+e*x)^(m+1)*(a+b*x^2)^(p+1)*(a*(e*R-d*S)+(b*d*R+a*e*S)*x)/(2*a*(p+1)*(b*d^2+a*e^2)) + 
  1/(2*a*(p+1)*(b*d^2+a*e^2)) \[Star] Int[(d+e*x)^m*(a+b*x^2)^(p+1)*
   ExpandToSum[2*a*(p+1)*(b*d^2+a*e^2)*Qx+b*d^2*R*(2*p+3)-a*e*(d*S*m-e*R*(m+2*p+3))+e*(b*d*R+a*e*S)*(m+2*p+4)*x,x],x]] /;
FreeQ[{a,b,d,e,m},x] && PolyQ[Pq,x] && NeQ[b*d^2+a*e^2,0] && LtQ[p,-1] && 
  Not[IGtQ[m,0] && RationalQ[a,b,d,e] && (IntegerQ[p] || ILtQ[p+1/2,0])]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  With[{Qx=PolynomialQuotient[Pq,d+e*x,x], R=PolynomialRemainder[Pq,d+e*x,x]},
  e*R*(d+e*x)^(m+1)*(a+b*x^2)^(p+1)/((m+1)*(b*d^2+a*e^2)) + 
  1/((m+1)*(b*d^2+a*e^2)) \[Star] Int[(d+e*x)^(m+1)*(a+b*x^2)^p*
     ExpandToSum[(m+1)*(b*d^2+a*e^2)*Qx+b*d*R*(m+1)-b*e*R*(m+2*p+3)*x,x],x]] /;
FreeQ[{a,b,d,e,p},x] && PolyQ[Pq,x] && NeQ[b*d^2+a*e^2,0] && LtQ[m,-1]


(* ::Code:: *)
Int[x_^m_.*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  Module[{q=Expon[Pq,x],k},
  Int[x^m*Sum[Coeff[Pq,x,2*k]*x^(2*k),{k,0,q/2}]*(a+b*x^2)^p,x] + 
  Int[x^(m+1)*Sum[Coeff[Pq,x,2*k+1]*x^(2*k),{k,0,(q-1)/2}]*(a+b*x^2)^p,x]] /;
FreeQ[{a,b,p},x] && PolyQ[Pq,x] && Not[PolyQ[Pq,x^2]] && IGtQ[m,-2] && Not[IntegerQ[2*p]]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_,x_Symbol] :=
  With[{q=Expon[Pq,x],f=Coeff[Pq,x,Expon[Pq,x]]},
  f*(d+e*x)^(m+q-1)*(a+b*x^2)^(p+1)/(b*e^(q-1)*(m+q+2*p+1)) + 
  1/(b*e^q*(m+q+2*p+1)) \[Star] Int[(d+e*x)^m*(a+b*x^2)^p*
    ExpandToSum[b*e^q*(m+q+2*p+1)*Pq-b*f*(m+q+2*p+1)*(d+e*x)^q-f*(d+e*x)^(q-2)*(a*e^2*(m+q-1)-b*d^2*(m+q+2*p+1)-2*b*d*e*(m+q+p)*x),x],x] /;
 GtQ[q,1] && NeQ[m+q+2*p+1,0]] /;
FreeQ[{a,b,d,e,m,p},x] && PolyQ[Pq,x] && NeQ[b*d^2+a*e^2,0] && Not[EqQ[d,0] && True] && 
  Not[IGtQ[m,0] && RationalQ[a,b,d,e] && (IntegerQ[p] || ILtQ[p+1/2,0])]


(* ::Code:: *)
Int[(d_+e_.*x_)^m_.*Pq_*(a_+b_.*x_^2)^p_.,x_Symbol] :=
  With[{q=Expon[Pq,x]},
  Coeff[Pq,x,q]/e^q \[Star] Int[(d+e*x)^(m+q)*(a+b*x^2)^p,x] + 
  1/e^q \[Star] Int[(d+e*x)^m*(a+b*x^2)^p*ExpandToSum[e^q*Pq-Coeff[Pq,x,q]*(d+e*x)^q,x],x]] /;
FreeQ[{a,b,d,e,m,p},x] && PolyQ[Pq,x] && NeQ[b*d^2+a*e^2,0] && 
  Not[IGtQ[m,0] && RationalQ[a,b,d,e] && (IntegerQ[p] || ILtQ[p+1/2,0])]


