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
Int[u_.*(b_.*x_^n_)^p_*(d_.*x_^n_)^q_,x_Symbol] :=
  b^IntPart[p]*d^IntPart[q]*(b*x^n)^FracPart[p]*(d*x^n)^FracPart[q]/x^(n*(FracPart[p]+FracPart[q])) \[Star] Int[u*x^(n*(p+q)),x] /;
FreeQ[{b,d,n,p,q},x]


(* ::Code:: *)
Int[u_.*(a_+b_.*x_^n_)^p_.*(c_+d_.*x_^n_)^q_.,x_Symbol] :=
  (b/d)^p \[Star] Int[u*(c+d*x^n)^(p+q),x] /;
FreeQ[{a,b,c,d,n,p,q},x] && EqQ[b*c-a*d,0] && IntegerQ[p] && Not[IntegerQ[q] && SimplerQ[a+b*x^n,c+d*x^n]]


(* ::Code:: *)
Int[u_.*(a_+b_.*x_^n_)^p_*(c_+d_.*x_^n_)^q_,x_Symbol] :=
  (b/d)^p \[Star] Int[u*(c+d*x^n)^(p+q),x] /;
FreeQ[{a,b,c,d,n,p,q},x] && EqQ[b*c-a*d,0] && GtQ[b/d,0] && Not[SimplerQ[a+b*x^n,c+d*x^n]]


(* ::Code:: *)
Int[u_.*(a_+b_.*x_^n_)^p_*(c_+d_.*x_^n_)^q_,x_Symbol] :=
  (a+b*x^n)^p/(c+d*x^n)^p \[Star] Int[u*(c+d*x^n)^(p+q),x] /;
FreeQ[{a,b,c,d,n,p,q},x] && EqQ[b*c-a*d,0] && Not[SimplerQ[a+b*x^n,c+d*x^n]]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_.*(c_+d_.*x_^2)^p_.,x_Symbol] :=
  Int[(a*c+b*d*x^4)^p,x] /;
FreeQ[{a,b,c,d,p},x] && EqQ[b*c+a*d,0] && (IntegerQ[p] || GtQ[a,0] && GtQ[c,0])


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^p_,x_Symbol] :=
  x*(a+b*x^2)^p*(c+d*x^2)^p/(4*p+1) + 
  4*a*c*p/(4*p+1) \[Star] Int[(a+b*x^2)^(p-1)*(c+d*x^2)^(p-1),x] /;
FreeQ[{a,b,c,d},x] && EqQ[b*c+a*d,0] && GtQ[p,0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^p_,x_Symbol] :=
  -x*(a+b*x^2)^(p+1)*(c+d*x^2)^(p+1)/(4*a*c*(p+1)) + 
  (4*p+5)/(4*a*c*(p+1)) \[Star] Int[(a+b*x^2)^(p+1)*(c+d*x^2)^(p+1),x] /;
FreeQ[{a,b,c,d},x] && EqQ[b*c+a*d,0] && LtQ[p,-1]


(* ::Code:: *)
Int[1/(Sqrt[a_+b_.*x_^2]*Sqrt[c_+d_.*x_^2]),x_Symbol] :=
  1/Sqrt[2*a*d]*EllipticF[ArcSin[Sqrt[2*d]*x/Sqrt[c+d*x^2]],1/2] /;
FreeQ[{a,b,c,d},x] && EqQ[b*c+a*d,0] && GtQ[a,0] && GtQ[d,0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^p_,x_Symbol] :=
  (c+d*x^2)^FracPart[p]/((-1)^IntPart[p]*(-c-d*x^2)^FracPart[p]) \[Star] Int[(-a*c-b*d*x^4)^p,x] /;
FreeQ[{a,b,c,d,p},x] && EqQ[b*c+a*d,0] && GtQ[a,0] && LtQ[c,0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^p_,x_Symbol] :=
  (a+b*x^2)^FracPart[p]*(c+d*x^2)^FracPart[p]/(a*c+b*d*x^4)^FracPart[p] \[Star] Int[(a*c+b*d*x^4)^p,x] /;
FreeQ[{a,b,c,d,p},x] && EqQ[b*c+a*d,0] && Not[IntegerQ[p]]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_.*(c_+d_.*x_^2)^q_.,x_Symbol] :=
  Int[ExpandIntegrand[(a+b*x^2)^p*(c+d*x^2)^q,x],x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && IGtQ[p,0] && IGtQ[q,0]


(* ::Code:: *)
Int[1/(Sqrt[a_+b_.*x_^2]*(c_+d_.*x_^2)),x_Symbol] :=
  Subst[Int[1/(c-(b*c-a*d)*x^2),x],x,x/Sqrt[a+b*x^2]] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_.,x_Symbol] :=
  -x*(a+b*x^2)^(p+1)*(c+d*x^2)^q/(2*a*(p+1)) - 
  c*q/(a*(p+1)) \[Star] Int[(a+b*x^2)^(p+1)*(c+d*x^2)^(q-1),x] /;
FreeQ[{a,b,c,d,p},x] && NeQ[b*c-a*d,0] && EqQ[2*(p+q+1)+1,0] && GtQ[q,0] && NeQ[p,-1]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  a^p*x/(c^(p+1)*Sqrt[c+d*x^2])*Hypergeometric2F1[1/2,-p,3/2,-(b*c-a*d)*x^2/(a*(c+d*x^2))] /;
FreeQ[{a,b,c,d,q},x] && NeQ[b*c-a*d,0] && EqQ[2*(p+q+1)+1,0] && ILtQ[p,0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  x*(a+b*x^2)^p/(c*(c*(a+b*x^2)/(a*(c+d*x^2)))^p*(c+d*x^2)^(1/2+p))*
    Hypergeometric2F1[1/2,-p,3/2,-(b*c-a*d)*x^2/(a*(c+d*x^2))] /;
FreeQ[{a,b,c,d,p,q},x] && NeQ[b*c-a*d,0] && EqQ[2*(p+q+1)+1,0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  x*(a+b*x^2)^(p+1)*(c+d*x^2)^(q+1)/(a*c) /;
FreeQ[{a,b,c,d,p,q},x] && NeQ[b*c-a*d,0] && EqQ[2*(p+q+2)+1,0] && EqQ[a*d*(p+1)+b*c*(q+1),0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  -b*x*(a+b*x^2)^(p+1)*(c+d*x^2)^(q+1)/(2*a*(p+1)*(b*c-a*d)) + 
  (b*c+2*(p+1)*(b*c-a*d))/(2*a*(p+1)*(b*c-a*d)) \[Star] Int[(a+b*x^2)^(p+1)*(c+d*x^2)^q,x] /;
FreeQ[{a,b,c,d,q},x] && NeQ[b*c-a*d,0] && EqQ[2*(p+q+2)+1,0] && (LtQ[p,-1] || Not[LtQ[q,-1]]) && NeQ[p,-1]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_.*(c_+d_.*x_^2),x_Symbol] :=
  c*x*(a+b*x^2)^(p+1)/a /;
FreeQ[{a,b,c,d,p},x] && NeQ[b*c-a*d,0] && EqQ[a*d-b*c*(2*p+3),0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2),x_Symbol] :=
  -(b*c-a*d)*x*(a+b*x^2)^(p+1)/(2*a*b*(p+1)) - 
  (a*d-b*c*(2*p+3))/(2*a*b*(p+1)) \[Star] Int[(a+b*x^2)^(p+1),x] /;
FreeQ[{a,b,c,d,p},x] && NeQ[b*c-a*d,0] && (LtQ[p,-1] || ILtQ[1/2+p,0])


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2),x_Symbol] :=
  d*x*(a+b*x^2)^(p+1)/(b*(2*p+3)) - 
  (a*d-b*c*(2*p+3))/(b*(2*p+3)) \[Star] Int[(a+b*x^2)^p,x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && NeQ[2*p+3,0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  Int[PolynomialDivide[(a+b*x^2)^p,(c+d*x^2)^(-q),x],x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && IGtQ[p,0] && ILtQ[q,0] && GeQ[p,-q]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_./(c_+d_.*x_^2),x_Symbol] :=
  b/d \[Star] Int[(a+b*x^2)^(p-1),x] - (b*c-a*d)/d \[Star] Int[(a+b*x^2)^(p-1)/(c+d*x^2),x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && GtQ[p,0] && (EqQ[p,1/2] || EqQ[Denominator[p],4] || EqQ[p,2/3] && EqQ[b*c+3*a*d,0])


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_/(c_+d_.*x_^2),x_Symbol] :=
  b/(b*c-a*d) \[Star] Int[(a+b*x^2)^p,x] - d/(b*c-a*d) \[Star] Int[(a+b*x^2)^(p+1)/(c+d*x^2),x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && LtQ[p,-1] && EqQ[Denominator[p],4] && (EqQ[p,-5/4] || EqQ[p,-7/4])


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)*(c_+d_.*x_^2)),x_Symbol] :=
  b/(b*c-a*d) \[Star] Int[1/(a+b*x^2),x] - d/(b*c-a*d) \[Star] Int[1/(c+d*x^2),x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0]


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)^(1/3)*(c_+d_.*x_^2)),x_Symbol] :=
  With[{q=Rt[b/a,2]},
  q*ArcTanh[Sqrt[3]/(q*x)]/(2*2^(2/3)*Sqrt[3]*a^(1/3)*d) + 
  q*ArcTanh[Sqrt[3]*(a^(1/3)-2^(1/3)*(a+b*x^2)^(1/3))/(a^(1/3)*q*x)]/(2*2^(2/3)*Sqrt[3]*a^(1/3)*d) + 
  q*ArcTan[q*x]/(6*2^(2/3)*a^(1/3)*d) - 
  q*ArcTan[(a^(1/3)*q*x)/(a^(1/3)+2^(1/3)*(a+b*x^2)^(1/3))]/(2*2^(2/3)*a^(1/3)*d)] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && EqQ[b*c+3*a*d,0] && PosQ[b/a]


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)^(1/3)*(c_+d_.*x_^2)),x_Symbol] :=
  With[{q=Rt[-b/a,2]},
  q*ArcTan[Sqrt[3]/(q*x)]/(2*2^(2/3)*Sqrt[3]*a^(1/3)*d) + 
  q*ArcTan[Sqrt[3]*(a^(1/3)-2^(1/3)*(a+b*x^2)^(1/3))/(a^(1/3)*q*x)]/(2*2^(2/3)*Sqrt[3]*a^(1/3)*d) - 
  q*ArcTanh[q*x]/(6*2^(2/3)*a^(1/3)*d) + 
  q*ArcTanh[(a^(1/3)*q*x)/(a^(1/3)+2^(1/3)*(a+b*x^2)^(1/3))]/(2*2^(2/3)*a^(1/3)*d)] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && EqQ[b*c+3*a*d,0] && NegQ[b/a]


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)^(1/3)*(c_+d_.*x_^2)),x_Symbol] :=
  With[{q=Rt[b/a,2]},
  q*ArcTan[q*x/3]/(12*Rt[a,3]*d) +  
  q*ArcTan[(Rt[a,3]-(a+b*x^2)^(1/3))^2/(3*Rt[a,3]^2*q*x)]/(12*Rt[a,3]*d) - 
  q*ArcTanh[(Sqrt[3]*(Rt[a,3]-(a+b*x^2)^(1/3)))/(Rt[a,3]*q*x)]/(4*Sqrt[3]*Rt[a,3]*d)] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && EqQ[b*c-9*a*d,0] && PosQ[b/a]


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)^(1/3)*(c_+d_.*x_^2)),x_Symbol] :=
  With[{q=Rt[-b/a,2]},
  -q*ArcTanh[q*x/3]/(12*Rt[a,3]*d) +  
  q*ArcTanh[(Rt[a,3]-(a+b*x^2)^(1/3))^2/(3*Rt[a,3]^2*q*x)]/(12*Rt[a,3]*d) - 
  q*ArcTan[(Sqrt[3]*(Rt[a,3]-(a+b*x^2)^(1/3)))/(Rt[a,3]*q*x)]/(4*Sqrt[3]*Rt[a,3]*d)] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && EqQ[b*c-9*a*d,0] && NegQ[b/a]


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)^(1/4)*(c_+d_.*x_^2)),x_Symbol] :=
  With[{q=Rt[b^2/a,4]},
  -b/(2*a*d*q)*ArcTan[(b+q^2*Sqrt[a+b*x^2])/(q^3*x*(a+b*x^2)^(1/4))] - 
  b/(2*a*d*q)*ArcTanh[(b-q^2*Sqrt[a+b*x^2])/(q^3*x*(a+b*x^2)^(1/4))]] /;
FreeQ[{a,b,c,d},x] && EqQ[b*c-2*a*d,0] && PosQ[b^2/a]


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)^(1/4)*(c_+d_.*x_^2)),x_Symbol] :=
  With[{q=Rt[-b^2/a,4]},
  b/(2*Sqrt[2]*a*d*q)*ArcTan[q*x/(Sqrt[2]*(a+b*x^2)^(1/4))] + 
  b/(2*Sqrt[2]*a*d*q)*ArcTanh[q*x/(Sqrt[2]*(a+b*x^2)^(1/4))]] /;
FreeQ[{a,b,c,d},x] && EqQ[b*c-2*a*d,0] && NegQ[b^2/a]


(* ::Code:: *)
(* Int[1/((a_+b_.*x_^2)^(1/4)*(c_+d_.*x_^2)),x_Symbol] :=
  With[{q=Rt[-b^2/a,4]},
  b/(2*Sqrt[2]*a*d*q)*ArcTan[q*x/(Sqrt[2]*(a+b*x^2)^(1/4))] + 
  b/(4*Sqrt[2]*a*d*q)*Log[(Sqrt[2]*q*x+2*(a+b*x^2)^(1/4))/(Sqrt[2]*q*x-2*(a+b*x^2)^(1/4))]] /;
FreeQ[{a,b,c,d},x] && EqQ[b*c-2*a*d,0] && NegQ[b^2/a] *)


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)^(1/4)*(c_+d_.*x_^2)),x_Symbol] :=
  2*Sqrt[-b*x^2/a]/x \[Star] Subst[Int[x^2/(Sqrt[1-x^4/a]*(b*c-a*d+d*x^4)),x],x,(a+b*x^2)^(1/4)] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0]


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)^(3/4)*(c_+d_.*x_^2)),x_Symbol] :=
  1/c \[Star] Int[1/(a+b*x^2)^(3/4),x] - d/c \[Star] Int[x^2/((a+b*x^2)^(3/4)*(c+d*x^2)),x] /;
FreeQ[{a,b,c,d},x] && EqQ[b*c-2*a*d,0]


(* ::Code:: *)
Int[1/((a_+b_.*x_^2)^(3/4)*(c_+d_.*x_^2)),x_Symbol] :=
  Sqrt[-b*x^2/a]/(2*x) \[Star] Subst[Int[1/(Sqrt[-b*x/a]*(a+b*x)^(3/4)*(c+d*x)),x],x,x^2] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0]


(* ::Code:: *)
Int[Sqrt[a_+b_.*x_^2]/(c_+d_.*x_^2)^(3/2),x_Symbol] :=
  Sqrt[a+b*x^2]/(c*Rt[d/c,2]*Sqrt[c+d*x^2]*Sqrt[c*(a+b*x^2)/(a*(c+d*x^2))])*EllipticE[ArcTan[Rt[d/c,2]*x],1-b*c/(a*d)] /;
FreeQ[{a,b,c,d},x] && PosQ[b/a] && PosQ[d/c]


(* ::Code:: *)
(* Int[Sqrt[a_+b_.*x_^2]/(c_+d_.*x_^2)^(3/2),x_Symbol] :=
  a*Sqrt[c+d*x^2]*Sqrt[(c*(a+b*x^2))/(a*(c+d*x^2))]/(c^2*Rt[d/c,2]*Sqrt[a+b*x^2])*EllipticE[ArcTan[Rt[d/c,2]*x],1-b*c/(a*d)] /;
FreeQ[{a,b,c,d},x] && PosQ[b/a] && PosQ[d/c] *)


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  -x*(a+b*x^2)^(p+1)*(c+d*x^2)^q/(2*a*(p+1)) + 
  1/(2*a*(p+1)) \[Star] Int[(a+b*x^2)^(p+1)*(c+d*x^2)^(q-1)*Simp[c*(2*p+3)+d*(2*(p+q+1)+1)*x^2,x],x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && LtQ[p,-1] && LtQ[0,q,1] && IntBinomialQ[a,b,c,d,2,p,q,x]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  (a*d-c*b)*x*(a+b*x^2)^(p+1)*(c+d*x^2)^(q-1)/(2*a*b*(p+1)) - 
  1/(2*a*b*(p+1)) \[Star] 
    Int[(a+b*x^2)^(p+1)*(c+d*x^2)^(q-2)*Simp[c*(a*d-c*b*(2*p+3))+d*(a*d*(2*(q-1)+1)-b*c*(2*(p+q)+1))*x^2,x],x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && LtQ[p,-1] && GtQ[q,1] && IntBinomialQ[a,b,c,d,2,p,q,x]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  -b*x*(a+b*x^2)^(p+1)*(c+d*x^2)^(q+1)/(2*a*(p+1)*(b*c-a*d)) + 
  1/(2*a*(p+1)*(b*c-a*d)) \[Star] 
    Int[(a+b*x^2)^(p+1)*(c+d*x^2)^q*Simp[b*c+2*(p+1)*(b*c-a*d)+d*b*(2*(p+q+2)+1)*x^2,x],x] /;
FreeQ[{a,b,c,d,q},x] && NeQ[b*c-a*d,0] && LtQ[p,-1] && Not[Not[IntegerQ[p]] && IntegerQ[q] && LtQ[q,-1]] && 
  IntBinomialQ[a,b,c,d,2,p,q,x]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  Int[ExpandIntegrand[(a+b*x^2)^p*(c+d*x^2)^q,x],x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && IntegersQ[p,q] && GtQ[p+q,0]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  d*x*(a+b*x^2)^(p+1)*(c+d*x^2)^(q-1)/(b*(2*(p+q)+1)) + 
  1/(b*(2*(p+q)+1)) \[Star] 
    Int[(a+b*x^2)^p*(c+d*x^2)^(q-2)*Simp[c*(b*c*(2*(p+q)+1)-a*d)+d*(b*c*(2*(p+2*q-1)+1)-a*d*(2*(q-1)+1))*x^2,x],x] /;
FreeQ[{a,b,c,d,p},x] && NeQ[b*c-a*d,0] && GtQ[q,1] && NeQ[2*(p+q)+1,0] && Not[IGtQ[p,1]] && IntBinomialQ[a,b,c,d,2,p,q,x]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  x*(a+b*x^2)^p*(c+d*x^2)^q/(2*(p+q)+1) + 
  2/(2*(p+q)+1) \[Star] Int[(a+b*x^2)^(p-1)*(c+d*x^2)^(q-1)*Simp[a*c*(p+q)+(q*(b*c-a*d)+a*d*(p+q))*x^2,x],x] /;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0] && GtQ[q,0] && GtQ[p,0] && IntBinomialQ[a,b,c,d,2,p,q,x]


(* ::Code:: *)
Int[1/(Sqrt[a_+b_.*x_^2]*Sqrt[c_+d_.*x_^2]),x_Symbol] :=
  Sqrt[a+b*x^2]/(a*Rt[d/c,2]*Sqrt[c+d*x^2]*Sqrt[c*(a+b*x^2)/(a*(c+d*x^2))])*EllipticF[ArcTan[Rt[d/c,2]*x],1-b*c/(a*d)] /;
FreeQ[{a,b,c,d},x] && PosQ[d/c] && PosQ[b/a] && Not[SimplerSqrtQ[b/a,d/c]]


(* ::Code:: *)
(* Int[1/(Sqrt[a_+b_.*x_^2]*Sqrt[c_+d_.*x_^2]),x_Symbol] :=
  Sqrt[c+d*x^2]*Sqrt[c*(a+b*x^2)/(a*(c+d*x^2))]/(c*Rt[d/c,2]*Sqrt[a+b*x^2])*EllipticF[ArcTan[Rt[d/c,2]*x],1-b*c/(a*d)] /;
FreeQ[{a,b,c,d},x] && PosQ[d/c] && PosQ[b/a] && Not[SimplerSqrtQ[b/a,d/c]] *)


(* ::Code:: *)
Int[1/(Sqrt[a_+b_.*x_^2]*Sqrt[c_+d_.*x_^2]),x_Symbol] :=
  1/(Sqrt[a]*Sqrt[c]*Rt[-d/c,2])*EllipticF[ArcSin[Rt[-d/c,2]*x],b*c/(a*d)] /;
FreeQ[{a,b,c,d},x] && NegQ[d/c] && GtQ[c,0] && GtQ[a,0] && Not[NegQ[b/a] && SimplerSqrtQ[-b/a,-d/c]]


(* ::Code:: *)
Int[1/(Sqrt[a_+b_.*x_^2]*Sqrt[c_+d_.*x_^2]),x_Symbol] :=
  -1/(Sqrt[c]*Rt[-d/c,2]*Sqrt[a-b*c/d])*EllipticF[ArcCos[Rt[-d/c,2]*x],b*c/(b*c-a*d)] /;
FreeQ[{a,b,c,d},x] && NegQ[d/c] && GtQ[c,0] && GtQ[a-b*c/d,0]


(* ::Code:: *)
Int[1/(Sqrt[a_+b_.*x_^2]*Sqrt[c_+d_.*x_^2]),x_Symbol] :=
  Sqrt[1+d/c*x^2]/Sqrt[c+d*x^2] \[Star] Int[1/(Sqrt[a+b*x^2]*Sqrt[1+d/c*x^2]),x] /;
FreeQ[{a,b,c,d},x] && Not[GtQ[c,0]]


(* ::Code:: *)
Int[Sqrt[a_+b_.*x_^2]/Sqrt[c_+d_.*x_^2],x_Symbol] :=
  a \[Star] Int[1/(Sqrt[a+b*x^2]*Sqrt[c+d*x^2]),x] + b \[Star] Int[x^2/(Sqrt[a+b*x^2]*Sqrt[c+d*x^2]),x] /;
FreeQ[{a,b,c,d},x] && PosQ[d/c] && PosQ[b/a]


(* ::Code:: *)
Int[Sqrt[a_+b_.*x_^2]/Sqrt[c_+d_.*x_^2],x_Symbol] :=
  x*Sqrt[a+b*x^2]/Sqrt[c+d*x^2] + Sqrt[-2*a]*x/Sqrt[d*x^2]*EllipticE[ArcSin[Sqrt[2*c]/Sqrt[c+d*x^2]],1/2] /;
FreeQ[{a,b,c,d},x] && PosQ[d/c] && EqQ[b*c+a*d,0] && LtQ[a,0] && GtQ[c,0]


(* ::Code:: *)
Int[Sqrt[a_+b_.*x_^2]/Sqrt[c_+d_.*x_^2],x_Symbol] :=
  b/d \[Star] Int[Sqrt[c+d*x^2]/Sqrt[a+b*x^2],x] - (b*c-a*d)/d \[Star] Int[1/(Sqrt[a+b*x^2]*Sqrt[c+d*x^2]),x] /;
FreeQ[{a,b,c,d},x] && PosQ[d/c] && NegQ[b/a]


(* ::Code:: *)
Int[Sqrt[a_+b_.*x_^2]/Sqrt[c_+d_.*x_^2],x_Symbol] :=
  Sqrt[a]/(Sqrt[c]*Rt[-d/c,2])*EllipticE[ArcSin[Rt[-d/c,2]*x],b*c/(a*d)] /;
FreeQ[{a,b,c,d},x] && NegQ[d/c] && GtQ[c,0] && GtQ[a,0]


(* ::Code:: *)
Int[Sqrt[a_+b_.*x_^2]/Sqrt[c_+d_.*x_^2],x_Symbol] :=
  -Sqrt[a-b*c/d]/(Sqrt[c]*Rt[-d/c,2])*EllipticE[ArcCos[Rt[-d/c,2]*x],b*c/(b*c-a*d)] /;
FreeQ[{a,b,c,d},x] && NegQ[d/c] && GtQ[c,0] && GtQ[a-b*c/d,0]


(* ::Code:: *)
Int[Sqrt[a_+b_.*x_^2]/Sqrt[c_+d_.*x_^2],x_Symbol] :=
  a*Sqrt[1-b^2*x^4/a^2]/(Sqrt[a+b*x^2]*Sqrt[c+d*x^2]) \[Star] Int[Sqrt[1+b*x^2/a]/Sqrt[1-b*x^2/a],x] /;
FreeQ[{a,b,c,d},x] && EqQ[b*c+a*d,0] && Not[LtQ[a*c,0] && GtQ[a*b,0]]


(* ::Code:: *)
Int[Sqrt[a_+b_.*x_^2]/Sqrt[c_+d_.*x_^2],x_Symbol] :=
  Sqrt[a+b*x^2]/Sqrt[1+b/a*x^2] \[Star] Int[Sqrt[1+b/a*x^2]/Sqrt[c+d*x^2],x] /;
FreeQ[{a,b,c,d},x] && NegQ[d/c] && GtQ[c,0] && Not[GtQ[a,0]]


(* ::Code:: *)
Int[Sqrt[a_+b_.*x_^2]/Sqrt[c_+d_.*x_^2],x_Symbol] :=
  Sqrt[1+d/c*x^2]/Sqrt[c+d*x^2] \[Star] Int[Sqrt[a+b*x^2]/Sqrt[1+d/c*x^2],x] /;
FreeQ[{a,b,c,d},x] && NegQ[d/c] && Not[GtQ[c,0]]


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_.*(c_+d_.*x_^2)^q_,x_Symbol] :=
  Int[ExpandIntegrand[(a+b*x^2)^p*(c+d*x^2)^q,x],x] /;
FreeQ[{a,b,c,d,q},x] && NeQ[b*c-a*d,0] && IGtQ[p,0]


(* ::Code:: *)
(* Int[(a_+b_.*x_^2)^p_.*(c_+d_.*x_^2)^q_.,x_Symbol] :=
  Sqrt[x^2]/(2*x) \[Star] Subst[Int[(a+b*x)^p*(c+d*x)^q/Sqrt[x],x],x,x^2] /;
FreeQ[{a,b,c,d,p,q},x] && NeQ[b*c-a*d,0] *)


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  a^p*c^q*x*AppellF1[1/2,-p,-q,3/2,-b*x^2/a,-d*x^2/c] /;
FreeQ[{a,b,c,d,p,q},x] && NeQ[b*c-a*d,0] && (IntegerQ[p] || GtQ[a,0]) && (IntegerQ[q] || GtQ[c,0])


(* ::Code:: *)
Int[(a_+b_.*x_^2)^p_*(c_+d_.*x_^2)^q_,x_Symbol] :=
  a^IntPart[p]*(a+b*x^2)^FracPart[p]/(1+b*x^2/a)^FracPart[p] \[Star] Int[(1+b*x^2/a)^p*(c+d*x^2)^q,x] /;
FreeQ[{a,b,c,d,p,q},x] && NeQ[b*c-a*d,0] && Not[IntegerQ[p] || GtQ[a,0]]



