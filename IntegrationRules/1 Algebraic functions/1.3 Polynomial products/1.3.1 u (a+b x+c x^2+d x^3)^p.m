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
Int[u_.*Px_^p_,x_Symbol] :=
  With[{b=Coeff[Px,x,1],c=Coeff[Px,x,2],d=Coeff[Px,x,3]},
  Px^FracPart[p]/(x^FracPart[p]*(b+c*x+d*x^2)^FracPart[p]) \[Star] Int[u*x^p*(b+c*x+d*x^2)^p,x]] /;
FreeQ[p,x] && PolyQ[Px,x,3] && EqQ[Coeff[Px,x,0],0] && Not[IntegerQ[p]]


(* ::Code:: *)
Int[(a_.+b_.*x_+d_.*x_^3)^p_,x_Symbol] :=
  1/(3^(3*p)*a^(2*p)) \[Star] Int[(3*a-b*x)^p*(3*a+2*b*x)^(2*p),x] /;
FreeQ[{a,b,d},x] && EqQ[4*b^3+27*a^2*d,0] && IntegerQ[p]


(* ::Code:: *)
Int[(a_.+b_.*x_+d_.*x_^3)^p_,x_Symbol] :=
  (a+b*x+d*x^3)^p/((3*a-b*x)^p*(3*a+2*b*x)^(2*p)) \[Star] Int[(3*a-b*x)^p*(3*a+2*b*x)^(2*p),x] /;
FreeQ[{a,b,d,p},x] && EqQ[4*b^3+27*a^2*d,0] && Not[IntegerQ[p]]


(* ::Code:: *)
Int[(a_.+b_.*x_+d_.*x_^3)^p_,x_Symbol] :=
  With[{r=Rt[-9*a*d^2+Sqrt[3]*d*Sqrt[4*b^3*d+27*a^2*d^2],3]},
  1/d^(2*p) \[Star] Int[Simp[18^(1/3)*b*d/(3*r)-r/18^(1/3)+d*x,x]^p*
    Simp[b*d/3+12^(1/3)*b^2*d^2/(3*r^2)+r^2/(3*12^(1/3))-d*(2^(1/3)*b*d/(3^(1/3)*r)-r/18^(1/3))*x+d^2*x^2,x]^p,x]] /;
FreeQ[{a,b,d},x] && NeQ[4*b^3+27*a^2*d,0] && IntegerQ[p]


(* ::Code:: *)
Int[(a_.+b_.*x_+d_.*x_^3)^p_,x_Symbol] :=
  With[{r=Rt[-9*a*d^2+Sqrt[3]*d*Sqrt[4*b^3*d+27*a^2*d^2],3]},
  (a+b*x+d*x^3)^p/
    (Simp[18^(1/3)*b*d/(3*r)-r/18^(1/3)+d*x,x]^p*
      Simp[b*d/3+12^(1/3)*b^2*d^2/(3*r^2)+r^2/(3*12^(1/3))-d*(2^(1/3)*b*d/(3^(1/3)*r)-r/18^(1/3))*x+d^2*x^2,x]^p) \[Star] 
    Int[Simp[18^(1/3)*b*d/(3*r)-r/18^(1/3)+d*x,x]^p*
      Simp[b*d/3+12^(1/3)*b^2*d^2/(3*r^2)+r^2/(3*12^(1/3))-d*(2^(1/3)*b*d/(3^(1/3)*r)-r/18^(1/3))*x+d^2*x^2,x]^p,x]] /;
FreeQ[{a,b,d,p},x] && NeQ[4*b^3+27*a^2*d,0] && Not[IntegerQ[p]]


(* ::Code:: *)
Int[Px_^p_,x_Symbol] :=
  With[{a=Coeff[Px,x,0],b=Coeff[Px,x,1],c=Coeff[Px,x,2],d=Coeff[Px,x,3]},
  1/d^p \[Star] Int[(c+d*x)^p*(b+d*x^2)^p,x] /;
 EqQ[b*c-a*d,0]] /;
PolyQ[Px,x,3] && IntegerQ[p]


(* ::Code:: *)
Int[Px_^p_,x_Symbol] :=
  With[{a=Coeff[Px,x,0],b=Coeff[Px,x,1],c=Coeff[Px,x,2],d=Coeff[Px,x,3]},
  Px^p/((c+d*x)^p*(b+d*x^2)^p) \[Star] Int[(c+d*x)^p*(b+d*x^2)^p,x] /;
 EqQ[b*c-a*d,0]] /;
FreeQ[p,x] && PolyQ[Px,x,3] && Not[IntegerQ[p]]


(* ::Code:: *)
Int[Px_^p_,x_Symbol] :=
  With[{a=Coeff[Px,x,0],b=Coeff[Px,x,1],c=Coeff[Px,x,2],d=Coeff[Px,x,3]},
  Subst[Int[Simp[a-b^2/(3*c)+d*x^3,x]^p,x],x,c/(3*d)+x] /;
 EqQ[c^2-3*b*d,0]] /;
FreeQ[p,x] && PolyQ[Px,x,3]


(* ::Code:: *)
Int[Px_^p_,x_Symbol] :=
  With[{a=Coeff[Px,x,0],b=Coeff[Px,x,1],c=Coeff[Px,x,2],d=Coeff[Px,x,3]},
  1/(4^p*(c^2-3*b*d)^(3*p)) \[Star] Int[(c^3-4*b*c*d+9*a*d^2+d*(c^2-3*b*d)*x)^p*(b*c-9*a*d+2*(c^2-3*b*d)*x)^(2*p),x] /;
 EqQ[b^2*c^2-4*a*c^3-4*b^3*d+18*a*b*c*d-27*a^2*d^2,0] && NeQ[c^2-3*b*d,0]] /;
FreeQ[p,x] && PolyQ[Px,x,3] && IntegerQ[p]


(* ::Code:: *)
Int[Px_^p_,x_Symbol] :=
  With[{a=Coeff[Px,x,0],b=Coeff[Px,x,1],c=Coeff[Px,x,2],d=Coeff[Px,x,3]},
  Px^p/((c^3-4*b*c*d+9*a*d^2+d*(c^2-3*b*d)*x)^p*(b*c-9*a*d+2*(c^2-3*b*d)*x)^(2*p)) \[Star] 
    Int[(c^3-4*b*c*d+9*a*d^2+d*(c^2-3*b*d)*x)^p*(b*c-9*a*d+2*(c^2-3*b*d)*x)^(2*p),x] /;
 EqQ[b^2*c^2-4*a*c^3-4*b^3*d+18*a*b*c*d-27*a^2*d^2,0] && NeQ[c^2-3*b*d,0]] /;
FreeQ[p,x] && PolyQ[Px,x,3] && Not[IntegerQ[p]]


(* ::Code:: *)
Int[Px_^p_,x_Symbol] :=
  With[{a=Coeff[Px,x,0],b=Coeff[Px,x,1],c=Coeff[Px,x,2],d=Coeff[Px,x,3]},
  Subst[Int[Simp[(2*c^3-9*b*c*d+27*a*d^2)/(27*d^2)-(c^2-3*b*d)*x/(3*d)+d*x^3,x]^p,x],x,c/(3*d)+x]] /;
FreeQ[p,x] && PolyQ[Px,x,3]


(* ::Code:: *)
Int[(e_.+f_.*x_)^m_.*(a_+b_.*x_+d_.*x_^3)^p_.,x_Symbol] :=
  1/(3^(3*p)*a^(2*p)) \[Star] Int[(e+f*x)^m*(3*a-b*x)^p*(3*a+2*b*x)^(2*p),x] /;
FreeQ[{a,b,d,e,f,m},x] && EqQ[4*b^3+27*a^2*d,0] && IntegerQ[p]


(* ::Code:: *)
Int[(e_.+f_.*x_)^m_.*(a_+b_.*x_+d_.*x_^3)^p_,x_Symbol] :=
  (a+b*x+d*x^3)^p/((3*a-b*x)^p*(3*a+2*b*x)^(2*p)) \[Star] Int[(e+f*x)^m*(3*a-b*x)^p*(3*a+2*b*x)^(2*p),x] /;
FreeQ[{a,b,d,e,f,m,p},x] && EqQ[4*b^3+27*a^2*d,0] && Not[IntegerQ[p]]


(* ::Code:: *)
Int[(e_.+f_.*x_)^m_.*(a_+b_.*x_+d_.*x_^3)^p_.,x_Symbol] :=
  Int[ExpandIntegrand[(e+f*x)^m*(a+b*x+d*x^3)^p,x],x] /;
FreeQ[{a,b,d,e,f,m},x] && NeQ[4*b^3+27*a^2*d,0] && IGtQ[p,0]


(* ::Code:: *)
Int[(e_.+f_.*x_)^m_.*(a_+b_.*x_+d_.*x_^3)^p_,x_Symbol] :=
  With[{r=Rt[-9*a*d^2+Sqrt[3]*d*Sqrt[4*b^3*d+27*a^2*d^2],3]},
  1/d^(2*p) \[Star] Int[(e+f*x)^m*Simp[18^(1/3)*b*d/(3*r)-r/18^(1/3)+d*x,x]^p*
    Simp[b*d/3+12^(1/3)*b^2*d^2/(3*r^2)+r^2/(3*12^(1/3))-d*(2^(1/3)*b*d/(3^(1/3)*r)-r/18^(1/3))*x+d^2*x^2,x]^p,x]] /;
FreeQ[{a,b,d,e,f,m},x] && NeQ[4*b^3+27*a^2*d,0] && ILtQ[p,0]


(* ::Code:: *)
Int[(e_.+f_.*x_)^m_.*(a_+b_.*x_+d_.*x_^3)^p_,x_Symbol] :=
  With[{r=Rt[-9*a*d^2+Sqrt[3]*d*Sqrt[4*b^3*d+27*a^2*d^2],3]},
  (a+b*x+d*x^3)^p/
    (Simp[18^(1/3)*b*d/(3*r)-r/18^(1/3)+d*x,x]^p*
      Simp[b*d/3+12^(1/3)*b^2*d^2/(3*r^2)+r^2/(3*12^(1/3))-d*(2^(1/3)*b*d/(3^(1/3)*r)-r/18^(1/3))*x+d^2*x^2,x]^p) \[Star] 
    Int[(e+f*x)^m*Simp[18^(1/3)*b*d/(3*r)-r/18^(1/3)+d*x,x]^p*
      Simp[b*d/3+12^(1/3)*b^2*d^2/(3*r^2)+r^2/(3*12^(1/3))-d*(2^(1/3)*b*d/(3^(1/3)*r)-r/18^(1/3))*x+d^2*x^2,x]^p,x]] /;
FreeQ[{a,b,d,e,f,m,p},x] && NeQ[4*b^3+27*a^2*d,0] && Not[IntegerQ[p]]


(* ::Code:: *)
Int[(e_.+f_.*x_)^m_.*(a_.+b_.*x_+c_.*x_^2+d_.*x_^3)^p_,x_Symbol] :=
  Subst[Int[((3*d*e-c*f)/(3*d)+f*x)^m*Simp[a-b^2/(3*c)+d*x^3,x]^p,x],x,x+c/(3*d)] /;
FreeQ[{a,b,c,d,e,f,m,p},x] && EqQ[c^2-3*b*d,0]


(* ::Code:: *)
Int[(e_.+f_.*x_)^m_.*(a_.+b_.*x_+c_.*x_^2+d_.*x_^3)^p_,x_Symbol] :=
  1/(4^p*(c^2-3*b*d)^(3*p)) \[Star] Int[(e+f*x)^m*(c^3-4*b*c*d+9*a*d^2+d*(c^2-3*b*d)*x)^p*(b*c-9*a*d+2*(c^2-3*b*d)*x)^(2*p),x] /;
FreeQ[{a,b,c,d,e,f,m,p},x] && NeQ[c^2-3*b*d,0] && EqQ[b^2*c^2-4*a*c^3-4*b^3*d+18*a*b*c*d-27*a^2*d^2,0] && ILtQ[p,0]


(* ::Code:: *)
Int[(e_.+f_.*x_)^m_.*(a_.+b_.*x_+c_.*x_^2+d_.*x_^3)^p_,x_Symbol] :=
  (a+b*x+c*x^2+d*x^3)^p/((c^3-4*b*c*d+9*a*d^2+d*(c^2-3*b*d)*x)^p*(b*c-9*a*d+2*(c^2-3*b*d)*x)^(2*p)) \[Star] 
    Int[(e+f*x)^m*(c^3-4*b*c*d+9*a*d^2+d*(c^2-3*b*d)*x)^p*(b*c-9*a*d+2*(c^2-3*b*d)*x)^(2*p),x] /;
FreeQ[{a,b,c,d,e,f,m,p},x] && NeQ[c^2-3*b*d,0] && EqQ[b^2*c^2-4*a*c^3-4*b^3*d+18*a*b*c*d-27*a^2*d^2,0] && Not[IntegerQ[p]]


(* ::Code:: *)
Int[(e_.+f_.*x_)^m_.*P3_^p_.,x_Symbol] :=
  With[{a=Coeff[P3,x,0],b=Coeff[P3,x,1],c=Coeff[P3,x,2],d=Coeff[P3,x,3]},
  Subst[Int[((3*d*e-c*f)/(3*d)+f*x)^m*Simp[(2*c^3-9*b*c*d+27*a*d^2)/(27*d^2)-(c^2-3*b*d)*x/(3*d)+d*x^3,x]^p,x],x,x+c/(3*d)] /;
 NeQ[c,0]] /;
FreeQ[{e,f,m,p},x] && PolyQ[P3,x,3]



