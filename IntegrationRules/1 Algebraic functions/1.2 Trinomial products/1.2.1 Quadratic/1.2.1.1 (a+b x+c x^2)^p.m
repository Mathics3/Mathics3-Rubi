
(* ::Subsection::Closed:: *)
(* 1.2.1.1 (a+b x+c x^2)^p *)
Int[(a_ + b_.*x_ + c_.*x_^2)^p_, x_Symbol] := 2*(a + b*x + c*x^2)^(p + 1)/((2*p + 1)*(b + 2*c*x)) /; FreeQ[{a, b, c, p}, x] && EqQ[b^2 - 4*a*c, 0] && LtQ[p, -1]
Int[1/Sqrt[a_ + b_.*x_ + c_.*x_^2], x_Symbol] := (b/2 + c*x)/Sqrt[a + b*x + c*x^2]*Int[1/(b/2 + c*x), x] /; FreeQ[{a, b, c}, x] && EqQ[b^2 - 4*a*c, 0]
Int[(a_ + b_.*x_ + c_.*x_^2)^p_, x_Symbol] := (b + 2*c*x)*(a + b*x + c*x^2)^p/(2*c*(2*p + 1)) /; FreeQ[{a, b, c, p}, x] && EqQ[b^2 - 4*a*c, 0] && NeQ[p, -1/2]
Int[(a_ + b_.*x_ + c_.*x_^2)^p_, x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, 1/c^p* Int[Simp[b/2 - q/2 + c*x, x]^p*Simp[b/2 + q/2 + c*x, x]^p, x]] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && IGtQ[p, 0] && PerfectSquareQ[b^2 - 4*a*c]
Int[(a_. + b_.*x_ + c_.*x_^2)^p_, x_Symbol] := Int[ExpandIntegrand[(a + b*x + c*x^2)^p, x], x] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && IGtQ[p, 0] && (EqQ[a, 0] || Not[PerfectSquareQ[b^2 - 4*a*c]])
Int[(a_. + b_.*x_ + c_.*x_^2)^p_, x_Symbol] := (b + 2*c*x)*(a + b*x + c*x^2)^p/(2*c*(2*p + 1)) - p*(b^2 - 4*a*c)/(2*c*(2*p + 1))* Int[(a + b*x + c*x^2)^(p - 1), x] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && GtQ[p, 0] && IntegerQ[4*p]
Int[1/(a_. + b_.*x_ + c_.*x_^2)^(3/2), x_Symbol] := -2*(b + 2*c*x)/((b^2 - 4*a*c)*Sqrt[a + b*x + c*x^2]) /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0]
Int[(a_. + b_.*x_ + c_.*x_^2)^p_, x_Symbol] := (b + 2*c*x)*(a + b*x + c*x^2)^(p + 1)/((p + 1)*(b^2 - 4*a*c)) - 2*c*(2*p + 3)/((p + 1)*(b^2 - 4*a*c))* Int[(a + b*x + c*x^2)^(p + 1), x] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && LtQ[p, -1] && NeQ[p, -3/2] && IntegerQ[4*p]
Int[1/(b_.*x_ + c_.*x_^2), x_Symbol] := Log[x]/b - Log[RemoveContent[b + c*x, x]]/b /; FreeQ[{b, c}, x]
Int[1/(a_. + b_.*x_ + c_.*x_^2), x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, c/q*Int[1/Simp[b/2 - q/2 + c*x, x], x] - c/q*Int[1/Simp[b/2 + q/2 + c*x, x], x]] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && PosQ[b^2 - 4*a*c] && PerfectSquareQ[b^2 - 4*a*c]
Int[1/(a_ + b_.*x_ + c_.*x_^2), x_Symbol] := With[{q = 1 - 4*Simplify[a*c/b^2]}, -2/b*Subst[Int[1/(q - x^2), x], x, 1 + 2*c*x/b] /; RationalQ[q] && (EqQ[q^2, 1] || Not[RationalQ[b^2 - 4*a*c]])] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0]
Int[1/(a_. + b_.*x_ + c_.*x_^2), x_Symbol] := -2*Subst[Int[1/Simp[b^2 - 4*a*c - x^2, x], x], x, b + 2*c*x] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0]
Int[(a_. + b_.*x_ + c_.*x_^2)^p_, x_Symbol] := 1/(2*c*(-4*c/(b^2 - 4*a*c))^p)* Subst[Int[Simp[1 - x^2/(b^2 - 4*a*c), x]^p, x], x, b + 2*c*x] /; FreeQ[{a, b, c, p}, x] && GtQ[4*a - b^2/c, 0]
Int[1/Sqrt[b_.*x_ + c_.*x_^2], x_Symbol] := 2*Subst[Int[1/(1 - c*x^2), x], x, x/Sqrt[b*x + c*x^2]] /; FreeQ[{b, c}, x]
Int[1/Sqrt[a_ + b_.*x_ + c_.*x_^2], x_Symbol] := 2*Subst[Int[1/(4*c - x^2), x], x, (b + 2*c*x)/Sqrt[a + b*x + c*x^2]] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0]
Int[(b_.*x_ + c_.*x_^2)^p_, x_Symbol] := (b*x + c*x^2)^p/(-c*(b*x + c*x^2)/(b^2))^p* Int[(-c*x/b - c^2*x^2/b^2)^p, x] /; FreeQ[{b, c}, x] && RationalQ[p] && 3 <= Denominator[p] <= 4
(* Int[(a_.+b_.*x_+c_.*x_^2)^p_,x_Symbol] := (a+b*x+c*x^2)^p/(-c*(a+b*x+c*x^2)/(b^2-4*a*c))^p*Int[(-a*c/(b^2-4*a* c)-b*c*x/(b^2-4*a*c)-c^2*x^2/(b^2-4*a*c))^p,x] /; FreeQ[{a,b,c},x] && NeQ[b^2-4*a*c,0] && RationalQ[p] &&  3<=Denominator[p]<=4 *)
Int[(a_. + b_.*x_ + c_.*x_^2)^p_, x_Symbol] := With[{d = Denominator[p]}, d*Sqrt[(b + 2*c*x)^2]/(b + 2*c*x)* Subst[Int[x^(d*(p + 1) - 1)/Sqrt[b^2 - 4*a*c + 4*c*x^d], x], x, (a + b*x + c*x^2)^(1/d)] /; 3 <= d <= 4] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && RationalQ[p]
Int[(a_. + b_.*x_ + c_.*x_^2)^p_, x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, -(a + b*x + c*x^2)^(p + 1)/(q*(p + 1)*((q - b - 2*c*x)/(2*q))^(p + 1))* Hypergeometric2F1[-p, p + 1, p + 2, (b + q + 2*c*x)/(2*q)]] /; FreeQ[{a, b, c, p}, x] && NeQ[b^2 - 4*a*c, 0] && Not[IntegerQ[4*p]]
Int[(a_. + b_.*u_ + c_.*u_^2)^p_, x_Symbol] := 1/Coefficient[u, x, 1]*Subst[Int[(a + b*x + c*x^2)^p, x], x, u] /; FreeQ[{a, b, c, p}, x] && LinearQ[u, x] && NeQ[u, x]
