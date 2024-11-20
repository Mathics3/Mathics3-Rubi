
(* ::Subsection::Closed:: *)
(* 1.2.3.1 (a+b x^n+c x^(2 n))^p *)
Int[(a_ + b_.*x_^n_ + c_.*x_^n2_.)^p_., x_Symbol] := Int[x^(2*n*p)*(c + b*x^(-n) + a*x^(-2*n))^p, x] /; FreeQ[{a, b, c}, x] && EqQ[n2, 2*n] && LtQ[n, 0] && IntegerQ[p]
Int[(a_ + b_.*x_^n_ + c_.*x_^n2_.)^p_, x_Symbol] := With[{k = Denominator[n]}, k*Subst[Int[x^(k - 1)*(a + b*x^(k*n) + c*x^(2*k*n))^p, x], x, x^(1/k)]] /; FreeQ[{a, b, c, p}, x] && EqQ[n2, 2*n] && FractionQ[n]
Int[(a_ + b_.*x_^n_ + c_.*x_^n2_.)^p_, x_Symbol] := -Subst[Int[(a + b*x^(-n) + c*x^(-2*n))^p/x^2, x], x, 1/x] /; FreeQ[{a, b, c, p}, x] && EqQ[n2, 2*n] && ILtQ[n, 0]
Int[(a_ + b_.*x_^n_. + c_.*x_^n2_.)^p_, x_Symbol] := (a + b*x^n + c*x^(2*n))^p/(b + 2*c*x^n)^(2*p)* Int[(b + 2*c*x^n)^(2*p), x] /; FreeQ[{a, b, c, n, p}, x] && EqQ[n2, 2*n] && EqQ[b^2 - 4*a*c, 0]
Int[(a_ + b_.*x_^n_ + c_.*x_^n2_.)^p_, x_Symbol] := Int[ExpandIntegrand[(a + b*x^n + c*x^(2*n))^p, x], x] /; FreeQ[{a, b, c, n}, x] && EqQ[n2, 2*n] && NeQ[b^2 - 4*a*c, 0] && IGtQ[p, 0]
Int[(a_ + b_.*x_^n_ + c_.*x_^n2_.)^p_, x_Symbol] := -x*(b^2 - 2*a*c + b*c*x^n)*(a + b*x^n + c*x^(2*n))^(p + 1)/(a* n*(p + 1)*(b^2 - 4*a*c)) + 1/(a*n*(p + 1)*(b^2 - 4*a*c))* Int[(b^2 - 2*a*c + n*(p + 1)*(b^2 - 4*a*c) + b*c*(n*(2*p + 3) + 1)*x^n)*(a + b*x^n + c*x^(2*n))^(p + 1), x] /; FreeQ[{a, b, c, n}, x] && EqQ[n2, 2*n] && NeQ[b^2 - 4*a*c, 0] && ILtQ[p, -1]
Int[1/(a_ + b_.*x_^n_ + c_.*x_^n2_), x_Symbol] := With[{q = Rt[a/c, 2]}, With[{r = Rt[2*q - b/c, 2]}, 1/(2*c*q*r)*Int[(r - x^(n/2))/(q - r*x^(n/2) + x^n), x] + 1/(2*c*q*r)*Int[(r + x^(n/2))/(q + r*x^(n/2) + x^n), x]]] /; FreeQ[{a, b, c}, x] && EqQ[n2, 2*n] && NeQ[b^2 - 4*a*c, 0] && IGtQ[n/2, 0] && NegQ[b^2 - 4*a*c]
Int[1/(a_ + b_.*x_^n_ + c_.*x_^n2_), x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, c/q*Int[1/(b/2 - q/2 + c*x^n), x] - c/q*Int[1/(b/2 + q/2 + c*x^n), x]] /; FreeQ[{a, b, c}, x] && EqQ[n2, 2*n] && NeQ[b^2 - 4*a*c, 0]
Int[(a_ + b_.*x_^n_ + c_.*x_^n2_.)^p_, x_Symbol] := a^IntPart[p]*(a + b*x^n + c*x^(2*n))^FracPart[p]/ ((1 + 2*c*x^n/(b + Rt[b^2 - 4*a*c, 2]))^ FracPart[p]*(1 + 2*c*x^n/(b - Rt[b^2 - 4*a*c, 2]))^FracPart[p])* Int[(1 + 2*c*x^n/(b + Sqrt[b^2 - 4*a*c]))^ p*(1 + 2*c*x^n/(b - Sqrt[b^2 - 4*a*c]))^p, x] /; FreeQ[{a, b, c, n, p}, x] && EqQ[n2, 2*n] && NeQ[b^2 - 4*a*c, 0] && Not[IntegerQ[p]]
Int[(a_ + b_.*u_^n_ + c_.*u_^n2_.)^p_, x_Symbol] := 1/Coefficient[u, x, 1]* Subst[Int[(a + b*x^n + c*x^(2*n))^p, x], x, u] /; FreeQ[{a, b, c, n, p}, x] && EqQ[n2, 2*n] && LinearQ[u, x] && NeQ[u, x]
Int[(a_ + b_.*x_^mn_ + c_.*x_^n_.)^p_., x_Symbol] := Int[(b + a*x^n + c*x^(2*n))^p/x^(n*p), x] /; FreeQ[{a, b, c, n}, x] && EqQ[mn, -n] && IntegerQ[p] && PosQ[n]
Int[(a_ + b_.*x_^mn_ + c_.*x_^n_.)^p_, x_Symbol] := x^(n*FracPart[p])*(a + b*x^(-n) + c*x^n)^ FracPart[p]/(b + a*x^n + c*x^(2*n))^FracPart[p]* Int[(b + a*x^n + c*x^(2*n))^p/x^(n*p), x] /; FreeQ[{a, b, c, n, p}, x] && EqQ[mn, -n] && Not[IntegerQ[p]] && PosQ[n]
