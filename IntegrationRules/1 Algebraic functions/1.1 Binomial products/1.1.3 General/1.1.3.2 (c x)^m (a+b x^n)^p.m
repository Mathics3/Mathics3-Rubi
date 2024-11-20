
(* ::Subsection::Closed:: *)
(* 1.1.3.2 (c x)^m (a+b x^n)^p *)
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := Int[(c*x)^m*(a1*a2 + b1*b2*x^(2*n))^p, x] /; FreeQ[{a1, b1, a2, b2, c, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && (IntegerQ[p] || GtQ[a1, 0] && GtQ[a2, 0])
Int[x_^m_./(a_ + b_.*x_^n_), x_Symbol] := Log[RemoveContent[a + b*x^n, x]]/(b*n) /; FreeQ[{a, b, m, n}, x] && EqQ[m, n - 1]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := (a + b*x^n)^(p + 1)/(b*n*(p + 1)) /; FreeQ[{a, b, m, n, p}, x] && EqQ[m, n - 1] && NeQ[p, -1]
Int[x_^m_.*(a1_ + b1_.*x_^n_.)^p_*(a2_ + b2_.*x_^n_.)^p_, x_Symbol] := (a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(2*b1*b2*n*(p + 1)) /; FreeQ[{a1, b1, a2, b2, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && EqQ[m, 2*n - 1] && NeQ[p, -1]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := Int[x^(m + n*p)*(b + a*x^(-n))^p, x] /; FreeQ[{a, b, m, n}, x] && IntegerQ[p] && NegQ[n]
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a + b*x^n)^(p + 1)/(a*c*(m + 1)) /; FreeQ[{a, b, c, m, n, p}, x] && EqQ[(m + 1)/n + p + 1, 0] && NeQ[m, -1]
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(a1*a2* c*(m + 1)) /; FreeQ[{a1, b1, a2, b2, c, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && EqQ[(m + 1)/(2*n) + p + 1, 0] && NeQ[m, -1]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := 1/n*Subst[Int[x^(Simplify[(m + 1)/n] - 1)*(a + b*x)^p, x], x, x^n] /; FreeQ[{a, b, m, n, p}, x] && IntegerQ[Simplify[(m + 1)/n]]
Int[x_^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := 1/n*Subst[ Int[x^(Simplify[(m + 1)/n] - 1)*(a1 + b1*x)^p*(a2 + b2*x)^p, x], x, x^n] /; FreeQ[{a1, b1, a2, b2, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IntegerQ[Simplify[(m + 1)/(2*n)]]
Int[(c_*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a + b*x^n)^p, x] /; FreeQ[{a, b, c, m, n, p}, x] && IntegerQ[Simplify[(m + 1)/n]]
Int[(c_*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, c, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IntegerQ[Simplify[(m + 1)/(2*n)]]
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_., x_Symbol] := Int[ExpandIntegrand[(c*x)^m*(a + b*x^n)^p, x], x] /; FreeQ[{a, b, c, m, n}, x] && IGtQ[p, 0]
Int[x_^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := x^(m + 1)*(a + b*x^n)^(p + 1)/(a*(m + 1)) - b*(m + n*(p + 1) + 1)/(a*(m + 1))* Int[x^(m + n)*(a + b*x^n)^p, x] /; FreeQ[{a, b, m, n, p}, x] && ILtQ[Simplify[(m + 1)/n + p + 1], 0] && NeQ[m, -1]
Int[x_^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := x^(m + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(a1* a2*(m + 1)) - b1*b2*(m + 2*n*(p + 1) + 1)/(a1*a2*(m + 1))* Int[x^(m + 2*n)*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && ILtQ[Simplify[(m + 1)/(2*n) + p + 1], 0] && NeQ[m, -1]
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := -(c*x)^(m + 1)*(a + b*x^n)^(p + 1)/(a*c*n*(p + 1)) + (m + n*(p + 1) + 1)/(a*n*(p + 1))* Int[(c*x)^m*(a + b*x^n)^(p + 1), x] /; FreeQ[{a, b, c, m, n, p}, x] && ILtQ[Simplify[(m + 1)/n + p + 1], 0] && NeQ[p, -1]
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := -(c*x)^(m + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(2*a1* a2*c*n*(p + 1)) + (m + 2*n*(p + 1) + 1)/(2*a1*a2*n*(p + 1))* Int[(c*x)^m*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1), x] /; FreeQ[{a1, b1, a2, b2, c, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && ILtQ[Simplify[(m + 1)/(2*n) + p + 1], 0] && NeQ[p, -1]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := With[{k = GCD[m + 1, n]}, 1/k*Subst[Int[x^((m + 1)/k - 1)*(a + b*x^(n/k))^p, x], x, x^k] /; k != 1] /; FreeQ[{a, b, p}, x] && IGtQ[n, 0] && IntegerQ[m]
Int[x_^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := With[{k = GCD[m + 1, 2*n]}, 1/k* Subst[Int[ x^((m + 1)/k - 1)*(a1 + b1*x^(n/k))^p*(a2 + b2*x^(n/k))^p, x], x, x^k] /; k != 1] /; FreeQ[{a1, b1, a2, b2, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && IntegerQ[m]
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a + b*x^n)^p/(c*(m + 1)) - b*n*p/(c^n*(m + 1))*Int[(c*x)^(m + n)*(a + b*x^n)^(p - 1), x] /; FreeQ[{a, b, c}, x] && IGtQ[n, 0] && GtQ[p, 0] && LtQ[m, -1] && Not[ILtQ[(m + n*p + n + 1)/n, 0]] && IntBinomialQ[a, b, c, n, m, p, x]
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p/(c*(m + 1)) - 2*b1*b2*n*p/(c^(2*n)*(m + 1))* Int[(c*x)^(m + 2*n)*(a1 + b1*x^n)^(p - 1)*(a2 + b2*x^n)^(p - 1), x] /; FreeQ[{a1, b1, a2, b2, c, m}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && GtQ[p, 0] && LtQ[m, -1] && NeQ[m + 2*n*p + 1, 0] && IntBinomialQ[a1*a2, b1*b2, c, 2*n, m, p, x]
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a + b*x^n)^p/(c*(m + n*p + 1)) + a*n*p/(m + n*p + 1)*Int[(c*x)^m*(a + b*x^n)^(p - 1), x] /; FreeQ[{a, b, c, m}, x] && IGtQ[n, 0] && GtQ[p, 0] && NeQ[m + n*p + 1, 0] && IntBinomialQ[a, b, c, n, m, p, x]
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p/(c*(m + 2*n*p + 1)) + 2*a1*a2*n*p/(m + 2*n*p + 1)* Int[(c*x)^m*(a1 + b1*x^n)^(p - 1)*(a2 + b2*x^n)^(p - 1), x] /; FreeQ[{a1, b1, a2, b2, c, m}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && GtQ[p, 0] && NeQ[m + 2*n*p + 1, 0] && IntBinomialQ[a1*a2, b1*b2, c, 2*n, m, p, x]
Int[x_^2/(a_ + b_.*x_^4)^(5/4), x_Symbol] := x*(1 + a/(b*x^4))^(1/4)/(b*(a + b*x^4)^(1/4))* Int[1/(x^3*(1 + a/(b*x^4))^(5/4)), x] /; FreeQ[{a, b}, x] && PosQ[b/a]
Int[x_^m_/(a_ + b_.*x_^4)^(5/4), x_Symbol] := x^(m - 3)/(b*(m - 4)*(a + b*x^4)^(1/4)) - a*(m - 3)/(b*(m - 4))*Int[x^(m - 4)/(a + b*x^4)^(5/4), x] /; FreeQ[{a, b}, x] && PosQ[b/a] && IGtQ[(m - 2)/4, 0]
Int[x_^m_/(a_ + b_.*x_^4)^(5/4), x_Symbol] := x^(m + 1)/(a*(m + 1)*(a + b*x^4)^(1/4)) - b*m/(a*(m + 1))*Int[x^(m + 4)/(a + b*x^4)^(5/4), x] /; FreeQ[{a, b}, x] && PosQ[b/a] && ILtQ[(m - 2)/4, 0]
Int[Sqrt[c_.*x_]/(a_ + b_.*x_^2)^(5/4), x_Symbol] := Sqrt[c*x]*(1 + a/(b*x^2))^(1/4)/(b*(a + b*x^2)^(1/4))* Int[1/(x^2*(1 + a/(b*x^2))^(5/4)), x] /; FreeQ[{a, b, c}, x] && PosQ[b/a]
Int[(c_.*x_)^m_/(a_ + b_.*x_^2)^(5/4), x_Symbol] := 2*c*(c*x)^(m - 1)/(b*(2*m - 3)*(a + b*x^2)^(1/4)) - 2*a*c^2*(m - 1)/(b*(2*m - 3))* Int[(c*x)^(m - 2)/(a + b*x^2)^(5/4), x] /; FreeQ[{a, b, c}, x] && PosQ[b/a] && IntegerQ[2*m] && GtQ[m, 3/2] 
Int[(c_.*x_)^m_/(a_ + b_.*x_^2)^(5/4), x_Symbol] := (c*x)^(m + 1)/(a*c*(m + 1)*(a + b*x^2)^(1/4)) - b*(2*m + 1)/(2*a*c^2*(m + 1))* Int[(c*x)^(m + 2)/(a + b*x^2)^(5/4), x] /; FreeQ[{a, b, c}, x] && PosQ[b/a] && IntegerQ[2*m] && LtQ[m, -1]
Int[x_^2/(a_ + b_.*x_^4)^(5/4), x_Symbol] := -1/(b*x*(a + b*x^4)^(1/4)) - 1/b*Int[1/(x^2*(a + b*x^4)^(1/4)), x] /; FreeQ[{a, b}, x] && NegQ[b/a]
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := c^(n - 1)*(c*x)^(m - n + 1)*(a + b*x^n)^(p + 1)/(b*n*(p + 1)) - c^n*(m - n + 1)/(b*n*(p + 1))* Int[(c*x)^(m - n)*(a + b*x^n)^(p + 1), x] /; FreeQ[{a, b, c}, x] && IGtQ[n, 0] && LtQ[p, -1] && GtQ[m + 1, n] && Not[ILtQ[(m + n*(p + 1) + 1)/n, 0]] && IntBinomialQ[a, b, c, n, m, p, x]
(* Int[(c_.*x_)^m_.*u_^p_*v_^p_,x_Symbol] := With[{a=BinomialParts[u,x][[1]],b=BinomialParts[u,x][[2]],n= BinomialParts[u,x][[3]]}, c^(n-1)*(c*x)^(m-n+1)*u^(p+1)*v^(p+1)/(b*n*(p+1)) - c^n*(m-n+1)/(b*n*(p+1))*Int[(c*x)^(m-n)*u^(p+1)*v^(p+1),x] /; IGtQ[n,0] && m+1>n && Not[ILtQ[(m+n*(p+1)+1)/n,0]] && IntBinomialQ[a,b,c,n,m,p,x]] /; FreeQ[c,x] && BinomialQ[u*v,x] && LtQ[p,-1] *)
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := c^(2*n - 1)*(c*x)^(m - 2*n + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(2*b1*b2*n*(p + 1)) - c^(2*n)*(m - 2*n + 1)/(2*b1*b2*n*(p + 1))* Int[(c*x)^(m - 2*n)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1), x] /; FreeQ[{a1, b1, a2, b2, c}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && LtQ[p, -1] && m + 1 > 2*n && Not[ILtQ[(m + 2*n*(p + 1) + 1)/(2*n), 0]] && IntBinomialQ[a1*a2, b1*b2, c, 2*n, m, p, x]
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := -(c*x)^(m + 1)*(a + b*x^n)^(p + 1)/(a*c*n*(p + 1)) + (m + n*(p + 1) + 1)/(a*n*(p + 1))* Int[(c*x)^m*(a + b*x^n)^(p + 1), x] /; FreeQ[{a, b, c, m}, x] && IGtQ[n, 0] && LtQ[p, -1] && IntBinomialQ[a, b, c, n, m, p, x]
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := -(c*x)^(m + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(2*a1* a2*c*n*(p + 1)) + (m + 2*n*(p + 1) + 1)/(2*a1*a2*n*(p + 1))* Int[(c*x)^m*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1), x] /; FreeQ[{a1, b1, a2, b2, c, m}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && LtQ[p, -1] && IntBinomialQ[a1*a2, b1*b2, c, 2*n, m, p, x]
Int[x_/(a_ + b_.*x_^3), x_Symbol] := -1/(3*Rt[a, 3]*Rt[b, 3])*Int[1/(Rt[a, 3] + Rt[b, 3]*x), x] + 1/(3*Rt[a, 3]*Rt[b, 3])* Int[(Rt[a, 3] + Rt[b, 3]*x)/(Rt[a, 3]^2 - Rt[a, 3]*Rt[b, 3]*x + Rt[b, 3]^2*x^2), x] /; FreeQ[{a, b}, x]
(* Int[x_^m_./(a_+b_.*x_^5),x_Symbol] := With[{r=Numerator[Rt[a/b,5]], s=Denominator[Rt[a/b,5]]}, (-1)^m*r^(m+1)/(5*a*s^m)*Int[1/(r+s*x),x] + 2*r^(m+1)/(5*a*s^m)*Int[(r*Cos[m*Pi/5]-s*Cos[(m+1)*Pi/5]*x)/(r^2-1/ 2*(1+Sqrt[5])*r*s*x+s^2*x^2),x] + 2*r^(m+1)/(5*a*s^m)*Int[(r*Cos[3*m*Pi/5]-s*Cos[3*(m+1)*Pi/5]*x)/(r^ 2-1/2*(1-Sqrt[5])*r*s*x+s^2*x^2),x]] /; FreeQ[{a,b},x] && IGtQ[m,0] && LtQ[m,4] && PosQ[a/b] *)
(* Int[x_^m_./(a_+b_.*x_^5),x_Symbol] := With[{r=Numerator[Rt[-a/b,5]], s=Denominator[Rt[-a/b,5]]}, (r^(m+1)/(5*a*s^m))*Int[1/(r-s*x),x] + 2*(-1)^m*r^(m+1)/(5*a*s^m)*Int[(r*Cos[m*Pi/5]+s*Cos[(m+1)*Pi/5]*x)/( r^2+1/2*(1+Sqrt[5])*r*s*x+s^2*x^2),x] + 2*(-1)^m*r^(m+1)/(5*a*s^m)*Int[(r*Cos[3*m*Pi/5]+s*Cos[3*(m+1)*Pi/5]* x)/(r^2+1/2*(1-Sqrt[5])*r*s*x+s^2*x^2),x]] /; FreeQ[{a,b},x] && IGtQ[m,0] && LtQ[m,4] && NegQ[a/b] *)
Int[x_^m_./(a_ + b_.*x_^n_), x_Symbol] := Module[{r = Numerator[Rt[a/b, n]], s = Denominator[Rt[a/b, n]], k, u}, u = Int[(r*Cos[(2*k - 1)*m*Pi/n] - s*Cos[(2*k - 1)*(m + 1)*Pi/n]*x)/(r^2 - 2*r*s*Cos[(2*k - 1)*Pi/n]*x + s^2*x^2), x]; -(-r)^(m + 1)/(a*n*s^m)*Int[1/(r + s*x), x] + Dist[2*r^(m + 1)/(a*n*s^m), Sum[u, {k, 1, (n - 1)/2}], x]] /; FreeQ[{a, b}, x] && IGtQ[(n - 1)/2, 0] && IGtQ[m, 0] && LtQ[m, n - 1] && PosQ[a/b]
Int[x_^m_./(a_ + b_.*x_^n_), x_Symbol] := Module[{r = Numerator[Rt[-a/b, n]], s = Denominator[Rt[-a/b, n]], k, u}, u = Int[(r*Cos[(2*k - 1)*m*Pi/n] + s*Cos[(2*k - 1)*(m + 1)*Pi/n]*x)/(r^2 + 2*r*s*Cos[(2*k - 1)*Pi/n]*x + s^2*x^2), x]; r^(m + 1)/(a*n*s^m)*Int[1/(r - s*x), x] - Dist[2*(-r)^(m + 1)/(a*n*s^m), Sum[u, {k, 1, (n - 1)/2}], x]] /; FreeQ[{a, b}, x] && IGtQ[(n - 1)/2, 0] && IGtQ[m, 0] && LtQ[m, n - 1] && NegQ[a/b]
Int[x_^m_./(a_ + b_.*x_^n_), x_Symbol] := Module[{r = Numerator[Rt[a/b, n]], s = Denominator[Rt[a/b, n]], k, u}, u = Int[(r*Cos[(2*k - 1)*m*Pi/n] - s*Cos[(2*k - 1)*(m + 1)*Pi/n]*x)/(r^2 - 2*r*s*Cos[(2*k - 1)*Pi/n]*x + s^2*x^2), x] + Int[(r*Cos[(2*k - 1)*m*Pi/n] + s*Cos[(2*k - 1)*(m + 1)*Pi/n]*x)/(r^2 + 2*r*s*Cos[(2*k - 1)*Pi/n]*x + s^2*x^2), x]; 2*(-1)^(m/2)*r^(m + 2)/(a*n*s^m)*Int[1/(r^2 + s^2*x^2), x] + Dist[2*r^(m + 1)/(a*n*s^m), Sum[u, {k, 1, (n - 2)/4}], x]] /; FreeQ[{a, b}, x] && IGtQ[(n - 2)/4, 0] && IGtQ[m, 0] && LtQ[m, n - 1] && PosQ[a/b]
Int[x_^m_./(a_ + b_.*x_^n_), x_Symbol] := Module[{r = Numerator[Rt[-a/b, n]], s = Denominator[Rt[-a/b, n]], k, u}, u = Int[(r*Cos[2*k*m*Pi/n] - s*Cos[2*k*(m + 1)*Pi/n]*x)/(r^2 - 2*r*s*Cos[2*k*Pi/n]*x + s^2*x^2), x] + Int[(r*Cos[2*k*m*Pi/n] + s*Cos[2*k*(m + 1)*Pi/n]*x)/(r^2 + 2*r*s*Cos[2*k*Pi/n]*x + s^2*x^2), x]; 2*r^(m + 2)/(a*n*s^m)*Int[1/(r^2 - s^2*x^2), x] + Dist[2*r^(m + 1)/(a*n*s^m), Sum[u, {k, 1, (n - 2)/4}], x]] /; FreeQ[{a, b}, x] && IGtQ[(n - 2)/4, 0] && IGtQ[m, 0] && LtQ[m, n - 1] && NegQ[a/b]
Int[x_^2/(a_ + b_.*x_^4), x_Symbol] := With[{r = Numerator[Rt[a/b, 2]], s = Denominator[Rt[a/b, 2]]}, 1/(2*s)*Int[(r + s*x^2)/(a + b*x^4), x] - 1/(2*s)*Int[(r - s*x^2)/(a + b*x^4), x]] /; FreeQ[{a, b}, x] && (GtQ[a/b, 0] || PosQ[a/b] && AtomQ[SplitProduct[SumBaseQ, a]] && AtomQ[SplitProduct[SumBaseQ, b]])
Int[x_^2/(a_ + b_.*x_^4), x_Symbol] := With[{r = Numerator[Rt[-a/b, 2]], s = Denominator[Rt[-a/b, 2]]}, s/(2*b)*Int[1/(r + s*x^2), x] - s/(2*b)*Int[1/(r - s*x^2), x]] /; FreeQ[{a, b}, x] && Not[GtQ[a/b, 0]]
Int[x_^m_./(a_ + b_.*x_^n_), x_Symbol] := With[{r = Numerator[Rt[a/b, 4]], s = Denominator[Rt[a/b, 4]]}, s^3/(2*Sqrt[2]*b*r)* Int[x^(m - n/4)/(r^2 - Sqrt[2]*r*s*x^(n/4) + s^2*x^(n/2)), x] - s^3/(2*Sqrt[2]*b*r)* Int[x^(m - n/4)/(r^2 + Sqrt[2]*r*s*x^(n/4) + s^2*x^(n/2)), x]] /; FreeQ[{a, b}, x] && IGtQ[n/4, 0] && IGtQ[m, 0] && LtQ[m, n - 1] && GtQ[a/b, 0]
Int[x_^m_/(a_ + b_.*x_^n_), x_Symbol] := With[{r = Numerator[Rt[-a/b, 2]], s = Denominator[Rt[-a/b, 2]]}, r/(2*a)*Int[x^m/(r + s*x^(n/2)), x] + r/(2*a)*Int[x^m/(r - s*x^(n/2)), x]] /; FreeQ[{a, b}, x] && IGtQ[n/4, 0] && IGtQ[m, 0] && LtQ[m, n/2] && Not[GtQ[a/b, 0]]
Int[x_^m_/(a_ + b_.*x_^n_), x_Symbol] := With[{r = Numerator[Rt[-a/b, 2]], s = Denominator[Rt[-a/b, 2]]}, s/(2*b)*Int[x^(m - n/2)/(r + s*x^(n/2)), x] - s/(2*b)*Int[x^(m - n/2)/(r - s*x^(n/2)), x]] /; FreeQ[{a, b}, x] && IGtQ[n/4, 0] && IGtQ[m, 0] && LeQ[n/2, m] && LtQ[m, n] && Not[GtQ[a/b, 0]]
Int[x_^m_/(a_ + b_.*x_^n_), x_Symbol] := Int[PolynomialDivide[x^m, (a + b*x^n), x], x] /; FreeQ[{a, b}, x] && IGtQ[m, 0] && IGtQ[n, 0] && GtQ[m, 2*n - 1]
Int[x_/Sqrt[a_ + b_.*x_^3], x_Symbol] := With[{r = Numer[Rt[b/a, 3]], s = Denom[Rt[b/a, 3]]}, -(1 - Sqrt[3])*s/r*Int[1/Sqrt[a + b*x^3], x] + 1/r*Int[((1 - Sqrt[3])*s + r*x)/Sqrt[a + b*x^3], x]] /; FreeQ[{a, b}, x] && PosQ[a]
Int[x_/Sqrt[a_ + b_.*x_^3], x_Symbol] := With[{r = Numer[Rt[b/a, 3]], s = Denom[Rt[b/a, 3]]}, -(1 + Sqrt[3])*s/r*Int[1/Sqrt[a + b*x^3], x] + 1/r*Int[((1 + Sqrt[3])*s + r*x)/Sqrt[a + b*x^3], x]] /; FreeQ[{a, b}, x] && NegQ[a]
Int[x_^2/Sqrt[a_ + b_.*x_^4], x_Symbol] := With[{q = Rt[b/a, 2]}, 1/q*Int[1/Sqrt[a + b*x^4], x] - 1/q*Int[(1 - q*x^2)/Sqrt[a + b*x^4], x]] /; FreeQ[{a, b}, x] && PosQ[b/a]
Int[x_^2/Sqrt[a_ + b_.*x_^4], x_Symbol] := With[{q = Rt[-b/a, 2]}, 1/q*Int[1/Sqrt[a + b*x^4], x] - 1/q*Int[(1 - q*x^2)/Sqrt[a + b*x^4], x]] /; FreeQ[{a, b}, x] && LtQ[a, 0] && GtQ[b, 0]
Int[x_^2/Sqrt[a_ + b_.*x_^4], x_Symbol] := With[{q = Rt[-b/a, 2]}, -1/q*Int[1/Sqrt[a + b*x^4], x] + 1/q*Int[(1 + q*x^2)/Sqrt[a + b*x^4], x]] /; FreeQ[{a, b}, x] && NegQ[b/a]
Int[x_^4/Sqrt[a_ + b_.*x_^6], x_Symbol] := With[{r = Numer[Rt[b/a, 3]], s = Denom[Rt[b/a, 3]]}, (Sqrt[3] - 1)*s^2/(2*r^2)*Int[1/Sqrt[a + b*x^6], x] - 1/(2*r^2)* Int[((Sqrt[3] - 1)*s^2 - 2*r^2*x^4)/Sqrt[a + b*x^6], x]] /; FreeQ[{a, b}, x]
(* Int[x_^4/Sqrt[a_+b_.*x_^6],x_Symbol] := With[{r=Numer[Rt[b/a,3]], s=Denom[Rt[b/a,3]]}, (1+Sqrt[3])*r*x*Sqrt[a+b*x^6]/(2*b*(s+(1+Sqrt[3])*r*x^2)) - 3^(1/4)*s*x*(s+r*x^2)*Sqrt[(s^2-r*s*x^2+r^2*x^4)/(s+(1+Sqrt[3])*r*x^ 2)^2]/ (2*r^2*Sqrt[a+b*x^6]*Sqrt[r*x^2*(s+r*x^2)/(s+(1+Sqrt[3])*r*x^2)^2] )* EllipticE[ArcCos[(s+(1-Sqrt[3])*r*x^2)/(s+(1+Sqrt[3])*r*x^2)],(2+ Sqrt[3])/4] - (1-Sqrt[3])*s*x*(s+r*x^2)*Sqrt[(s^2-r*s*x^2+r^2*x^4)/(s+(1+Sqrt[3])* r*x^2)^2]/ (4*3^(1/4)*r^2*Sqrt[a+b*x^6]*Sqrt[r*x^2*(s+r*x^2)/(s+(1+Sqrt[3])* r*x^2)^2])* EllipticF[ArcCos[(s+(1-Sqrt[3])*r*x^2)/(s+(1+Sqrt[3])*r*x^2)],(2+ Sqrt[3])/4]] /; FreeQ[{a,b},x] *)
Int[x_^2/Sqrt[a_ + b_.*x_^8], x_Symbol] := 1/(2*Rt[b/a, 4])*Int[(1 + Rt[b/a, 4]*x^2)/Sqrt[a + b*x^8], x] - 1/(2*Rt[b/a, 4])*Int[(1 - Rt[b/a, 4]*x^2)/Sqrt[a + b*x^8], x] /; FreeQ[{a, b}, x]
Int[x_^2/(a_ + b_.*x_^4)^(1/4), x_Symbol] := x^3/(2*(a + b*x^4)^(1/4)) - a/2*Int[x^2/(a + b*x^4)^(5/4), x] /; FreeQ[{a, b}, x] && PosQ[b/a]
Int[x_^2/(a_ + b_.*x_^4)^(1/4), x_Symbol] := (a + b*x^4)^(3/4)/(2*b*x) + a/(2*b)*Int[1/(x^2*(a + b*x^4)^(1/4)), x] /; FreeQ[{a, b}, x] && NegQ[b/a]
Int[1/(x_^2*(a_ + b_.*x_^4)^(1/4)), x_Symbol] := -1/(x*(a + b*x^4)^(1/4)) - b*Int[x^2/(a + b*x^4)^(5/4), x] /; FreeQ[{a, b}, x] && PosQ[b/a]
Int[1/(x_^2*(a_ + b_.*x_^4)^(1/4)), x_Symbol] := x*(1 + a/(b*x^4))^(1/4)/(a + b*x^4)^(1/4)* Int[1/(x^3*(1 + a/(b*x^4))^(1/4)), x] /; FreeQ[{a, b}, x] && NegQ[b/a]
Int[Sqrt[c_*x_]/(a_ + b_.*x_^2)^(1/4), x_Symbol] := x*Sqrt[c*x]/(a + b*x^2)^(1/4) - a/2*Int[Sqrt[c*x]/(a + b*x^2)^(5/4), x] /; FreeQ[{a, b, c}, x] && PosQ[b/a]
Int[Sqrt[c_*x_]/(a_ + b_.*x_^2)^(1/4), x_Symbol] := c*(a + b*x^2)^(3/4)/(b*Sqrt[c*x]) + a*c^2/(2*b)*Int[1/((c*x)^(3/2)*(a + b*x^2)^(1/4)), x] /; FreeQ[{a, b, c}, x] && NegQ[b/a]
Int[1/((c_.*x_)^(3/2)*(a_ + b_.*x_^2)^(1/4)), x_Symbol] := -2/(c*Sqrt[c*x]*(a + b*x^2)^(1/4)) - b/c^2*Int[Sqrt[c*x]/(a + b*x^2)^(5/4), x] /; FreeQ[{a, b, c}, x] && PosQ[b/a]
Int[1/((c_.*x_)^(3/2)*(a_ + b_.*x_^2)^(1/4)), x_Symbol] := Sqrt[c*x]*(1 + a/(b*x^2))^(1/4)/(c^2*(a + b*x^2)^(1/4))* Int[1/(x^2*(1 + a/(b*x^2))^(1/4)), x] /; FreeQ[{a, b, c}, x] && NegQ[b/a]
Int[Sqrt[x_]/Sqrt[a_ + b_.*x_^2], x_Symbol] := -2/(Sqrt[a]*(-b/a)^(3/4))* Subst[Int[Sqrt[1 - 2*x^2]/Sqrt[1 - x^2], x], x, Sqrt[1 - Sqrt[-b/a]*x]/Sqrt[2]] /; FreeQ[{a, b}, x] && GtQ[-b/a, 0] && GtQ[a, 0]
Int[Sqrt[x_]/Sqrt[a_ + b_.*x_^2], x_Symbol] := Sqrt[1 + b*x^2/a]/Sqrt[a + b*x^2]* Int[Sqrt[x]/Sqrt[1 + b*x^2/a], x] /; FreeQ[{a, b}, x] && GtQ[-b/a, 0] && Not[GtQ[a, 0]]
Int[Sqrt[c_*x_]/Sqrt[a_ + b_.*x_^2], x_Symbol] := Sqrt[c*x]/Sqrt[x]*Int[Sqrt[x]/Sqrt[a + b*x^2], x] /; FreeQ[{a, b, c}, x] && GtQ[-b/a, 0]
Int[(c_.*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := c^(n - 1)*(c*x)^(m - n + 1)*(a + b*x^n)^(p + 1)/(b*(m + n*p + 1)) - a*c^n*(m - n + 1)/(b*(m + n*p + 1))* Int[(c*x)^(m - n)*(a + b*x^n)^p, x] /; FreeQ[{a, b, c, p}, x] && IGtQ[n, 0] && GtQ[m, n - 1] && NeQ[m + n*p + 1, 0] && IntBinomialQ[a, b, c, n, m, p, x]
Int[(c_.*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := c^(n - 1)*(c*x)^(m - n + 1)*(a + b*x^n)^(p + 1)/(b*(m + n*p + 1)) - a*c^n*(m - n + 1)/(b*(m + n*p + 1))* Int[(c*x)^(m - n)*(a + b*x^n)^p, x] /; FreeQ[{a, b, c, m, p}, x] && IGtQ[n, 0] && SumSimplerQ[m, -n] && NeQ[m + n*p + 1, 0] && ILtQ[Simplify[(m + 1)/n + p], 0]
Int[(c_.*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := c^(2*n - 1)*(c*x)^(m - 2*n + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(b1*b2*(m + 2*n*p + 1)) - a1*a2*c^(2*n)*(m - 2*n + 1)/(b1*b2*(m + 2*n*p + 1))* Int[(c*x)^(m - 2*n)*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, c, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && GtQ[m, 2*n - 1] && NeQ[m + 2*n*p + 1, 0] && IntBinomialQ[a1*a2, b1*b2, c, 2*n, m, p, x]
Int[(c_.*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := c^(2*n - 1)*(c*x)^(m - 2*n + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(b1*b2*(m + 2*n*p + 1)) - a1*a2*c^(2*n)*(m - 2*n + 1)/(b1*b2*(m + 2*n*p + 1))* Int[(c*x)^(m - 2*n)*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, c, m, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && SumSimplerQ[m, -2*n] && NeQ[m + 2*n*p + 1, 0] && ILtQ[Simplify[(m + 1)/(2*n) + p], 0]
Int[(c_.*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a + b*x^n)^(p + 1)/(a*c*(m + 1)) - b*(m + n*(p + 1) + 1)/(a*c^n*(m + 1))* Int[(c*x)^(m + n)*(a + b*x^n)^p, x] /; FreeQ[{a, b, c, p}, x] && IGtQ[n, 0] && LtQ[m, -1] && IntBinomialQ[a, b, c, n, m, p, x]
Int[(c_.*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a + b*x^n)^(p + 1)/(a*c*(m + 1)) - b*(m + n*(p + 1) + 1)/(a*c^n*(m + 1))* Int[(c*x)^(m + n)*(a + b*x^n)^p, x] /; FreeQ[{a, b, c, m, p}, x] && IGtQ[n, 0] && SumSimplerQ[m, n] && ILtQ[Simplify[(m + 1)/n + p], 0]
Int[(c_.*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(a1*a2* c*(m + 1)) - b1*b2*(m + 2*n*(p + 1) + 1)/(a1*a2*c^(2*n)*(m + 1))* Int[(c*x)^(m + 2*n)*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, c, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && LtQ[m, -1] && IntBinomialQ[a1*a2, b1*b2, c, 2*n, m, p, x]
Int[(c_.*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(a1*a2* c*(m + 1)) - b1*b2*(m + 2*n*(p + 1) + 1)/(a1*a2*c^(2*n)*(m + 1))* Int[(c*x)^(m + 2*n)*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, c, m, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && SumSimplerQ[m, 2*n] && ILtQ[Simplify[(m + 1)/(2*n) + p], 0]
Int[(c_.*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := With[{k = Denominator[m]}, k/c* Subst[Int[x^(k*(m + 1) - 1)*(a + b*x^(k*n)/c^n)^p, x], x, (c*x)^(1/k)]] /; FreeQ[{a, b, c, p}, x] && IGtQ[n, 0] && FractionQ[m] && IntBinomialQ[a, b, c, n, m, p, x]
Int[(c_.*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := With[{k = Denominator[m]}, k/c* Subst[Int[ x^(k*(m + 1) - 1)*(a1 + b1*x^(k*n)/c^n)^p*(a2 + b2*x^(k*n)/c^n)^ p, x], x, (c*x)^(1/k)]] /; FreeQ[{a1, b1, a2, b2, c, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && FractionQ[m] && IntBinomialQ[a1*a2, b1*b2, c, 2*n, m, p, x]
Int[x_/(a_ + b_.*x_^3)^(2/3), x_Symbol] := With[{q = Rt[b, 3]}, -ArcTan[(1 + 2*q*x/(a + b*x^3)^(1/3))/Sqrt[3]]/(Sqrt[3]*q^2) - Log[q*x - (a + b*x^3)^(1/3)]/(2*q^2)] /; FreeQ[{a, b}, x]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := a^(p + (m + 1)/n)* Subst[Int[x^m/(1 - b*x^n)^(p + (m + 1)/n + 1), x], x, x/(a + b*x^n)^(1/n)] /; FreeQ[{a, b}, x] && IGtQ[n, 0] && LtQ[-1, p, 0] && NeQ[p, -1/2] && IntegersQ[m, p + (m + 1)/n]
Int[x_^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := (a1*a2)^(p + (m + 1)/(2*n))* Subst[ Int[x^m/((1 - b1*x^n)^(p + (m + 1)/(2*n) + 1)*(1 - b2*x^n)^(p + (m + 1)/(2*n) + 1)), x], x, x/((a1 + b1*x^n)^(1/(2*n))*(a2 + b2*x^n)^(1/(2*n)))] /; FreeQ[{a1, b1, a2, b2}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && LtQ[-1, p, 0] && NeQ[p, -1/2] && IntegersQ[m, p + (m + 1)/(2*n)]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := (a/(a + b*x^n))^(p + (m + 1)/n)*(a + b*x^n)^(p + (m + 1)/n)* Subst[Int[x^m/(1 - b*x^n)^(p + (m + 1)/n + 1), x], x, x/(a + b*x^n)^(1/n)] /; FreeQ[{a, b}, x] && IGtQ[n, 0] && LtQ[-1, p, 0] && NeQ[p, -1/2] && IntegerQ[m] && LtQ[Denominator[p + (m + 1)/n], Denominator[p]]
Int[x_^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := (a1/(a1 + b1*x^n))^(p + (m + 1)/(2*n))*(a1 + b1*x^n)^(p + (m + 1)/(2*n))*(a2/(a2 + b2*x^n))^(p + (m + 1)/(2*n))*(a2 + b2*x^n)^(p + (m + 1)/(2*n))* Subst[ Int[x^m/((1 - b1*x^n)^(p + (m + 1)/(2*n) + 1)*(1 - b2*x^n)^(p + (m + 1)/(2*n) + 1)), x], x, x/((a1 + b1*x^n)^(1/(2*n))*(a2 + b2*x^n)^(1/(2*n)))] /; FreeQ[{a1, b1, a2, b2}, x] && EqQ[a2*b1 + a1*b2, 0] && IGtQ[2*n, 0] && LtQ[-1, p, 0] && NeQ[p, -1/2] && IntegerQ[m] && LtQ[Denominator[p + (m + 1)/(2*n)], Denominator[p]]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := -Subst[Int[(a + b*x^(-n))^p/x^(m + 2), x], x, 1/x] /; FreeQ[{a, b, p}, x] && ILtQ[n, 0] && IntegerQ[m]
Int[x_^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := -Subst[Int[(a1 + b1*x^(-n))^p*(a2 + b2*x^(-n))^p/x^(m + 2), x], x, 1/x] /; FreeQ[{a1, b1, a2, b2, p}, x] && EqQ[a2*b1 + a1*b2, 0] && ILtQ[2*n, 0] && IntegerQ[m]
Int[(c_.*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := With[{k = Denominator[m]}, -k/c* Subst[Int[(a + b*c^(-n)*x^(-k*n))^p/x^(k*(m + 1) + 1), x], x, 1/(c*x)^(1/k)]] /; FreeQ[{a, b, c, p}, x] && ILtQ[n, 0] && FractionQ[m]
Int[(c_.*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := With[{k = Denominator[m]}, -k/c* Subst[Int[(a1 + b1*c^(-n)*x^(-k*n))^p*(a2 + b2*c^(-n)*x^(-k*n))^p/ x^(k*(m + 1) + 1), x], x, 1/(c*x)^(1/k)]] /; FreeQ[{a1, b1, a2, b2, c, p}, x] && EqQ[a2*b1 + a1*b2, 0] && ILtQ[2*n, 0] && FractionQ[m]
Int[(c_.*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := -1/c*(c*x)^(m + 1)*(1/x)^(m + 1)* Subst[Int[(a + b*x^(-n))^p/x^(m + 2), x], x, 1/x] /; FreeQ[{a, b, c, m, p}, x] && ILtQ[n, 0] && Not[RationalQ[m]]
Int[(c_.*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := -1/c*(c*x)^(m + 1)*(1/x)^(m + 1)* Subst[Int[(a1 + b1*x^(-n))^p*(a2 + b2*x^(-n))^p/x^(m + 2), x], x, 1/x] /; FreeQ[{a1, b1, a2, b2, c, m, p}, x] && EqQ[a2*b1 + a1*b2, 0] && ILtQ[2*n, 0] && Not[RationalQ[m]]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := With[{k = Denominator[n]}, k*Subst[Int[x^(k*(m + 1) - 1)*(a + b*x^(k*n))^p, x], x, x^(1/k)]] /; FreeQ[{a, b, m, p}, x] && FractionQ[n]
Int[x_^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := With[{k = Denominator[2*n]}, k*Subst[ Int[x^(k*(m + 1) - 1)*(a1 + b1*x^(k*n))^p*(a2 + b2*x^(k*n))^p, x], x, x^(1/k)]] /; FreeQ[{a1, b1, a2, b2, m, p}, x] && EqQ[a2*b1 + a1*b2, 0] && FractionQ[2*n]
Int[(c_*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a + b*x^n)^p, x] /; FreeQ[{a, b, c, m, p}, x] && FractionQ[n]
Int[(c_*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, c, m, p}, x] && EqQ[a2*b1 + a1*b2, 0] && FractionQ[2*n]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := 1/(m + 1)* Subst[Int[(a + b*x^Simplify[n/(m + 1)])^p, x], x, x^(m + 1)] /; FreeQ[{a, b, m, n, p}, x] && IntegerQ[Simplify[n/(m + 1)]] && Not[IntegerQ[n]]
Int[x_^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := 1/(m + 1)* Subst[Int[(a1 + b1*x^Simplify[n/(m + 1)])^ p*(a2 + b2*x^Simplify[n/(m + 1)])^p, x], x, x^(m + 1)] /; FreeQ[{a1, b1, a2, b2, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IntegerQ[Simplify[2*n/(m + 1)]] && Not[IntegerQ[2*n]]
Int[(c_*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a + b*x^n)^p, x] /; FreeQ[{a, b, c, m, n, p}, x] && IntegerQ[Simplify[n/(m + 1)]] && Not[IntegerQ[n]]
Int[(c_*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, c, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && IntegerQ[Simplify[2*n/(m + 1)]] && Not[IntegerQ[2*n]]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := x^(m + 1)*(a + b*x^n)^p/(m + 1) - b*n*p/(m + 1)*Int[x^(m + n)*(a + b*x^n)^(p - 1), x] /; FreeQ[{a, b, m, n}, x] && EqQ[(m + 1)/n + p, 0] && GtQ[p, 0]
Int[x_^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := x^(m + 1)*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p/(m + 1) - 2*b1*b2*n*p/(m + 1)* Int[x^(m + 2*n)*(a1 + b1*x^n)^(p - 1)*(a2 + b2*x^n)^(p - 1), x] /; FreeQ[{a1, b1, a2, b2, m, n}, x] && EqQ[a2*b1 + a1*b2, 0] && EqQ[(m + 1)/(2*n) + p, 0] && GtQ[p, 0]
Int[(c_*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a + b*x^n)^p, x] /; FreeQ[{a, b, c, m, n}, x] && EqQ[(m + 1)/n + p, 0] && GtQ[p, 0]
Int[(c_*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, c, m, n}, x] && EqQ[a2*b1 + a1*b2, 0] && EqQ[(m + 1)/(2*n) + p, 0] && GtQ[p, 0]
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a + b*x^n)^p/(c*(m + n*p + 1)) + a*n*p/(m + n*p + 1)*Int[(c*x)^m*(a + b*x^n)^(p - 1), x] /; FreeQ[{a, b, c, m, n}, x] && IntegerQ[p + Simplify[(m + 1)/n]] && GtQ[p, 0] && NeQ[m + n*p + 1, 0]
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := (c*x)^(m + 1)*(a1 + b1*x^n)^ p*(a2 + b2*x^n)^p/(c*(m + 2*n*p + 1)) + 2*a1*a2*n*p/(m + 2*n*p + 1)* Int[(c*x)^m*(a1 + b1*x^n)^(p - 1)*(a2 + b2*x^n)^(p - 1), x] /; FreeQ[{a1, b1, a2, b2, c, m, n}, x] && EqQ[a2*b1 + a1*b2, 0] && IntegerQ[p + Simplify[(m + 1)/(2*n)]] && GtQ[p, 0] && NeQ[m + 2*n*p + 1, 0]
Int[x_^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := With[{k = Denominator[p]}, k*a^(p + Simplify[(m + 1)/n])/n* Subst[ Int[x^(k*Simplify[(m + 1)/n] - 1)/(1 - b*x^k)^(p + Simplify[(m + 1)/n] + 1), x], x, x^(n/k)/(a + b*x^n)^(1/k)]] /; FreeQ[{a, b, m, n}, x] && IntegerQ[p + Simplify[(m + 1)/n]] && LtQ[-1, p, 0]
Int[x_^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := With[{k = Denominator[p]}, k*(a1*a2)^(p + Simplify[(m + 1)/(2*n)])/(2*n)* Subst[ Int[x^(k*Simplify[(m + 1)/(2*n)] - 1)/(1 - b1*b2*x^k)^(p + Simplify[(m + 1)/(2*n)] + 1), x], x, x^(2*n/k)/((a1 + b1*x^n)^(1/k)*(a2 + b2*x^n)^(1/k))]] /; FreeQ[{a1, b1, a2, b2, m, n}, x] && EqQ[a2*b1 + a1*b2, 0] && IntegerQ[p + Simplify[(m + 1)/(2*n)]] && LtQ[-1, p, 0]
Int[(c_*x_)^m_*(a_ + b_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a + b*x^n)^p, x] /; FreeQ[{a, b, c, m, n}, x] && IntegerQ[p + Simplify[(m + 1)/n]] && LtQ[-1, p, 0]
Int[(c_*x_)^m_*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m*(a1 + b1*x^n)^p*(a2 + b2*x^n)^p, x] /; FreeQ[{a1, b1, a2, b2, c, m, n}, x] && EqQ[a2*b1 + a1*b2, 0] && IntegerQ[p + Simplify[(m + 1)/(2*n)]] && LtQ[-1, p, 0]
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := -(c*x)^(m + 1)*(a + b*x^n)^(p + 1)/(a*c*n*(p + 1)) + (m + n*(p + 1) + 1)/(a*n*(p + 1))* Int[(c*x)^m*(a + b*x^n)^(p + 1), x] /; FreeQ[{a, b, c, m, n}, x] && IntegerQ[p + Simplify[(m + 1)/n]] && LtQ[p, -1]
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := -(c*x)^(m + 1)*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1)/(2*a1* a2*c*n*(p + 1)) + (m + 2*n*(p + 1) + 1)/(2*a1*a2*n*(p + 1))* Int[(c*x)^m*(a1 + b1*x^n)^(p + 1)*(a2 + b2*x^n)^(p + 1), x] /; FreeQ[{a1, b1, a2, b2, c, m, n}, x] && EqQ[a2*b1 + a1*b2, 0] && IntegerQ[p + Simplify[(m + 1)/(2*n)]] && LtQ[p, -1]
Int[x_^m_./(a_ + b_.*x_^n_), x_Symbol] := With[{mn = Simplify[m - n]}, x^(mn + 1)/(b*(mn + 1)) - a/b*Int[x^mn/(a + b*x^n), x]] /; FreeQ[{a, b, m, n}, x] && FractionQ[Simplify[(m + 1)/n]] && SumSimplerQ[m, -n]
Int[x_^m_/(a_ + b_.*x_^n_), x_Symbol] := x^(m + 1)/(a*(m + 1)) - b/a*Int[x^Simplify[m + n]/(a + b*x^n), x] /; FreeQ[{a, b, m, n}, x] && FractionQ[Simplify[(m + 1)/n]] && SumSimplerQ[m, n]
Int[(c_*x_)^m_/(a_ + b_.*x_^n_), x_Symbol] := c^IntPart[m]*(c*x)^FracPart[m]/x^FracPart[m]* Int[x^m/(a + b*x^n), x] /; FreeQ[{a, b, c, m, n}, x] && FractionQ[ Simplify[(m + 1)/n]] && (SumSimplerQ[m, n] || SumSimplerQ[m, -n])
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := a^p*(c*x)^(m + 1)/(c*(m + 1))* Hypergeometric2F1[-p, (m + 1)/n, (m + 1)/n + 1, -b*x^n/a] /; FreeQ[{a, b, c, m, n, p}, x] && Not[IGtQ[p, 0]] && (ILtQ[p, 0] || GtQ[a, 0])
(* Int[(c_.*x_)^m_.*(a_+b_.*x_^n_)^p_,x_Symbol] := (c*x)^(m+1)*(a+b*x^n)^(p+1)/(a*c*(m+1))*Hypergeometric2F1[1,(m+1)/n+ p+1,(m+1)/n+1,-b*x^n/a] /; FreeQ[{a,b,c,m,n,p},x] && Not[IGtQ[p,0]] && Not[ILtQ[p,0] ||  GtQ[a,0]] *)
Int[(c_.*x_)^m_.*(a_ + b_.*x_^n_)^p_, x_Symbol] := a^IntPart[p]*(a + b*x^n)^FracPart[p]/(1 + b*x^n/a)^FracPart[p]* Int[(c*x)^m*(1 + b*x^n/a)^p, x] /; FreeQ[{a, b, c, m, n, p}, x] && Not[IGtQ[p, 0]] && Not[ILtQ[p, 0] || GtQ[a, 0]]
Int[(c_.*x_)^m_.*(a1_ + b1_.*x_^n_)^p_*(a2_ + b2_.*x_^n_)^p_, x_Symbol] := (a1 + b1*x^n)^ FracPart[p]*(a2 + b2*x^n)^FracPart[p]/(a1*a2 + b1*b2*x^(2*n))^ FracPart[p]*Int[(c*x)^m*(a1*a2 + b1*b2*x^(2*n))^p, x] /; FreeQ[{a1, b1, a2, b2, c, m, n, p}, x] && EqQ[a2*b1 + a1*b2, 0] && Not[IntegerQ[p]]
(* IntBinomialQ[a,b,c,n,m,p,x] returns True iff (c*x)^m*(a+b*x^n)^p  is integrable wrt x in terms of non-hypergeometric functions. *) IntBinomialQ[a_, b_, c_, n_, m_, p_, x_] := IGtQ[p, 0] || RationalQ[m] && IntegersQ[n, 2*p] || IntegerQ[(m + 1)/n + p] || (EqQ[n, 2] || EqQ[n, 4]) && IntegersQ[2*m, 4*p] || EqQ[n, 2] && IntegerQ[6*p] && (IntegerQ[m] || IntegerQ[m - p])
Int[(d_.*x_)^m_.*(a_ + b_.*(c_*x_)^n_)^p_., x_Symbol] := 1/c*Subst[Int[(d*x/c)^m*(a + b*x^n)^p, x], x, c*x] /; FreeQ[{a, b, c, d, m, n, p}, x]
Int[(d_.*x_)^m_.*(a_ + b_.*(c_.*x_^q_)^n_)^p_., x_Symbol] := (d*x)^(m + 1)/(d*((c*x^q)^(1/q))^(m + 1))* Subst[Int[x^m*(a + b*x^(n*q))^p, x], x, (c*x^q)^(1/q)] /; FreeQ[{a, b, c, d, m, n, p, q}, x] && IntegerQ[n*q] && NeQ[x, (c*x^q)^(1/q)]
Int[(d_.*x_)^m_.*(a_ + b_.*(c_.*x_^q_)^n_)^p_., x_Symbol] := With[{k = Denominator[n]}, Subst[Int[(d*x)^m*(a + b*c^n*x^(n*q))^p, x], x^(1/k), (c*x^q)^(1/k)/(c^(1/k)*(x^(1/k))^(q - 1))]] /; FreeQ[{a, b, c, d, m, p, q}, x] && FractionQ[n]
Int[(d_.*x_)^m_.*(a_ + b_.*(c_.*x_^q_)^n_)^p_., x_Symbol] := Subst[Int[(d*x)^m*(a + b*c^n*x^(n*q))^p, x], x^(n*q), (c*x^q)^n/c^n] /; FreeQ[{a, b, c, d, m, n, p, q}, x] && Not[RationalQ[n]]
Int[x_^m_.*(a_ + b_.*v_^n_)^p_., x_Symbol] := With[{c = Coefficient[v, x, 0], d = Coefficient[v, x, 1]}, 1/d^(m + 1)* Subst[Int[SimplifyIntegrand[(x - c)^m*(a + b*x^n)^p, x], x], x, v] /; NeQ[c, 0]] /; FreeQ[{a, b, n, p}, x] && LinearQ[v, x] && IntegerQ[m]
Int[u_^m_.*(a_ + b_.*v_^n_)^p_., x_Symbol] := u^m/(Coefficient[v, x, 1]*v^m)* Subst[Int[x^m*(a + b*x^n)^p, x], x, v] /; FreeQ[{a, b, m, n, p}, x] && LinearPairQ[u, v, x]
