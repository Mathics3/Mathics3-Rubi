
(* ::Subsection::Closed:: *)
(* 1.2.2.2 (d x)^m (a+b x^2+c x^4)^p *)
(* Int[(d_.*x_)^m_.*(b_.*x_^2+c_.*x_^4)^p_.,x_Symbol] := 1/d^(2*p)*Int[(d*x)^(m+2*p)*(b+c*x^2)^p,x] /; FreeQ[{b,c,d,m},x] && IntegerQ[p] *)
(* Int[(d_.*x_)^m_.*(b_.*x_^2+c_.*x_^4)^p_,x_Symbol] := (b*x^2+c*x^4)^p/((d*x)^(2*p)*(b+c*x^2)^p)*Int[(d*x)^(m+2*p)*(b+c*x^ 2)^p,x] /; FreeQ[{b,c,d,m,p},x] && Not[IntegerQ[p]] *)
Int[x_*(a_ + b_.*x_^2 + c_.*x_^4)^p_., x_Symbol] := 1/2*Subst[Int[(a + b*x + c*x^2)^p, x], x, x^2] /; FreeQ[{a, b, c, p}, x]
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_., x_Symbol] := Int[ExpandIntegrand[(d*x)^m*(a + b*x^2 + c*x^4)^p, x], x] /; FreeQ[{a, b, c, d, m}, x] && IGtQ[p, 0] && Not[IntegerQ[(m + 1)/2]]
(* Int[(d_.*x_)^m_.*(a_+b_.*x_^2+c_.*x_^4)^p_.,x_Symbol] := 1/c^p*Int[(d*x)^m*(b/2+c*x^2)^(2*p),x] /; FreeQ[{a,b,c,d,m,p},x] && EqQ[b^2-4*a*c,0] && IntegerQ[p] *)
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := 2*(d*x)^(m + 1)*(a + b*x^2 + c*x^4)^(p + 1)/(d*(m + 3)*(2*a + b*x^2)) - (d*x)^(m + 1)*(a + b*x^2 + c*x^4)^(p + 1)/(2*a* d*(m + 3)*(p + 1)) /; FreeQ[{a, b, c, d, m, p}, x] && EqQ[b^2 - 4*a*c, 0] && Not[IntegerQ[p]] && EqQ[m + 4*p + 5, 0] && LtQ[p, -1]
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := (d*x)^(m + 1)*(a + b*x^2 + c*x^4)^(p + 1)/(4*a* d*(p + 1)*(2*p + 1)) - (d*x)^(m + 1)*(2*a + b*x^2)*(a + b*x^2 + c*x^4)^ p/(4*a*d*(2*p + 1)) /; FreeQ[{a, b, c, d, m, p}, x] && EqQ[b^2 - 4*a*c, 0] && Not[IntegerQ[p]] && EqQ[m + 4*p + 5, 0] && NeQ[p, -1/2]
Int[x_^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := 1/2*Subst[Int[x^((m - 1)/2)*(a + b*x + c*x^2)^p, x], x, x^2] /; FreeQ[{a, b, c, p}, x] && EqQ[b^2 - 4*a*c, 0] && IntegerQ[p - 1/2] && IntegerQ[(m - 1)/2] && (GtQ[m, 0] || LtQ[0, 4*p, -m - 1])
(* Int[(d_.*x_)^m_.*(a_+b_.*x_^2+c_.*x_^4)^p_,x_Symbol] := c*(a+b*x^2+c*x^4)^(p+1)/(b/2+c*x^2)^(2*(p+1))*Int[(d*x)^m*(b/2+c*x^ 2)^(2*p),x] /; FreeQ[{a,b,c,d,m,p},x] && EqQ[b^2-4*a*c,0] && IntegerQ[p-1/2] &&  IGeQ[m,2*p] *)
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := (a + b*x^2 + c*x^4)^ FracPart[p]/(c^IntPart[p]*(b/2 + c*x^2)^(2*FracPart[p]))* Int[(d*x)^m*(b/2 + c*x^2)^(2*p), x] /; FreeQ[{a, b, c, d, m, p}, x] && EqQ[b^2 - 4*a*c, 0] && IntegerQ[p - 1/2]
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := a^IntPart[p]*(a + b*x^2 + c*x^4)^ FracPart[p]/(1 + 2*c*x^2/b)^(2*FracPart[p])* Int[(d*x)^m*(1 + 2*c*x^2/b)^(2*p), x] /; FreeQ[{a, b, c, d, m, p}, x] && EqQ[b^2 - 4*a*c, 0] && Not[IntegerQ[2*p]]
Int[x_^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_., x_Symbol] := 1/2*Subst[Int[x^((m - 1)/2)*(a + b*x + c*x^2)^p, x], x, x^2] /; FreeQ[{a, b, c, p}, x] && IntegerQ[(m - 1)/2]
Int[(d_.*x_)^m_*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := With[{k = Denominator[m]}, k/d* Subst[Int[x^(k*(m + 1) - 1)*(a + b*x^(2*k)/d^2 + c*x^(4*k)/d^4)^p, x], x, (d*x)^(1/k)]] /; FreeQ[{a, b, c, d, p}, x] && NeQ[b^2 - 4*a*c, 0] && FractionQ[m] && IntegerQ[p]
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := d*(d*x)^(m - 1)*(a + b*x^2 + c*x^4)^ p*(2*b*p + c*(m + 4*p - 1)*x^2)/(c*(m + 4*p + 1)*(m + 4*p - 1)) - 2*p*d^2/(c*(m + 4*p + 1)*(m + 4*p - 1))* Int[(d*x)^(m - 2)*(a + b*x^2 + c*x^4)^(p - 1)* Simp[a*b*(m - 1) - (2*a*c*(m + 4*p - 1) - b^2*(m + 2*p - 1))* x^2, x], x] /; FreeQ[{a, b, c, d}, x] && NeQ[b^2 - 4*a*c, 0] && GtQ[p, 0] && GtQ[m, 1] && IntegerQ[2*p] && (IntegerQ[p] || IntegerQ[m])
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := (d*x)^(m + 1)*(a + b*x^2 + c*x^4)^p/(d*(m + 1)) - 2*p/(d^2*(m + 1))* Int[(d*x)^(m + 2)*(b + 2*c*x^2)*(a + b*x^2 + c*x^4)^(p - 1), x] /; FreeQ[{a, b, c, d}, x] && NeQ[b^2 - 4*a*c, 0] && GtQ[p, 0] && LtQ[m, -1] && IntegerQ[2*p] && (IntegerQ[p] || IntegerQ[m])
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := (d*x)^(m + 1)*(a + b*x^2 + c*x^4)^p/(d*(m + 4*p + 1)) + 2*p/(m + 4*p + 1)* Int[(d*x)^m*(2*a + b*x^2)*(a + b*x^2 + c*x^4)^(p - 1), x] /; FreeQ[{a, b, c, d, m}, x] && NeQ[b^2 - 4*a*c, 0] && GtQ[p, 0] && NeQ[m + 4*p + 1, 0] && IntegerQ[2*p] && (IntegerQ[p] || IntegerQ[m])
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := d*(d*x)^(m - 1)*(b + 2*c*x^2)*(a + b*x^2 + c*x^4)^(p + 1)/(2*(p + 1)*(b^2 - 4*a*c)) - d^2/(2*(p + 1)*(b^2 - 4*a*c))* Int[(d*x)^(m - 2)*(b*(m - 1) + 2*c*(m + 4*p + 5)*x^2)*(a + b*x^2 + c*x^4)^(p + 1), x] /; FreeQ[{a, b, c, d}, x] && NeQ[b^2 - 4*a*c, 0] && LtQ[p, -1] && GtQ[m, 1] && LeQ[m, 3] && IntegerQ[2*p] && (IntegerQ[p] || IntegerQ[m])
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := -d^3*(d*x)^(m - 3)*(2*a + b*x^2)*(a + b*x^2 + c*x^4)^(p + 1)/(2*(p + 1)*(b^2 - 4*a*c)) + d^4/(2*(p + 1)*(b^2 - 4*a*c))* Int[(d*x)^(m - 4)*(2*a*(m - 3) + b*(m + 4*p + 3)*x^2)*(a + b*x^2 + c*x^4)^(p + 1), x] /; FreeQ[{a, b, c, d}, x] && NeQ[b^2 - 4*a*c, 0] && LtQ[p, -1] && GtQ[m, 3] && IntegerQ[2*p] && (IntegerQ[p] || IntegerQ[m])
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := -(d*x)^(m + 1)*(b^2 - 2*a*c + b*c*x^2)*(a + b*x^2 + c*x^4)^(p + 1)/(2*a* d*(p + 1)*(b^2 - 4*a*c)) + 1/(2*a*(p + 1)*(b^2 - 4*a*c))* Int[(d*x)^m*(a + b*x^2 + c*x^4)^(p + 1)* Simp[b^2*(m + 2*p + 3) - 2*a*c*(m + 4*p + 5) + b*c*(m + 4*p + 7)*x^2, x], x] /; FreeQ[{a, b, c, d, m}, x] && NeQ[b^2 - 4*a*c, 0] && LtQ[p, -1] && IntegerQ[2*p] && (IntegerQ[p] || IntegerQ[m])
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := d^3*(d*x)^(m - 3)*(a + b*x^2 + c*x^4)^(p + 1)/(c*(m + 4*p + 1)) - d^4/(c*(m + 4*p + 1))* Int[(d*x)^(m - 4)* Simp[a*(m - 3) + b*(m + 2*p - 1)*x^2, x]*(a + b*x^2 + c*x^4)^p, x] /; FreeQ[{a, b, c, d, p}, x] && NeQ[b^2 - 4*a*c, 0] && GtQ[m, 3] && NeQ[m + 4*p + 1, 0] && IntegerQ[2*p] && (IntegerQ[p] || IntegerQ[m])
Int[(d_.*x_)^m_*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := (d*x)^(m + 1)*(a + b*x^2 + c*x^4)^(p + 1)/(a*d*(m + 1)) - 1/(a*d^2*(m + 1))* Int[(d*x)^(m + 2)*(b*(m + 2*p + 3) + c*(m + 4*p + 5)*x^2)*(a + b*x^2 + c*x^4)^p, x] /; FreeQ[{a, b, c, d, p}, x] && NeQ[b^2 - 4*a*c, 0] && LtQ[m, -1] && IntegerQ[2*p] && (IntegerQ[p] || IntegerQ[m])
Int[(d_.*x_)^m_/(a_ + b_.*x_^2 + c_.*x_^4), x_Symbol] := (d*x)^(m + 1)/(a*d*(m + 1)) - 1/(a*d^2)*Int[(d*x)^(m + 2)*(b + c*x^2)/(a + b*x^2 + c*x^4), x] /; FreeQ[{a, b, c, d}, x] && NeQ[b^2 - 4*a*c, 0] && LtQ[m, -1]
Int[x_^m_/(a_ + b_.*x_^2 + c_.*x_^4), x_Symbol] := Int[PolynomialDivide[x^m, (a + b*x^2 + c*x^4), x], x] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && IGtQ[m, 5]
Int[(d_.*x_)^m_/(a_ + b_.*x_^2 + c_.*x_^4), x_Symbol] := d^3*(d*x)^(m - 3)/(c*(m - 3)) - d^4/c*Int[(d*x)^(m - 4)*(a + b*x^2)/(a + b*x^2 + c*x^4), x] /; FreeQ[{a, b, c, d}, x] && NeQ[b^2 - 4*a*c, 0] && GtQ[m, 3]
Int[x_^2/(a_ + b_.*x_^2 + c_.*x_^4), x_Symbol] := With[{q = Rt[a/c, 2]}, 1/2*Int[(q + x^2)/(a + b*x^2 + c*x^4), x] - 1/2*Int[(q - x^2)/(a + b*x^2 + c*x^4), x]] /; FreeQ[{a, b, c}, x] && LtQ[b^2 - 4*a*c, 0] && PosQ[a*c]
Int[x_^m_./(a_ + b_.*x_^2 + c_.*x_^4), x_Symbol] := With[{q = Rt[a/c, 2]}, With[{r = Rt[2*q - b/c, 2]}, 1/(2*c*r)*Int[x^(m - 3)*(q + r*x)/(q + r*x + x^2), x] - 1/(2*c*r)*Int[x^(m - 3)*(q - r*x)/(q - r*x + x^2), x]]] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && GeQ[m, 3] && LtQ[m, 4] && NegQ[b^2 - 4*a*c]
Int[x_^m_./(a_ + b_.*x_^2 + c_.*x_^4), x_Symbol] := With[{q = Rt[a/c, 2]}, With[{r = Rt[2*q - b/c, 2]}, 1/(2*c*r)*Int[x^(m - 1)/(q - r*x + x^2), x] - 1/(2*c*r)*Int[x^(m - 1)/(q + r*x + x^2), x]]] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && GeQ[m, 1] && LtQ[m, 3] && NegQ[b^2 - 4*a*c]
Int[(d_.*x_)^m_/(a_ + b_.*x_^2 + c_.*x_^4), x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, d^2/2*(b/q + 1)*Int[(d*x)^(m - 2)/(b/2 + q/2 + c*x^2), x] - d^2/2*(b/q - 1)*Int[(d*x)^(m - 2)/(b/2 - q/2 + c*x^2), x]] /; FreeQ[{a, b, c, d}, x] && NeQ[b^2 - 4*a*c, 0] && GeQ[m, 2]
Int[(d_.*x_)^m_./(a_ + b_.*x_^2 + c_.*x_^4), x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, c/q*Int[(d*x)^m/(b/2 - q/2 + c*x^2), x] - c/q*Int[(d*x)^m/(b/2 + q/2 + c*x^2), x]] /; FreeQ[{a, b, c, d, m}, x] && NeQ[b^2 - 4*a*c, 0]
Int[x_^2/Sqrt[a_ + b_.*x_^2 + c_.*x_^4], x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, 2*Sqrt[-c]* Int[x^2/(Sqrt[b + q + 2*c*x^2]*Sqrt[-b + q - 2*c*x^2]), x]] /; FreeQ[{a, b, c}, x] && GtQ[b^2 - 4*a*c, 0] && LtQ[c, 0]
Int[x_^2/Sqrt[a_ + b_.*x_^2 + c_.*x_^4], x_Symbol] := With[{q = Rt[c/a, 2]}, 1/q*Int[1/Sqrt[a + b*x^2 + c*x^4], x] - 1/q*Int[(1 - q*x^2)/Sqrt[a + b*x^2 + c*x^4], x]] /; FreeQ[{a, b, c}, x] && GtQ[b^2 - 4*a*c, 0] && GtQ[c/a, 0] && LtQ[b/a, 0]
Int[x_^2/Sqrt[a_ + b_.*x_^2 + c_.*x_^4], x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, -(b - q)/(2*c)*Int[1/Sqrt[a + b*x^2 + c*x^4], x] + 1/(2*c)*Int[(b - q + 2*c*x^2)/Sqrt[a + b*x^2 + c*x^4], x]] /; FreeQ[{a, b, c}, x] && GtQ[b^2 - 4*a*c, 0] && LtQ[a, 0] && GtQ[c, 0]
Int[x_^2/Sqrt[a_ + b_.*x_^2 + c_.*x_^4], x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, x*(b + q + 2*c*x^2)/(2*c*Sqrt[a + b*x^2 + c*x^4]) - Rt[(b + q)/(2*a), 2]*(2*a + (b + q)*x^2)* Sqrt[(2*a + (b - q)*x^2)/(2*a + (b + q)*x^2)]/(2*c* Sqrt[a + b*x^2 + c*x^4])* EllipticE[ArcTan[Rt[(b + q)/(2*a), 2]*x], 2*q/(b + q)] /; PosQ[(b + q)/a] && Not[PosQ[(b - q)/a] && SimplerSqrtQ[(b - q)/(2*a), (b + q)/(2*a)]]] /; FreeQ[{a, b, c}, x] && GtQ[b^2 - 4*a*c, 0]
Int[x_^2/Sqrt[a_ + b_.*x_^2 + c_.*x_^4], x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, x*(b - q + 2*c*x^2)/(2*c*Sqrt[a + b*x^2 + c*x^4]) - Rt[(b - q)/(2*a), 2]*(2*a + (b - q)*x^2)* Sqrt[(2*a + (b + q)*x^2)/(2*a + (b - q)*x^2)]/(2*c* Sqrt[a + b*x^2 + c*x^4])* EllipticE[ArcTan[Rt[(b - q)/(2*a), 2]*x], -2*q/(b - q)] /; PosQ[(b - q)/a]] /; FreeQ[{a, b, c}, x] && GtQ[b^2 - 4*a*c, 0]
Int[x_^2/Sqrt[a_ + b_.*x_^2 + c_.*x_^4], x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, -(b + q)/(2*c)*Int[1/Sqrt[a + b*x^2 + c*x^4], x] + 1/(2*c)*Int[(b + q + 2*c*x^2)/Sqrt[a + b*x^2 + c*x^4], x] /; NegQ[(b + q)/a] && Not[NegQ[(b - q)/a] && SimplerSqrtQ[-(b - q)/(2*a), -(b + q)/(2*a)]]] /; FreeQ[{a, b, c}, x] && GtQ[b^2 - 4*a*c, 0]
Int[x_^2/Sqrt[a_ + b_.*x_^2 + c_.*x_^4], x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, -(b - q)/(2*c)*Int[1/Sqrt[a + b*x^2 + c*x^4], x] + 1/(2*c)*Int[(b - q + 2*c*x^2)/Sqrt[a + b*x^2 + c*x^4], x] /; NegQ[(b - q)/a]] /; FreeQ[{a, b, c}, x] && GtQ[b^2 - 4*a*c, 0]
Int[x_^2/Sqrt[a_ + b_.*x_^2 + c_.*x_^4], x_Symbol] := With[{q = Rt[c/a, 2]}, 1/q*Int[1/Sqrt[a + b*x^2 + c*x^4], x] - 1/q*Int[(1 - q*x^2)/Sqrt[a + b*x^2 + c*x^4], x]] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && PosQ[c/a]
Int[x_^2/Sqrt[a_ + b_.*x_^2 + c_.*x_^4], x_Symbol] := With[{q = Rt[b^2 - 4*a*c, 2]}, Sqrt[1 + 2*c*x^2/(b - q)]* Sqrt[1 + 2*c*x^2/(b + q)]/Sqrt[a + b*x^2 + c*x^4]* Int[x^2/(Sqrt[1 + 2*c*x^2/(b - q)]*Sqrt[1 + 2*c*x^2/(b + q)]), x]] /; FreeQ[{a, b, c}, x] && NeQ[b^2 - 4*a*c, 0] && NegQ[c/a]
Int[(d_.*x_)^m_.*(a_ + b_.*x_^2 + c_.*x_^4)^p_, x_Symbol] := a^IntPart[p]*(a + b*x^2 + c*x^4)^FracPart[p]/ ((1 + 2*c*x^2/(b + Rt[b^2 - 4*a*c, 2]))^ FracPart[p]*(1 + 2*c*x^2/(b - Rt[b^2 - 4*a*c, 2]))^FracPart[p])* Int[(d*x)^m*(1 + 2*c*x^2/(b + Sqrt[b^2 - 4*a*c]))^ p*(1 + 2*c*x^2/(b - Sqrt[b^2 - 4*a*c]))^p, x] /; FreeQ[{a, b, c, d, m, p}, x]
Int[u_^m_.*(a_. + b_.*v_^2 + c_.*v_^4)^p_., x_Symbol] := u^m/(Coefficient[v, x, 1]*v^m)* Subst[Int[x^m*(a + b*x^2 + c*x^(2*2))^p, x], x, v] /; FreeQ[{a, b, c, m, p}, x] && LinearPairQ[u, v, x]
