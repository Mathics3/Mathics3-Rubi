(* ::Package:: *)

(* Section 1 *)

{0, x, 1, 0}
{1, x, 1, x}
{5, x, 1, 5*x}
{-2, x, 1, -2*x}
{-3/2, x, 1, -3/2*x}
{Pi, x, 1, Pi*x}
{a, x, 1, a*x}
{3*a, x, 1, 3*a*x}
{Pi/Sqrt[16 - E^2], x, 1, (Pi*x)/Sqrt[16 - E^2]}

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

{x^(5/2), x, 1, 2*x^(7/2)/7}
{x^(3/2), x, 1, 2*x^(5/2)/5}
{x^(1/2), x, 1, 2*x^(3/2)/3}
{1/x^(1/2), x, 1, 2*Sqrt[x]}
{1/x^(3/2), x, 1, -2/Sqrt[x]}
{1/x^(5/2), x, 1, -2/(3*x^(3/2))}

(* Section 2 *)

{F^(c*(a + b*x))*(d + e*x)^m, x, 1, (F^(c*(a - (b*d)/e))*(d + e*x)^m*Gamma[1 + m, -((b*c*(d + e*x)*Log[F])/e)])/((-((b*c*(d + e*x)*Log[F])/e))^m*(b*c*Log[F]))}

{F^(c*(a + b*x))*(d + e*x)^4, x, 5, (24*e^4*F^(c*(a + b*x)))/(b^5*c^5*Log[F]^5) - (24*e^3*F^(c*(a + b*x))*(d + e*x))/(b^4*c^4*Log[F]^4) + (12*e^2*F^(c*(a + b*x))*(d + e*x)^2)/(b^3*c^3*Log[F]^3) - (4*e*F^(c*(a + b*x))*(d + e*x)^3)/(b^2*c^2*Log[F]^2) + (F^(c*(a + b*x))*(d + e*x)^4)/(b*c*Log[F])}
{F^(c*(a + b*x))*(d + e*x)^3, x, 4, -((6*e^3*F^(c*(a + b*x)))/(b^4*c^4*Log[F]^4)) + (6*e^2*F^(c*(a + b*x))*(d + e*x))/(b^3*c^3*Log[F]^3) - (3*e*F^(c*(a + b*x))*(d + e*x)^2)/(b^2*c^2*Log[F]^2) + (F^(c*(a + b*x))*(d + e*x)^3)/(b*c*Log[F])}
{F^(c*(a + b*x))*(d + e*x)^2, x, 3, (2*e^2*F^(c*(a + b*x)))/(b^3*c^3*Log[F]^3) - (2*e*F^(c*(a + b*x))*(d + e*x))/(b^2*c^2*Log[F]^2) + (F^(c*(a + b*x))*(d + e*x)^2)/(b*c*Log[F])}
{F^(c*(a + b*x))*(d + e*x)^1, x, 2, -((e*F^(c*(a + b*x)))/(b^2*c^2*Log[F]^2)) + (F^(c*(a + b*x))*(d + e*x))/(b*c*Log[F])}
{F^(c*(a + b*x))*(d + e*x)^0, x, 1, F^(c*(a + b*x))/(b*c*Log[F])}
{F^(c*(a + b*x))/(d + e*x)^1, x, 1, (F^(c*(a - (b*d)/e))*ExpIntegralEi[(b*c*(d + e*x)*Log[F])/e])/e}
{F^(c*(a + b*x))/(d + e*x)^2, x, 2, -(F^(c*(a + b*x))/(e*(d + e*x))) + (b*c*F^(c*(a - (b*d)/e))*ExpIntegralEi[(b*c*(d + e*x)*Log[F])/e]*Log[F])/e^2}
{F^(c*(a + b*x))/(d + e*x)^3, x, 3, -(F^(c*(a + b*x))/(2*e*(d + e*x)^2)) - (b*c*F^(c*(a + b*x))*Log[F])/(2*e^2*(d + e*x)) + (b^2*c^2*F^(c*(a - (b*d)/e))*ExpIntegralEi[(b*c*(d + e*x)*Log[F])/e]*Log[F]^2)/(2*e^3)}
{F^(c*(a + b*x))/(d + e*x)^4, x, 4, -(F^(c*(a + b*x))/(3*e*(d + e*x)^3)) - (b*c*F^(c*(a + b*x))*Log[F])/(6*e^2*(d + e*x)^2) - (b^2*c^2*F^(c*(a + b*x))*Log[F]^2)/(6*e^3*(d + e*x)) + (b^3*c^3*F^(c*(a - (b*d)/e))*ExpIntegralEi[(b*c*(d + e*x)*Log[F])/e]*Log[F]^3)/(6*e^4)}
{F^(c*(a + b*x))/(d + e*x)^5, x, 5, -(F^(c*(a + b*x))/(4*e*(d + e*x)^4)) - (b*c*F^(c*(a + b*x))*Log[F])/(12*e^2*(d + e*x)^3) - (b^2*c^2*F^(c*(a + b*x))*Log[F]^2)/(24*e^3*(d + e*x)^2) - (b^3*c^3*F^(c*(a + b*x))*Log[F]^3)/(24*e^4*(d + e*x)) + (b^4*c^4*F^(c*(a - (b*d)/e))*ExpIntegralEi[(b*c*(d + e*x)*Log[F])/e]*Log[F]^4)/(24*e^5)}

(*
(* Section 3 *)
{x^3*Log[c*x], x, 1, -x^4/16 + (x^4*Log[c*x])/4}
{x^2*Log[c*x], x, 1, -x^3/9 + (x^3*Log[c*x])/3}
{x^1*Log[c*x], x, 1, -x^2/4 + (x^2*Log[c*x])/2}
{x^0*Log[c*x], x, 1, -x + x*Log[c*x]}
{Log[c*x]/x^1, x, 1, Log[c*x]^2/2}
{Log[c*x]/x^2, x, 1, -x^(-1) - Log[c*x]/x}
{Log[c*x]/x^3, x, 1, -1/(4*x^2) - Log[c*x]/(2*x^2)}


{x^3*Log[c*x]^2, x, 2, x^4/32 - (x^4*Log[c*x])/8 + (x^4*Log[c*x]^2)/4}
{x^2*Log[c*x]^2, x, 2, (2*x^3)/27 - (2*x^3*Log[c*x])/9 + (x^3*Log[c*x]^2)/3}
{x^1*Log[c*x]^2, x, 2, x^2/4 - (x^2*Log[c*x])/2 + (x^2*Log[c*x]^2)/2}
{x^0*Log[c*x]^2, x, 2, 2*x - 2*x*Log[c*x] + x*Log[c*x]^2}
{Log[c*x]^2/x^1, x, 2, Log[c*x]^3/3}
{Log[c*x]^2/x^2, x, 2, -2/x - (2*Log[c*x])/x - Log[c*x]^2/x}
{Log[c*x]^2/x^3, x, 2, -1/(4*x^2) - Log[c*x]/(2*x^2) - Log[c*x]^2/(2*x^2)}

(* Section 4 *)
{Sin[a + b*x]^1, x, 1, -(Cos[a + b*x]/b)}
{Sin[a + b*x]^2, x, 2, x/2 - (Cos[a + b*x]*Sin[a + b*x])/(2*b)}
{Sin[a + b*x]^3, x, 2, -(Cos[a + b*x]/b) + Cos[a + b*x]^3/(3*b)}
{Sin[a + b*x]^4, x, 3, (3*x)/8 - (3*Cos[a + b*x]*Sin[a + b*x])/(8*b) - (Cos[a + b*x]*Sin[a + b*x]^3)/(4*b)}
{Sin[a + b*x]^5, x, 2, -(Cos[a + b*x]/b) + (2*Cos[a + b*x]^3)/(3*b) - Cos[a + b*x]^5/(5*b)}
{Sin[a + b*x]^6, x, 4, (5*x)/16 - (5*Cos[a + b*x]*Sin[a + b*x])/(16*b) - (5*Cos[a + b*x]*Sin[a + b*x]^3)/(24*b) - (Cos[a + b*x]*Sin[a + b*x]^5)/(6*b)}
{Sin[a + b*x]^7, x, 2, -(Cos[a + b*x]/b) + Cos[a + b*x]^3/b - (3*Cos[a + b*x]^5)/(5*b) + Cos[a + b*x]^7/(7*b)}
{Sin[a + b*x]^8, x, 5, (35*x)/128 - (35*Cos[a + b*x]*Sin[a + b*x])/(128*b) - (35*Cos[a + b*x]*Sin[a + b*x]^3)/(192*b) - (7*Cos[a + b*x]*Sin[a + b*x]^5)/(48*b) - (Cos[a + b*x]*Sin[a + b*x]^7)/(8*b)}

{Sin[b*x]^(7/2), x, 3, -((10*EllipticF[Pi/4 - (b*x)/2, 2])/(21*b)) - (10*Cos[b*x]*Sqrt[Sin[b*x]])/(21*b) - (2*Cos[b*x]*Sin[b*x]^(5/2))/(7*b)}
{Sin[b*x]^(5/2), x, 2, -((6*EllipticE[Pi/4 - (b*x)/2, 2])/(5*b)) - (2*Cos[b*x]*Sin[b*x]^(3/2))/(5*b)}
{Sin[b*x]^(3/2), x, 2, -((2*EllipticF[Pi/4 - (b*x)/2, 2])/(3*b)) - (2*Cos[b*x]*Sqrt[Sin[b*x]])/(3*b)}
{Sin[b*x]^(1/2), x, 1, -((2*EllipticE[Pi/4 - (b*x)/2, 2])/b)}
{1/Sin[b*x]^(1/2), x, 1, -((2*EllipticF[Pi/4 - (b*x)/2, 2])/b)}
{1/Sin[b*x]^(3/2), x, 2, (2*EllipticE[Pi/4 - (b*x)/2, 2])/b - (2*Cos[b*x])/(b*Sqrt[Sin[b*x]])}
{1/Sin[b*x]^(5/2), x, 2, -((2*EllipticF[Pi/4 - (b*x)/2, 2])/(3*b)) - (2*Cos[b*x])/(3*b*Sin[b*x]^(3/2))}
{1/Sin[b*x]^(7/2), x, 3, (6*EllipticE[Pi/4 - (b*x)/2, 2])/(5*b) - (2*Cos[b*x])/(5*b*Sin[b*x]^(5/2)) - (6*Cos[b*x])/(5*b*Sqrt[Sin[b*x]])}

(* Section 5 *)
{x^6*(a + b*ArcCsc[c*x]), x, 7, (5*b*Sqrt[1 - 1/(c^2*x^2)]*x^2)/(112*c^5) + (5*b*Sqrt[1 - 1/(c^2*x^2)]*x^4)/(168*c^3) + (b*Sqrt[1 - 1/(c^2*x^2)]*x^6)/(42*c) + (x^7*(a + b*ArcCsc[c*x]))/7 + (5*b*ArcTanh[Sqrt[1 - 1/(c^2*x^2)]])/(112*c^7)}
{x^5*(a + b*ArcCsc[c*x]), x, 4, (4*b*Sqrt[1 - 1/(c^2*x^2)]*x)/(45*c^5) + (2*b*Sqrt[1 - 1/(c^2*x^2)]*x^3)/(45*c^3) + (b*Sqrt[1 - 1/(c^2*x^2)]*x^5)/(30*c) + (x^6*(a + b*ArcCsc[c*x]))/6}
{x^4*(a + b*ArcCsc[c*x]), x, 6, (3*b*Sqrt[1 - 1/(c^2*x^2)]*x^2)/(40*c^3) + (b*Sqrt[1 - 1/(c^2*x^2)]*x^4)/(20*c) + (x^5*(a + b*ArcCsc[c*x]))/5 + (3*b*ArcTanh[Sqrt[1 - 1/(c^2*x^2)]])/(40*c^5)}
{x^3*(a + b*ArcCsc[c*x]), x, 3, (b*Sqrt[1 - 1/(c^2*x^2)]*x)/(6*c^3) + (b*Sqrt[1 - 1/(c^2*x^2)]*x^3)/(12*c) + (x^4*(a + b*ArcCsc[c*x]))/4}
{x^2*(a + b*ArcCsc[c*x]), x, 5, (b*Sqrt[1 - 1/(c^2*x^2)]*x^2)/(6*c) + (x^3*(a + b*ArcCsc[c*x]))/3 + (b*ArcTanh[Sqrt[1 - 1/(c^2*x^2)]])/(6*c^3)}
{x*(a + b*ArcCsc[c*x]), x, 2, (b*Sqrt[1 - 1/(c^2*x^2)]*x)/(2*c) + (x^2*(a + b*ArcCsc[c*x]))/2}
{a + b*ArcCsc[c*x], x, 5, a*x + b*x*ArcCsc[c*x] + (b*ArcTanh[Sqrt[1 - 1/(c^2*x^2)]])/c}
{(a + b*ArcCsc[c*x])/x, x, 6, ((I/2)*(a + b*ArcCsc[c*x])^2)/b - (a + b*ArcCsc[c*x])*Log[1 - E^((2*I)*ArcCsc[c*x])] + (I/2)*b*PolyLog[2, E^((2*I)*ArcCsc[c*x])]}
{(a + b*ArcCsc[c*x])/x^2, x, 2, -(b*c*Sqrt[1 - 1/(c^2*x^2)]) - (a + b*ArcCsc[c*x])/x}
{(a + b*ArcCsc[c*x])/x^3, x, 4, -(b*c*Sqrt[1 - 1/(c^2*x^2)])/(4*x) + (b*c^2*ArcCsc[c*x])/4 - (a + b*ArcCsc[c*x])/(2*x^2)}
{(a + b*ArcCsc[c*x])/x^4, x, 4, -(b*c^3*Sqrt[1 - 1/(c^2*x^2)])/3 + (b*c^3*(1 - 1/(c^2*x^2))^(3/2))/9 - (a + b*ArcCsc[c*x])/(3*x^3)}
{(a + b*ArcCsc[c*x])/x^5, x, 5, -(b*c*Sqrt[1 - 1/(c^2*x^2)])/(16*x^3) - (3*b*c^3*Sqrt[1 - 1/(c^2*x^2)])/(32*x) + (3*b*c^4*ArcCsc[c*x])/32 - (a + b*ArcCsc[c*x])/(4*x^4)}
{(a + b*ArcCsc[c*x])/x^6, x, 4, -(b*c^5*Sqrt[1 - 1/(c^2*x^2)])/5 + (2*b*c^5*(1 - 1/(c^2*x^2))^(3/2))/15 - (b*c^5*(1 - 1/(c^2*x^2))^(5/2))/25 - (a + b*ArcCsc[c*x])/(5*x^5)}
{(a + b*ArcCsc[c*x])/x^7, x, 6, -(b*c*Sqrt[1 - 1/(c^2*x^2)])/(36*x^5) - (5*b*c^3*Sqrt[1 - 1/(c^2*x^2)])/(144*x^3) - (5*b*c^5*Sqrt[1 - 1/(c^2*x^2)])/(96*x) + (5*b*c^6*ArcCsc[c*x])/96 - (a + b*ArcCsc[c*x])/(6*x^6)}


{x^3*(a + b*ArcCsc[c*x])^2, x, 5, (b^2*x^2)/(12*c^2) + (b*Sqrt[1 - 1/(c^2*x^2)]*x*(a + b*ArcCsc[c*x]))/(3*c^3) + (b*Sqrt[1 - 1/(c^2*x^2)]*x^3*(a + b*ArcCsc[c*x]))/(6*c) + (x^4*(a + b*ArcCsc[c*x])^2)/4 + (b^2*Log[x])/(3*c^4)}
{x^2*(a + b*ArcCsc[c*x])^2, x, 8, (b^2*x)/(3*c^2) + (b*Sqrt[1 - 1/(c^2*x^2)]*x^2*(a + b*ArcCsc[c*x]))/(3*c) + (x^3*(a + b*ArcCsc[c*x])^2)/3 + (2*b*(a + b*ArcCsc[c*x])*ArcTanh[E^(I*ArcCsc[c*x])])/(3*c^3) - ((I/3)*b^2*PolyLog[2, -E^(I*ArcCsc[c*x])])/c^3 + ((I/3)*b^2*PolyLog[2, E^(I*ArcCsc[c*x])])/c^3}
{x*(a + b*ArcCsc[c*x])^2, x, 4, (b*Sqrt[1 - 1/(c^2*x^2)]*x*(a + b*ArcCsc[c*x]))/c + (x^2*(a + b*ArcCsc[c*x])^2)/2 + (b^2*Log[x])/c^2}
{(a + b*ArcCsc[c*x])^2, x, 7, x*(a + b*ArcCsc[c*x])^2 + (4*b*(a + b*ArcCsc[c*x])*ArcTanh[E^(I*ArcCsc[c*x])])/c - ((2*I)*b^2*PolyLog[2, -E^(I*ArcCsc[c*x])])/c + ((2*I)*b^2*PolyLog[2, E^(I*ArcCsc[c*x])])/c}
{(a + b*ArcCsc[c*x])^2/x, x, 6, ((I/3)*(a + b*ArcCsc[c*x])^3)/b - (a + b*ArcCsc[c*x])^2*Log[1 - E^((2*I)*ArcCsc[c*x])] + I*b*(a + b*ArcCsc[c*x])*PolyLog[2, E^((2*I)*ArcCsc[c*x])] - (b^2*PolyLog[3, E^((2*I)*ArcCsc[c*x])])/2}
{(a + b*ArcCsc[c*x])^2/x^2, x, 4, (2*b^2)/x - 2*b*c*Sqrt[1 - 1/(c^2*x^2)]*(a + b*ArcCsc[c*x]) - (a + b*ArcCsc[c*x])^2/x}
{(a + b*ArcCsc[c*x])^2/x^3, x, 4, b^2/(4*x^2) + (a*b*c^2*ArcCsc[c*x])/2 + (b^2*c^2*ArcCsc[c*x]^2)/4 - (b*c*Sqrt[1 - 1/(c^2*x^2)]*(a + b*ArcCsc[c*x]))/(2*x) - (a + b*ArcCsc[c*x])^2/(2*x^2)}
{(a + b*ArcCsc[c*x])^2/x^4, x, 5, (2*b^2)/(27*x^3) + (4*b^2*c^2)/(9*x) - (4*b*c^3*Sqrt[1 - 1/(c^2*x^2)]*(a + b*ArcCsc[c*x]))/9 - (2*b*c*Sqrt[1 - 1/(c^2*x^2)]*(a + b*ArcCsc[c*x]))/(9*x^2) - (a + b*ArcCsc[c*x])^2/(3*x^3)}
{(a + b*ArcCsc[c*x])^2/x^5, x, 5, b^2/(32*x^4) + (3*b^2*c^2)/(32*x^2) + (3*a*b*c^4*ArcCsc[c*x])/16 + (3*b^2*c^4*ArcCsc[c*x]^2)/32 - (b*c*Sqrt[1 - 1/(c^2*x^2)]*(a + b*ArcCsc[c*x]))/(8*x^3) - (3*b*c^3*Sqrt[1 - 1/(c^2*x^2)]*(a + b*ArcCsc[c*x]))/(16*x) - (a + b*ArcCsc[c*x])^2/(4*x^4)}

(* Section 6 *)
{(c + d*x)^4*Sinh[a + b*x], x, 5, (24*d^4*Cosh[a + b*x])/b^5 + (12*d^2*(c + d*x)^2*Cosh[a + b*x])/b^3 + ((c + d*x)^4*Cosh[a + b*x])/b - (24*d^3*(c + d*x)*Sinh[a + b*x])/b^4 - (4*d*(c + d*x)^3*Sinh[a + b*x])/b^2}
{(c + d*x)^3*Sinh[a + b*x], x, 4, (6*d^2*(c + d*x)*Cosh[a + b*x])/b^3 + ((c + d*x)^3*Cosh[a + b*x])/b - (6*d^3*Sinh[a + b*x])/b^4 - (3*d*(c + d*x)^2*Sinh[a + b*x])/b^2}
{(c + d*x)^2*Sinh[a + b*x], x, 3, (2*d^2*Cosh[a + b*x])/b^3 + ((c + d*x)^2*Cosh[a + b*x])/b - (2*d*(c + d*x)*Sinh[a + b*x])/b^2}
{(c + d*x)*Sinh[a + b*x], x, 2, ((c + d*x)*Cosh[a + b*x])/b - (d*Sinh[a + b*x])/b^2}
{Sinh[a + b*x]/(c + d*x), x, 3, (CoshIntegral[(b*c)/d + b*x]*Sinh[a - (b*c)/d])/d + (Cosh[a - (b*c)/d]*SinhIntegral[(b*c)/d + b*x])/d}
{Sinh[a + b*x]/(c + d*x)^2, x, 4, (b*Cosh[a - (b*c)/d]*CoshIntegral[(b*c)/d + b*x])/d^2 - Sinh[a + b*x]/(d*(c + d*x)) + (b*Sinh[a - (b*c)/d]*SinhIntegral[(b*c)/d + b*x])/d^2}
{Sinh[a + b*x]/(c + d*x)^3, x, 5, -(b*Cosh[a + b*x])/(2*d^2*(c + d*x)) + (b^2*CoshIntegral[(b*c)/d + b*x]*Sinh[a - (b*c)/d])/(2*d^3) - Sinh[a + b*x]/(2*d*(c + d*x)^2) + (b^2*Cosh[a - (b*c)/d]*SinhIntegral[(b*c)/d + b*x])/(2*d^3)}


{(c + d*x)^4*Sinh[a + b*x]^2, x, 6, (-3*d^4*x)/(4*b^4) - (d*(c + d*x)^3)/(2*b^2) - (c + d*x)^5/(10*d) + (3*d^4*Cosh[a + b*x]*Sinh[a + b*x])/(4*b^5) + (3*d^2*(c + d*x)^2*Cosh[a + b*x]*Sinh[a + b*x])/(2*b^3) + ((c + d*x)^4*Cosh[a + b*x]*Sinh[a + b*x])/(2*b) - (3*d^3*(c + d*x)*Sinh[a + b*x]^2)/(2*b^4) - (d*(c + d*x)^3*Sinh[a + b*x]^2)/b^2}
{(c + d*x)^3*Sinh[a + b*x]^2, x, 4, (-3*c*d^2*x)/(4*b^2) - (3*d^3*x^2)/(8*b^2) - (c + d*x)^4/(8*d) + (3*d^2*(c + d*x)*Cosh[a + b*x]*Sinh[a + b*x])/(4*b^3) + ((c + d*x)^3*Cosh[a + b*x]*Sinh[a + b*x])/(2*b) - (3*d^3*Sinh[a + b*x]^2)/(8*b^4) - (3*d*(c + d*x)^2*Sinh[a + b*x]^2)/(4*b^2)}
{(c + d*x)^2*Sinh[a + b*x]^2, x, 4, -(d^2*x)/(4*b^2) - (c + d*x)^3/(6*d) + (d^2*Cosh[a + b*x]*Sinh[a + b*x])/(4*b^3) + ((c + d*x)^2*Cosh[a + b*x]*Sinh[a + b*x])/(2*b) - (d*(c + d*x)*Sinh[a + b*x]^2)/(2*b^2)}
{(c + d*x)*Sinh[a + b*x]^2, x, 2, -(c*x)/2 - (d*x^2)/4 + ((c + d*x)*Cosh[a + b*x]*Sinh[a + b*x])/(2*b) - (d*Sinh[a + b*x]^2)/(4*b^2)}
{Sinh[a + b*x]^2/(c + d*x), x, 5, (Cosh[2*a - (2*b*c)/d]*CoshIntegral[(2*b*c)/d + 2*b*x])/(2*d) - Log[c + d*x]/(2*d) + (Sinh[2*a - (2*b*c)/d]*SinhIntegral[(2*b*c)/d + 2*b*x])/(2*d)}
{Sinh[a + b*x]^2/(c + d*x)^2, x, 5, (b*CoshIntegral[(2*b*c)/d + 2*b*x]*Sinh[2*a - (2*b*c)/d])/d^2 - Sinh[a + b*x]^2/(d*(c + d*x)) + (b*Cosh[2*a - (2*b*c)/d]*SinhIntegral[(2*b*c)/d + 2*b*x])/d^2}
{Sinh[a + b*x]^2/(c + d*x)^3, x, 7, (b^2*Cosh[2*a - (2*b*c)/d]*CoshIntegral[(2*b*c)/d + 2*b*x])/d^3 - (b*Cosh[a + b*x]*Sinh[a + b*x])/(d^2*(c + d*x)) - Sinh[a + b*x]^2/(2*d*(c + d*x)^2) + (b^2*Sinh[2*a - (2*b*c)/d]*SinhIntegral[(2*b*c)/d + 2*b*x])/d^3}
{Sinh[a + b*x]^2/(c + d*x)^4, x, 7, -b^2/(3*d^3*(c + d*x)) + (2*b^3*CoshIntegral[(2*b*c)/d + 2*b*x]*Sinh[2*a - (2*b*c)/d])/(3*d^4) - (b*Cosh[a + b*x]*Sinh[a + b*x])/(3*d^2*(c + d*x)^2) - Sinh[a + b*x]^2/(3*d*(c + d*x)^3) - (2*b^2*Sinh[a + b*x]^2)/(3*d^3*(c + d*x)) + (2*b^3*Cosh[2*a - (2*b*c)/d]*SinhIntegral[(2*b*c)/d + 2*b*x])/(3*d^4)}

(* Section 7 *)
{x^4*ArcSinh[a*x], x, 4, -(Sqrt[1 + a^2*x^2]/(5*a^5)) + (2*(1 + a^2*x^2)^(3/2))/(15*a^5) - (1 + a^2*x^2)^(5/2)/(25*a^5) + (1/5)*x^5*ArcSinh[a*x]}
{x^3*ArcSinh[a*x], x, 4, (3*x*Sqrt[1 + a^2*x^2])/(32*a^3) - (x^3*Sqrt[1 + a^2*x^2])/(16*a) - (3*ArcSinh[a*x])/(32*a^4) + (1/4)*x^4*ArcSinh[a*x]}
{x^2*ArcSinh[a*x], x, 4, Sqrt[1 + a^2*x^2]/(3*a^3) - (1 + a^2*x^2)^(3/2)/(9*a^3) + (1/3)*x^3*ArcSinh[a*x]}
{x^1*ArcSinh[a*x], x, 3, -((x*Sqrt[1 + a^2*x^2])/(4*a)) + ArcSinh[a*x]/(4*a^2) + (1/2)*x^2*ArcSinh[a*x]}
{x^0*ArcSinh[a*x], x, 2, -(Sqrt[1 + a^2*x^2]/a) + x*ArcSinh[a*x]}
{ArcSinh[a*x]/x^1, x, 5, (-(1/2))*ArcSinh[a*x]^2 + ArcSinh[a*x]*Log[1 - E^(2*ArcSinh[a*x])] + (1/2)*PolyLog[2, E^(2*ArcSinh[a*x])]}
{ArcSinh[a*x]/x^2, x, 4, -(ArcSinh[a*x]/x) - a*ArcTanh[Sqrt[1 + a^2*x^2]]}
{ArcSinh[a*x]/x^3, x, 2, -((a*Sqrt[1 + a^2*x^2])/(2*x)) - ArcSinh[a*x]/(2*x^2)}
{ArcSinh[a*x]/x^4, x, 5, -((a*Sqrt[1 + a^2*x^2])/(6*x^2)) - ArcSinh[a*x]/(3*x^3) + (1/6)*a^3*ArcTanh[Sqrt[1 + a^2*x^2]]}
{ArcSinh[a*x]/x^5, x, 3, -((a*Sqrt[1 + a^2*x^2])/(12*x^3)) + (a^3*Sqrt[1 + a^2*x^2])/(6*x) - ArcSinh[a*x]/(4*x^4)}
{ArcSinh[a*x]/x^6, x, 6, -((a*Sqrt[1 + a^2*x^2])/(20*x^4)) + (3*a^3*Sqrt[1 + a^2*x^2])/(40*x^2) - ArcSinh[a*x]/(5*x^5) - (3/40)*a^5*ArcTanh[Sqrt[1 + a^2*x^2]]}


{x^4*ArcSinh[a*x]^2, x, 7, (16*x)/(75*a^4) - (8*x^3)/(225*a^2) + (2*x^5)/125 - (16*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(75*a^5) + (8*x^2*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(75*a^3) - (2*x^4*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(25*a) + (1/5)*x^5*ArcSinh[a*x]^2}
{x^3*ArcSinh[a*x]^2, x, 6, (-3*x^2)/(32*a^2) + x^4/32 + (3*x*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(16*a^3) - (x^3*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(8*a) - (3*ArcSinh[a*x]^2)/(32*a^4) + (x^4*ArcSinh[a*x]^2)/4}
{x^2*ArcSinh[a*x]^2, x, 5, -((4*x)/(9*a^2)) + (2*x^3)/27 + (4*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(9*a^3) - (2*x^2*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(9*a) + (1/3)*x^3*ArcSinh[a*x]^2}
{x*ArcSinh[a*x]^2, x, 4, x^2/4 - (x*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(2*a) + ArcSinh[a*x]^2/(4*a^2) + (x^2*ArcSinh[a*x]^2)/2}
{ArcSinh[a*x]^2, x, 3, 2*x - (2*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/a + x*ArcSinh[a*x]^2}
{ArcSinh[a*x]^2/x, x, 6, -ArcSinh[a*x]^3/3 + ArcSinh[a*x]^2*Log[1 - E^(2*ArcSinh[a*x])] + ArcSinh[a*x]*PolyLog[2, E^(2*ArcSinh[a*x])] - PolyLog[3, E^(2*ArcSinh[a*x])]/2}
{ArcSinh[a*x]^2/x^2, x, 7, -(ArcSinh[a*x]^2/x) - 4*a*ArcSinh[a*x]*ArcTanh[E^ArcSinh[a*x]] - 2*a*PolyLog[2, -E^ArcSinh[a*x]] + 2*a*PolyLog[2, E^ArcSinh[a*x]]}
{ArcSinh[a*x]^2/x^3, x, 3, -((a*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/x) - ArcSinh[a*x]^2/(2*x^2) + a^2*Log[x]}
{ArcSinh[a*x]^2/x^4, x, 9, -(a^2/(3*x)) - (a*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(3*x^2) - ArcSinh[a*x]^2/(3*x^3) + (2/3)*a^3*ArcSinh[a*x]*ArcTanh[E^ArcSinh[a*x]] + (1/3)*a^3*PolyLog[2, -E^ArcSinh[a*x]] - (1/3)*a^3*PolyLog[2, E^ArcSinh[a*x]]}
{ArcSinh[a*x]^2/x^5, x, 5, -a^2/(12*x^2) - (a*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(6*x^3) + (a^3*Sqrt[1 + a^2*x^2]*ArcSinh[a*x])/(3*x) - ArcSinh[a*x]^2/(4*x^4) - (a^4*Log[x])/3}

(* Section 8 *)
{x^5*Erf[b*x], x, 5, (5*x)/(E^(b^2*x^2)*(8*b^5*Sqrt[Pi])) + (5*x^3)/(E^(b^2*x^2)*(12*b^3*Sqrt[Pi])) + x^5/(E^(b^2*x^2)*(6*b*Sqrt[Pi])) - (5*Erf[b*x])/(16*b^6) + (1/6)*x^6*Erf[b*x]}
{x^3*Erf[b*x], x, 4, (3*x)/(E^(b^2*x^2)*(8*b^3*Sqrt[Pi])) + x^3/(E^(b^2*x^2)*(4*b*Sqrt[Pi])) - (3*Erf[b*x])/(16*b^4) + (1/4)*x^4*Erf[b*x]}
{x^1*Erf[b*x], x, 3, x/(E^(b^2*x^2)*(2*b*Sqrt[Pi])) - Erf[b*x]/(4*b^2) + (1/2)*x^2*Erf[b*x]}
{Erf[b*x]/x^1, x, 1, (2*b*x*HypergeometricPFQ[{1/2, 1/2}, {3/2, 3/2}, (-b^2)*x^2])/Sqrt[Pi]}
{Erf[b*x]/x^3, x, 3, -(b/(E^(b^2*x^2)*(Sqrt[Pi]*x))) - b^2*Erf[b*x] - Erf[b*x]/(2*x^2)}
{Erf[b*x]/x^5, x, 4, -(b/(E^(b^2*x^2)*(6*Sqrt[Pi]*x^3))) + b^3/(E^(b^2*x^2)*(3*Sqrt[Pi]*x)) + (1/3)*b^4*Erf[b*x] - Erf[b*x]/(4*x^4)}
{Erf[b*x]/x^7, x, 5, -(b/(E^(b^2*x^2)*(15*Sqrt[Pi]*x^5))) + (2*b^3)/(E^(b^2*x^2)*(45*Sqrt[Pi]*x^3)) - (4*b^5)/(E^(b^2*x^2)*(45*Sqrt[Pi]*x)) - (4/45)*b^6*Erf[b*x] - Erf[b*x]/(6*x^6)}

{x^6*Erf[b*x], x, 5, 6/(E^(b^2*x^2)*(7*b^7*Sqrt[Pi])) + (6*x^2)/(E^(b^2*x^2)*(7*b^5*Sqrt[Pi])) + (3*x^4)/(E^(b^2*x^2)*(7*b^3*Sqrt[Pi])) + x^6/(E^(b^2*x^2)*(7*b*Sqrt[Pi])) + (1/7)*x^7*Erf[b*x]}
{x^4*Erf[b*x], x, 4, 2/(E^(b^2*x^2)*(5*b^5*Sqrt[Pi])) + (2*x^2)/(E^(b^2*x^2)*(5*b^3*Sqrt[Pi])) + x^4/(E^(b^2*x^2)*(5*b*Sqrt[Pi])) + (1/5)*x^5*Erf[b*x]}
{x^2*Erf[b*x], x, 3, 1/(E^(b^2*x^2)*(3*b^3*Sqrt[Pi])) + x^2/(E^(b^2*x^2)*(3*b*Sqrt[Pi])) + (1/3)*x^3*Erf[b*x]}
{x^0*Erf[b*x], x, 1, 1/(E^(b^2*x^2)*(b*Sqrt[Pi])) + x*Erf[b*x]}
{Erf[b*x]/x^2, x, 2, -(Erf[b*x]/x) + (b*ExpIntegralEi[(-b^2)*x^2])/Sqrt[Pi]}
{Erf[b*x]/x^4, x, 3, -(b/(E^(b^2*x^2)*(3*Sqrt[Pi]*x^2))) - Erf[b*x]/(3*x^3) - (b^3*ExpIntegralEi[(-b^2)*x^2])/(3*Sqrt[Pi])}
{Erf[b*x]/x^6, x, 4, -(b/(E^(b^2*x^2)*(10*Sqrt[Pi]*x^4))) + b^3/(E^(b^2*x^2)*(10*Sqrt[Pi]*x^2)) - Erf[b*x]/(5*x^5) + (b^5*ExpIntegralEi[(-b^2)*x^2])/(10*Sqrt[Pi])}
*)
