This is a copy of  `Rubi <https://rulebasedintegration.org>`_, a Rule-based Integrator written for WMA.

The version 4.17. or code in this repository runs in Mathics3 though. Perhaps one day, this will not be needed. But for the time being, it is very expedient.

To run from inside Mathics3::

        $ git clone git@github.com:Mathic3/Mathics3-Rubi.git
        $ cd Mathics3-Rubi

        $ mathics

        Mathics 7.0.1dev0
        on CPython 3.11.10 ...
        ...
        In[1]:= << Rubi.m
        Loading Rubi 4.17.3.0 will take a minute or two. In the future this will take less than a second
	.
        Loading /home/ark/git/mathics/mathics-core/mathics/packages/Rubi/IntegrationUtilityFunctions.m...

	Loading /home/ark/git/mathics/mathics-core/mathics/packages/Rubi/IntegrationRules/1 Algebraic functions/1.1 Binomial products/1.1.1 Linear/1.1.1.1 (a+b x)^m.m...

        Loading /home/ark/git/mathics/mathics-core/mathics/packages/Rubi/IntegrationRules/1 Algebraic functions/1.1 Binomial products/1.1.1 Linear/1.1.1.2 (a+b x)^m (c+d x)^n.m...

        Loading /home/ark/git/mathics/mathics-core/mathics/packages/Rubi/IntegrationRules/1 Algebraic functions/1.1 Binomial products/1.1.1 Linear/1.1.1.3 (a+b x)^m (c+d x)^n (e+f x)^p.m...

        Loading /home/ark/git/mathics/mathics-core/mathics/packages/Rubi/IntegrationRules/1 Algebraic functions/1.1 Binomial products/1.1.1 Linear/1.1.1.4 (a+b x)^m (c+d x)^n (e+f x)^p (g+h x)^q.m...

        Out[1]= None

        In[2]:= Int[(b x + a)^m, x]
        Out[2]= (a + b x) ^ (1 + m) / (b ( + m))

        In[3]:= Int[x, x]
        Out[3]= x ^ 2 / 2

        In[4]:= Int[(a + x)^2, x]
        Out[4]= (a + x) ^ 3 /

        In[5]:= Int[x^3, x]
        Out[5]= x ^ 4 / 4

        In[6]:= Int[(a + x)^m, x]
        Out[6]= (a + x) ^ (1 + m) / (1 +m)

Tests were taken from Rubi's `Integration Test-suites <https://rulebasedintegration.org/testProblems.html>`_ Mathematica syntax.
