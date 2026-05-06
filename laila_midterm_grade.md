## Midterm Grade

**Total Score: 33.5/77**

#### Overall Comments: 
You demonstrate clear strengths with certain parts of the material. But, there are some gaps across other concepts that could use attention. Make sure to also read each question carefully and explain your thinking explicitly as I can only give credit for what you write on the page. I can't grade based on what I *think* you know. 

**Note:** If you want to see the exam questions again, please see Amanda in office hours. The exam itself will not be posted to the course website. 

---

#### Week 1 - OLS Review

**1\)** Interpret `sibs` coefficient  **1/5**

*Comments:* 1.5 probability isn't correct as probability is between 0 and 1. You mean 0.015 or 1.5 percentage points. The model is looking at voting for Obama, not predicting *not* voting for him. Be specific about what variables are being held constant.

**2\)** Which set of $\beta$'s is the better fit? **7/7**

**3a)** Why differentiate with respect to $\beta$'s versus $X$ and $Y$? **1/3**

*Comments:* Does not answer why

**3b)** Why set partial derivatives equal to 0? **2/2**

---

#### Week 2 - Transformations

**4a)** Fitted regression equation **1/2**

*Comments:* The fitted equation should have the variable names. The generic equation would have X's (you have $\beta$'s).

**4b)** Predicted level of conflict **2/3**

*Comments:* Calculation is slightly wrong. You get 7 but the correct answer is 5.

**4c)** Is 4b the max/min/neither and why? **0/3**

*Comments:* Maximum because of the negative coefficient on the squared term (or can calculate with algebra/calculus)

**5a)** Marginal effect for $\text{log}(Y) = \beta_0 + \beta_1X$ **1/2**

*Comments:* Misses structural idea that the effect grows with $Y$

**5b)** Marginal effect for $Y = \beta_0 + \beta_1 \text{log}(X)$ **0/2**

*Comments:* The correct idea is diminishing returns. This has nothing to do with a sigmoid in a logit model.

**5c)** Marginal effect for $\text{log}(Y) = \beta_0 + \beta_1\text{log}(X)$ **1/2**

*Comments:* Misses key structural idea that there is constant elasticity

**6a)** Describe two pieces of code (body inside the function and final line) **1/4**

*Comments:* The body of the function is one simulation run that generates 150 observations from a set DGP (etc.). You don't discuss what the final line is doing.

**6b)** Why simulate 500 iterations instead of 1? **0.5/1**

*Comments:* Why can't 1 iteration do this?

**6c)** Larger sample size, what will happen to `mean(results$p.value < 0.05)` and why? **1/3**

*Comments:* The proportion will increase substantially, not decrease.

---

#### Week 3 - Logit

**7\)** Predicted probability **0/5**

*Comments:* No answer given.

**8\)** Explain why the marginal effect depends on $Z$ **1/5**

*Comments:* Does not specifically name or write the scaling factor $p(1-p)$ or the proper explanation of the linear index or how $Z$ shifts $p$ connected to how the marginal effect changes

**9a)** Bug 1 (quote line and explanation) **5/5**

**9b)** Bug 2 (quote line and explanation) **5/5**

---

#### Week 4 - Maximum Likelihood Estimation

**10\)** Compare/contrast OLS versus MLE approach **3/5**

*Comments:* The answer is mostly about the structural differences rather than how beta is obtained in each case. Similar optimization framework should be discussed and the different way a solution is found (linear, algebraically versus numerical optimization)

**11a)** Should the algorithm move higher or lower and why? **0/2**

*Comments:* Should be lower (negative gradient)

**11b)** Role of Hessian and why? **1/3**

*Comments:* Vaguely captures curvature, but doesn't connect to what the algorithm actually does with the Hessian (step size)

**12\)** Perfect separation **0/8**

*Comments:* No answer given. 

---
