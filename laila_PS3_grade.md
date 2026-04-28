## PS3 Grade

**Total Score: 16/18**

#### Overall Comments: Please format the assignment with each part of the questions labeled, otherwise it makes it challenging to find your responses to each question.  

---

#### Question 1: Your own data

1a) Dataset description: 1/1

1b) Outcome, treatment, control, and causal story: 1/1

1c) Difference-in-means and SE by hand: 1/1

1d) LPM and logit table; coef matches: 0.5/1

1e) Predicted probabilities from logit: 1/1

1f) Marginal effect plot: 0.5/1

1g) Average marginal effect and compare: 0.5/1

**Comments:** For Part C, this really should just be the difference in your outcome based on the treatment; the control variable causes issues (as the NaN's show up) when you try to compute a difference for the continuous release variable. In Part D, the logit is misinterpreted, confusing the reported odds ratio with a probability difference. For Part F, you plot the LPM model, not the logit. The LPM model will have a constant ME by design since it's linear. You didn't indicate this and instead attribute it to being of "no meaningful interaction" versus using the wrong model. Part G: the logit coefficient does not measure the linear approximation of the treatment variable.

---

#### Question 2: Classical vs. robust standard errors

2a) LPM classical and HC2 SEs: 1/1

2b) Percent differences and discussion: 1/1

2c) Simulation: 1/1

2d) 95% CIs: 1/1

2e) Coverage rates: 1/1

2f) Discussion: 1/1

**Comments:** In the set up for this question, the data isn't cleaned correctly so the predictions from the models are slightly off; the intended filtering of when dead==1 but death_cause is missing doesn't happen due to the second conditon. The correct code would filter like this: filter(!is.na(dead), !(dead == 1 & is.na(death_cause))). For Parts C and D, you should show the output you get (with head() or something similar) so I can check the results.

---

#### Question 3: Decomposing a standard error

3a) Bivariate LPM SE: 1/1

3b) Bivariate SE by hand: 1/1

3c) Multivariate LPM SE: 1/1

3d) Multivariate SE by hand: 1/1

3e) Comparison: $\hat\sigma^2$, $R^2_j$, and net effect on SE correctly discussed: 0.5/1

**Comments:** The incorrect r-squared is presented (it should be about 0.33).  

---

#### Question 4: Derive the logit's marginal effect — *optional, not graded*

**Comments:** N/A

---
