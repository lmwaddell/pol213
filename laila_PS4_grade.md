## PS4 Grade

**Total Score: 16.5/21**

#### Overall Comments: Your HTML file is not rendered properly with the embedded plots and output. Please check this for future submissions as it is time consuming to open and run the .qmd file to see your responses.

---

#### Question 1: The full logit workflow on Fearon & Laitin

1a) Sample size and base rate: 0.5/1

1b) Logit regression table: 1/1

1c) Plot: 1/1

1d) Average marginal effect of `numlang`: 0.5/1

1e) AME anchored against base rate: 1/1

1f) Results: 0/1

**Comments:** In Part A, you didn't report the number of country-years in the sample.	1B, note that log-odds are interpretable, just difficult to understand. For instance, the positive beta for the number of languages indicates that more languages -> higher onset risk on the log-odds scale. Part D framing is slightly off. More accurate: One additional language is associated with about a 0.0054 increase in the probability of conflict onset, averaged across the sample.	For Part F, 30% is the relative increase, not percentage-point change. Confidence intervals and point estimate not mentioned. 
---

#### Question 2: Searching for the MLE of `numlang` by hand

2a) MLEs pulled from Q1 `glm()`: 1/1

2b) `log_lik()` function written and verified: 1/1

2c) Grid of candidate values: 1/1

2d) Plot and interpretation: 1/1

Optional methods question) Full 4-parameter `optim()`: NA

**Comments:** You have a large window, 0.05 would have been better to show the cleaner bell shape around the peak.

---

#### Question 3: Slope and Hessian at four points

3a) Four slopes: 1/1

3b) Interpret sign: 1/1

3c) Interpret magnitude: 1/1

3d) Report Hessian and interpret: 1/1

---

#### Question 4: How sample size sharpens the likelihood

4a) Three subsamples: 1/1

4b) Refit `glm()` for each subsample, `log_lik()` grid: 0.5/1

4c) Plot: 0.5/1

4d) Plot interpretation: 0/1

**Comments:** In Part B, you have mutate(loglike_values = map_dbl(b_numlang, log_lik)) in all samples instead of what you previously made for each: log_small, log_medium, and log_full. The plot is incorrect due to the error in 4c. This isn't addressed. For Part D, you describe the curves being the same. Given the concept being demonstrated and explicitly written in the assignment prompt (regarding recentering and common window in order to compare the different curves), all three curves being the same should have signaled that something went wrong in the code. 

---

#### Question 5: Counting affairs — OLS, Poisson, and Negative Binomial

5a) Table and discussion: 0.5/1

5b) Plot: 1/1

5c) Interpretation: 1/1

**Comments:** In Part A, the table is correct, but it is incorrect that "the poisson model fits the data best given that it has the smaller errors."The Poisson SEs are artificially small due to overdispersion. For Part C, it would be beneficial to note: Poisson and negative binomial curves bend toward zero but stay positive (the log link guarantees this), while the OLS line keeps falling along a straight slope and dips into negative territory, predicting negative affair counts, which is impossible. 

---
