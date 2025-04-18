# README – Regression Discontinuity Design (RDD) Project

## What Problem Does RDD Solve?

The primary motivation behind using Regression Discontinuity Design (RDD) is to address and reduce selection bias in causal inference. In observational data, it's often difficult to establish causality due to underlying biases—RDD helps overcome this by leveraging a well-defined cutoff or threshold to isolate the treatment effect in a quasi-experimental setup.

## Key Concepts & Approach

In this project, we focus on the variables `score`, `democrat`, and `lagdemocrat` (which effectively captures Republican vote share as `1 - democrat`). We created a subsample that restricts vote shares between 48% and 52%, aligning with the foundational idea of RDD—using a clear threshold to differentiate treatment and control groups.

As highlighted by James, establishing such a cutoff point is central to the RDD methodology.

## Regression Results & Interpretation

We fit three linear regression models, each using `score`, `lagdemocrat`, and `democrat` as independent regressors. Across all models:

- The p-values are significantly below 0.01, indicating strong statistical significance, even under conservative alpha thresholds.
- The estimated coefficients are relatively large, suggesting a positive causal relationship.
- The high F-statistic values further reinforce the overall model fit.

These results support the divergence theory—that when Democrats maintain a liberal stance or Republicans adhere to right-wing ideologies, it reinforces their voter base and enhances electoral performance.

Additionally, residual plots show a bell-shaped pattern, hinting at the presence of an exogenous shock—a feature often explored within RDD frameworks to validate discontinuities at the cutoff.

## Observations on Incumbency Advantage

Notably, the coefficients and very low p-values for `democrat` and `lagdemocrat` also suggest an incumbency advantage, a well-documented phenomenon in political science where existing officeholders tend to attract more votes due to recognition, resources, or past performance.

## Variations and Model Behavior

1. Without a threshold, our results remain consistent, but the model no longer qualifies as a true RDD—since the defining cutoff is absent.

2. Introducing a third variable to the regression model did not drastically alter the outcome. However, its negative coefficient suggests an inverse relationship with existing regressors, implying some level of correlation or multicollinearity.
