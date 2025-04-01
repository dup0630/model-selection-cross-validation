# Project: Cross-Validation for Model Selection

## To do
- Fix simulation efficiency (switch to k-fold where appropriate, verify correctness)
- Continue typing notes (fill in gaps in Shao papers, create next sections)
- Fix assumptions enumeration.
- I still don't understand how the size p_n(\alpha) of a model can change with n.

## Topics
- Selection procedures:
    - Loo, k-fold averaging, majority-vote, single-split, convex aggregation.
- Settings
    - Linear model selection, nonparametric procedures.

## Literature
- Shao, 1993
- Shao, 1997
    - Two Categories: LOO (similar to GIC-2) and delete-d (similar to GIC-$$\lambda_{n}$$)
    - Thm 1: Assymptotic efficiency of category 1
    - Thm 2: Assimptotic efficiency of category 2
- Yang, 2007
- Bunea, Tsybakov 2007
- Tsybakov, 2009

# Questions
- Why are optimal rates squared?
- Why is the oracle defined as a map?
