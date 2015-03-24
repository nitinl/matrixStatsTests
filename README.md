# matrixStatsTests
#### Skill Tests for R package matrixStats

##### Repository contains:
1. Build and Check results for master and develop branches of matrixStats repository
2. R implementation for row mins that takes cols as argument. rowMins_R.R
3. C implementation for row mins using .Call. (rowmins.R, rowmins.c)
4. Tests for rowMins_R() and rowMins_C() (including checks for out of range subscript, wrong argument types, non numeric input.) tests.R
5. Verifying rowMins_R() and rowMins_C() work for set of column groups (included in tests.R)
6. Simple performance comparison of rowMins_R() and rowMins_C() based on time of execution (included in tests.R)
7. Weighted var C implementation in weightedVar/ (weightedvar.R, weightedvar.c)
8. Tests showing side by side comparison of weightedVar implemented from package matrixStats and C implementation. (weightedVar/weightedVarTest.R)
9. Output of test on weightedVar() (weightedVar/weightedVarOutput.txt)

**Note:** 
Build rowmins.c and weightedvar.c using R CMD SHLIB rowmins.c or R CMD SHLIB weightedvar.c.
Please change path of .so file generated thus within rowmins.R and weightedvar.R before testing.
