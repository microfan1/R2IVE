# R2IVE
Endogenous treatment effect estimation with a large and mixed set of instruments and control variables
The .r files implement the algorithm proposed in Fan and Wu (2022). R2IVE_V1.r implement the first step nonparametric reduced form as in Fan and Wu (2020).  R2IVE_V2.r implement the linear form for both structural and reduced form equations. besttuning.r is a function to choose tuning parameters.
The user's manual (Manual.pdf) shows detailed step-by-step instructions for interested users.
The data.csv is the demonstrative data file to run the above r codes.
For empirical example in Fan and Wu (2022), the main variables can be found in r package naivereg: https://cran.r-project.org/web/packages/naivereg/index.html The extended variables list can be found in the World Bank database.

References

Chen, J., Chen, Z., 2008. Extended bayesian information criteria for model selection with
large model spaces. Biometrika 95 (3), 759-771.

Fan, Q., Wu, Y., 2020. Endogenous treatment effect estimation with some invalid and irrelevant
instruments. arXiv preprint arXiv:2006.14998.

Fan, Q., Wu, Y., 2022. Endogenous treatment effect estimation with a large and mixed set
of instruments and control variables. Review of Economics and Statistics, forthcoming.

Wang, H., Li, B., Leng, C., 2009. Shrinkage tuning parameter selection with a diverging
number of parameters. Journal of the Royal Statistical Society Series B 71, 671-683.

Wang, L., Kim, Y., Li, R., 2013. Calibrating non-convex penalized regression in ultra-high
dimension. Annals of Statistics 41 (5), 2505-2536.
