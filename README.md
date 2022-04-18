# R2IVE
Endogenous treatment effect estimation with a large and mixed set of instruments and control variables
The .r files implement the algorithm proposed in Fan and Wu (2020). R2IVE_V1.r implement the first step nonparametric reduced form.  R2IVE_V2.r implement the linear form for both structural and reduced form equations. besttuning.r is a function to choose tuning parameters.
The user's manual (Manual.pdf) shows detailed step-by-step instructions for interested users.
The data.csv is the demonstrative data file to run the above r codes.
For empirical example in Fan and Wu (2020), the main variables can be found in r package naivereg: https://cran.r-project.org/web/packages/naivereg/index.html The extended variables list can be found in the World Bank database.
