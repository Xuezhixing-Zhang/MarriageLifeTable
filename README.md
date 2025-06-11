# Notes for Life Table Analysis

### Step 1: Fit the Bayesian Multi-state logistic regression

Install the `bayesmlogit` package and run `Step1.R`. In the analysis, we run the regression two times simultaneously to obtain 1000 Bayesian samples. 

The codes will generate two output files for each analysis, e.g.., `liv_out1.rds` and `liv_out2.rds` for living arrangement analysis.

### Step 2: Run the Life Table Analysis

Run `Step2.R`. The codes will generate life tables for each subgroup.

### Step 3: Summarize Results

Run codes in the folder `Step 3`. `Process_general` reports the summarized life expectancies for each state of all subgroups. `Process_Disparity` reports the life expectancy differences for each subgroup, compared to whites.  `Process_proportion` reports the life expectancy proportions for each state of all subgroups.

