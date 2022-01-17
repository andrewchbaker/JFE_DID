# JFE_DID

This repo contains the code to replicate the analyses in Baker, Larcker, Wang, "How Much Should We Trust Staggered Difference-in-Difference Estimates", forthcoming in the **Journal of Financial Economics**. The code here replicates the analyses in the January 2022 version of the paper. For those looking for replication files for a prior version (dated March 2021), these can be found at https://github.com/andrewchbaker/DiD_Codes.

Note that I use the package `renv` to save a snapshot of the versions of the software that are used in all of the codes. This should make the results fully replicable. To the extent you have any questions try reaching out to Grant McDermott, which is what most of us do when we have any kind of coding/version control questions.

The structure of the repo (codes in the `Code` folder) is as follows:

- **1a. Simulations - Save Data.R**: this code will pull the data from Compustat, make and winsorize the ROA variable for the simulations, and save a file called simulations_data.rds in a Data folder. Note that it requires a WRDS password and username, which I source from a separate (not included file). You will need to fill in your own username and password. 

- **1b. TWFE + binary DiD**: This code runs the Monte Carlo simulation as described in the Section 3.1.1. of the paper. It also produces the plots for Figures 1 and 2. 

- **1c. Alternative Estimators**: This code takes the simulation results from 1b and makes Figure 6, which provides the static DiD results for the alternative event study estimators. 

- **1d. Goodman-Bacon Decomp.R**: This code produces the Goodman-Bacon decomposition results which feed into Figure 3. 

- **1e. Alternative Event Study Estimators.R**: This code produces the event study estimates using the alternative DiD estimators, as reported in Figure 7.

- **1f. Rejections.R** - This code produces the results in Figure 4 showing TWFE rejection rates when the true expected ATT is 0. 

- **1g. Failures with Event Studies and Heterogeneity.R**: This code produces the results in Figure 5, showing how standard TWFE estimates (as commonly implemented) can both fail to pick up true pre-trends in the data, or falsely indicate the presence of pre-trends when they don't exist.

- **2. BLL.R**: This code produces the figures and tables for the replication and extension of <span style="color:blue"> *Beck, Levine, and Levkov (2010)* text</span>. Note that the replication data for this paper can be found at https://dataverse.nl/dataset.xhtml?persistentId=hdl:10411/15996.

- **3. FHLT.R**: This code produces the figures and tables for <span style="color:blue">some *Fauver, Hung, Li, and Taboada (2017)* text</span>. Unfortunately the data for this analysis was provided by the authors and cannot be uploaded here. 
