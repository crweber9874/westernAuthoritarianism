# Western Authoritarianism

The project relies on a lot of repeatedly called functions. I created a simple R package to house these functions. The package is called westernAuthoritarianism and is available on my github for the project. The package is not on CRAN, but can be installed from github using devtools. Alternatively, the package can be installed locally from the repository.

```         
## From github
devtools::install_github("crweber9874/westernAuthoritarianism")

## or locally, 
devtools::load_all()
```

# Data Structure

There are three unalterable datasets located in GCP, called grau_data (group-authoritarianism/data). They are prefixed with an r, noting "raw." These datasets are:

-   The "Western Data" (rWestern.dta)
-   The Wave I AVP data
-   The Wave II AVP data

These are relatively large, and should not be altered. The data are just pulled, cleaned, and repushed to the same location. The cleaned data are prefixed with a c, noting "cleaned," whereas the original data are always prefixed with r, noting "raw." The files can be downloaded from the google bucket by obtaining a json-api key from the project owner. GCP is also used periodically to write large files, such as the output of the latent variable analysis, and the output of the regression models. Throughout the pipeline, I also dump temporary files into the "tmp" folder.

## Environment

-   Renv(). To access the data, user must be a collaborator in project repository. Place the json-api key as a path in local Renv

-   renv.lock includes system build information and package dependencies.

-   cleanDirectory.R: A script to clean up the directory.

## Package Structure (\~/R)

The package is structured as follows:

1)  Analytic functions. brmsOrdinal.R, brmsLinear.R, brmsLogit.R, and brmsNominal.R are functions to run ordinal, linear, logistic, and nominal regression models, respectively. These functions are wrappers for the brms package, and are used to run the regression models in the project. The functions are called in modelsRegression.R. There is also a file called latent.r which runs the latent variable models

2)  Prediction functions. These are prefaced with posteriorMarginal.R and posteriorMeans.R and are used to make predictions from the brms models. The predictions are used to create the figures in the paper.

3)  Graphical and Misc functions; zeroOne.R, myTheme.R, uacolorScheme.R are used to create the figures in the paper.

## Analytic Files

1)  latent.R. This file runs the latent variable models used in the paper.The output of this file is dataActive.csv which is the merged file of the cleaned data and the posterior estimates of the latent variable model.

2)  regression.R. This file includes the regression models used in the paper.

3)  groupComparisons.Rmd. This file examines the charactericts of the latent variable measurement model specification

4)  regressionFigures.Rmd. This file creates the figures used in the paper. The file includes the effects of authoritariansm X latino interactions on issues - immigration, environment, guns.

5)  Data cleaning functions. The data cleaning functions are located in recode.r. They are used to clean the data, structure variables, and prepare the data for analysis. The output of recode.r is cWestern.rda, which is the cleaned Western States YouGov Survey data.
