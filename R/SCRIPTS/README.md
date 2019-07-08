# Organization

This folder contains all scripts necessary to replicate these results.

## Scripts: Analysis and Data

`/000-Libraries.R` - is a simple R script containing the libraries used in all of the subsequent scripts.

`/001-LoadCleanHFDHMDData.R` - will import and format the Human Fertility Database and Human Mortality Database files.

`/002-BayesAnalysis_sample.R` - contains the script necessary to produce the bTFR estimates on the data. Note, this script will take a very long time to run, even while parallelized. This script is presently commented out and written to produce a small subset of estimates to demonstrate the script actually works.

`/003-HMFDHFAnalysis_ipums.R` - contains the script necessary to generate all of the evaluation results using the HFD/HMD and DHS data.

`/005-NonhumanDataLoad.R` - will download, wrangle, and analyze the non-human data.

`/006-IPUMS_bayes.R` - creates the bTFR estimates using the DHS data.

`/007-SAnalysis.R` - is a short, simple script to demonstrate the accuracy of our approximation of s using q(5) data. This is based on 266 years of Swedish mortality data.

`/007-projections_2100.R` - contains the script necessary to generate all of the data in `PROJECTIONS/PROJECTIONS/`. This script will generate the population projections for the period 2015-2100.

`/0001-constants.R` - contains a very small script with the constants for q(5) and s.

## Scripts: Figures/Tables from the Manuscript
`/101-Figure1.R` - will create the primary figure for the manuscript. This is the multi panel, colored figure evaluating the HMD/HFD and DHS data for the iTFR, xTFR, and bTFR methods.

`/102-Figure2.R` - will create the non-human scatterplot in the manuscript.

`/103-figure3a_xTFRAfrica.R` - will download the data required to create the sub-national map of African fertility. Most of this script is commented out to increase processing time. Rendering the raster can take a while.

`/104-figure3b_historicbTFR.R` - will create the figure of the historic TFRs for Sweden, France, and the Netherlands.

`/105-figure3c_incomexTFR.R` - will create the figure of TFR by household income using American Community Survey data.

`/106-figure3_combined.R` - the Africa TFR map, historic TFR figure, and TFR by HH income were a single figure at one time. This script creates that single figure.

`/107-suppfig1_uscountyxTFR.R` - will import, process, evaluate, and map the errors associated with the subnational TFR estimates for US counties. This was a supplementary figure at one time.

`/108-suppfig2.R` - was a figure showing the relationship between the multiplier (ie 7 * C/W) and the fraction of women aged 25-34. It is no longer in the main document but is here for posterity.

`/109-suppfig3.R` - will create the multi-panel colored figure showing the error rates by infant mortality for all five variants. This was a supplementary figure at one point but is now included in the main document.

`/110-comparisontable.R` - will create the comparison figure for the three primary datasets (HFD/HMD, DHS, US counties) for all five variants. Note, this also includes a supplementary analysis using Rele's method that we cut from the manuscript. If you're interested in how well our methods perform compared to another indirect method, this script will do that.

# Scripts: Additional Helper Scripts

`/bayesTFR_function.R` - This script creates the bayesTFR function which will estimate the iTFR, xTFR, iTFR+, xTFR+, and bTFR for any given age-structure input and an associated q5 mortality. Note: this script requires STAN to properly run.

`/fast_bTFR function.R` - This script runs considerably faster than `bayesTFR_function.R` and does not require STAN to estimate. Will only produce bTFR estimates.

`/superfast_bTFR function.R` - This script runs even faster than `bayesTFR_function.R` and the `fast_bTFR function.R` and does not require STAN to estimate. Will only produce bTFR estimates.


# Correspondence
For any issues with the functionality of these scripts please create an issue.

## License
The data collected and presented is licensed under the [Creative Commons Attribution 3.0 license](http://creativecommons.org/licenses/by/3.0/us/deed.en_US), and the underlying code used to format, analyze and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
