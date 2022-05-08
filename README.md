# 2022_group6_final_project
This script-bundle has been created to perform exploratory data analysis on
the dataset produced by Zhou et. al 2014 in relation to exploration of mtDNA's
uses as a biomarker for prostate cancer.

R/00_doit.R runs all scripts in sequence and produces a .html slideshow with 
the most important visual results of the EDA.

The scripts prefixed with 01, 02 and 03 loads the excel data-set publicized by
Zhou et. al, cleans and augments it for further analysis.
04 outputs a series of boxplots which tests for outlying data.
05 performs Principal Component Analysis on the data, and 06 performs logistic
regression to test for correlation between variables.
07 explores the relation between participants BMI and dietary habits.
08 emulates the visualization used in the article, and produces an argueably
better version.
09 explores the role of mtDNA as a biomarker for tumor severity.
10 shows the role of PSA as a biomarker for the presence of prostate cancer.
11 was an attempt at EDA which led to a dead end.
12 attempts to describe the correlation between mtDNA levels and subject age.

The main output of these scripts is a set of data visualizations, which are
saved to the 'results'-directory. The 'data'-directory contains the raw data
publicized by Zhou et. al.
The 'docs'-directory contains various project notes, along with the slideshow
which is the de facto output of this project.
