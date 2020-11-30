---
title: "nettskjemar - An R-package for API connection to the University of Oslo's Nettskjema survey tool"
tags:
  - r
  - API
authors:
  - name: Athanasia M. Mowinckel
    orcid: 0000-0002-5756-0223
    affiliation: "1" 
affiliations:
 - name: "Center for Lifespan Changes in Brain and Cognition, University of Oslo, PO. box 1094 Blindern, 0317 Oslo, Norway"
   index: 1
citation_author: Mowinckel
date: 13 October 2020
year: 2020
output:
  rticles::joss_article:
    keep_md: true
bibliography: references.bib
csl: apa.csl
journal: JOSS
---

# Summary
Nettskjema is an online survey tool created by the University of Oslo, used by academic and research institutions across all of Norway. 
The tool is used for a wide variety of purposes, ranging from conference sign-ups, quizzes, and research surveys [@universityofosloitShortIntroductionNettskjema]. 
The data is stored on servers compliant with national and European GDPR specifications [@radley-gardnerREGULATIONEU20162016].
The online portal for creating surveys also has simple features for downloading data in text, excel, or SPSS formats, as well as downloading codebooks. 
However, during a project period, data is usually downloaded in increments to ensure data quality, and the online portal has no functionality to download data after a specific date. 
This forces researchers to download the complete dataset every time. 
Furthermore, most projects would also have multiple surveys going at the same time, and manually clicking into each survey to download data is cumbersome and error prone. 
The online portal, lastly, cannot download data when the number of survey responses exceeds a certain size. 
This forces the researchers with surveys exceeding approximately 2000 responses to find other ways to access their data. 

Together with the online portal, there is an API which can be accessed using CURL [@NettskjemaAPI]. 
This API has several features that are not accessible through the online portal, like incremental downloads and more extensive data and meta-data information. 
By using the API to access survey data and meta-data, researchers have the opportunity of acquiring information about their survey they otherwise could not, which also enables more thorough documentation of the entire survey for posterity. 
Thereby, a tool that provides easy access through the API would enable researchers to adopt more transparent workflows using Nettskjema.

The `nettskjemar` package for the R statistical software [@R], which is primarily created for researchers to download data and meta-data from their online surveys hosted with the Nettskjema survey tool. 
In a series of functions to set up API tokens and API user creation, the users are guided through the process of setting up a system where they can access their surveys through R. 
The [online package tutorials](https://lcbc-uio.github.io/nettskjemar/index.html) (and package vignettes) provides detailed instructions on the use of the package.
After user creation and successful API token retrieval, the user can access the survey data and meta-data both in tidied data formats, or as raw json content within R. 

The package enables Nettskjema R-users the possibility to retrieve their survey data, meta-data and codebooks directly into their working environments, and create fully documented workflows. 


# Funding
This tool is funded by the EU Horizon 2020 Grant "Healthy minds 0-100 years: Optimising the use of European brain imaging cohorts (Lifebrain)" (732592), under the societal challenges call "Health, demographic change and well-being". 

# References
