---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: "nettskjemar - An R-package for API connection to the University of Oslo's Nettskjema survey tool"
tags:
  - r
  - API
authors:
  - name: Athanasia M. Mowinckel
    orcid: 0000-0002-5756-0223
    affiliation: "1" # (Multiple affiliations must be quoted)
affiliations:
 - name: "Center for Lifespan Changes in Brain and Cognition, University of Oslo, PO. box 1094 Blindern, 0317 Oslo, Norway"
   index: 1
citation_author: Mowinckel
date: 13 October 2020
year: 2020
output: rticles::joss_article
bibliography: paper.bib
csl: apa.csl
journal: JOSS
---

# Summary
Nettskjema is an online survey tool created by the University of Oslo, used by academic and research institutions across all of Norway. 
The tool is used for a wide variety of purposes, ranging from conference sign-ups, quizzes, and research surveys. 
In addition to storing data on servers with compliant with national GDPR specifications, data can also be automatically encrypted and stored on the University of Oslo's Services for Secure Data storage which can store sensitive data securely (REF).
The online portal for creating surveys also has simple features for downloading data in text, excel or SPSS formats, as well as downloading codebooks. 
However, during a project period, data is usually downloaded in increments to ensure data quality, and most projects would also have multiple surveys going at the same time. 
The online portal has not functionality to download only data received after a certain time-stamp, and as such the entire dataset must be downloaded again.
Accessing survey data and meta-data manually through the portal is also time-consuming when there are many surveys in question.
Furthermore, the online portal cannot download data when the number of survey responses exceeds a certain size, as it is not built to scale to large surveys with many responses (typically around 2000 responses, in my experience). 

Together with the online portal, there is an API which can be accessed using CURL (REF). 
This API has several features that are not accessible through the online portal, like incremental downloads and more extensive data and meta-data information. 
By using the API to access survey data and meta-data, researchers have the opportunity of acquiring information about their survey they otherwise could not, which also enables more thorough documentation of the entire survey for posterity. 
Thereby, a tool that provides easy access through the API would enable researchers to adopt more transparent workflows using Nettskjema.

The nettskjemar R-package is primarily created for researchers to download data and meta-data from their online surveys hosted with the Nettskjema survey tool. 
In a series of functions to set up API tokens and API user creation, the users are guided through the process of setting up a system where they can access their surveys through R. 
The [online package tutorials](https://lcbc-uio.github.io/nettskjemar/index.html) (and package vignettes) provides detailed instructions on the use of the package.
After user creation and successful API token retrieval, the user can access the survey data and meta-data both in tidied data formats, or as raw json content within R. 



# Acknowledgements



# References
