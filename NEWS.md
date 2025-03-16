# nettskjemar 1.0.0

- Port to Nettskjema API v3
- improved function names, with shorter `ns` prefix
- Add support for Haven labelled data
- Improved vignettes
- Adds tests


# nettskjemar 0.1.4.008

- add functions to download attachments
- binaries returned as integer rather than double
- added general usage vignette

# nettskjemar 0.1.4.004

- add linear scale support  
- fix bugs in codebook and meta data that did not retrieve question data  
- force writing raw codebook and raw metadata to json extension  
- add direct check for if a form has codebook `has_codebook()`

# nettskjemar 0.1.4

* simplification of internal code  
* added function to find currently IP address   
* moved CI to github actions from travis (inconsequential for package use)  

# nettskjemar 0.1.03

* added JOSS submission paper
* added options for checkbox answers

# nettskjemar 0.1.02

* codebooks improved  
* Bug fix: all information from checkbox elements now returned
* automatic incremental download of data happens now at >2000 rather than >1000 responses  

# nettskjemar 0.1.01

* Added a `NEWS.md` file to track changes to the package  
* Codebooks can now be retrieved  
* new meta-data class added  
* Can now add additional columns to data based on meta-data and codebook information  
