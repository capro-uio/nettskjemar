## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)
* R-CMD-CHECK on GitHub Actions
  - {os: macOS-latest,   r: 'release'}
  - {os: windows-latest, r: 'release'}
  - {os: windows-latest, r: '3.6'}
  - {os: ubuntu-18.04,   r: 'release', rspm: "https://packagemanager.rstudio.com/cran/__linux__/bionic/latest"}
  - {os: ubuntu-18.04,   r: '3.4',     rspm: "https://packagemanager.rstudio.com/cran/__linux__/bionic/latest"}
  - {os: ubuntu-18.04,   r: '3.3',     rspm: "https://packagemanager.rstudio.com/cran/__linux__/bionic/latest"}
  - {os: ubuntu-20.04,   r: 'release', rspm: "https://packagemanager.rstudio.com/cran/__linux__/focal/latest"}
  - {os: ubuntu-20.04,   r: 'devel',   rspm: "https://packagemanager.rstudio.com/cran/__linux__/focal/latest"}

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
