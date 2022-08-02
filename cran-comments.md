## Test environments

* Windows10 x86_64-w64-mingw32 (64-bit), R version 4.2.1, locally.
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 1 note

* This is the first release of an experimental package that is aimed to provide a better underpinning for reproducible research packages in the rOpenGov collective in terms of 
documentation and data release on open science repositories or semantic web applications.

* Possibly misspelled words in DESCRIPTION:
    DataCite - this is the spelling of the standard
    DataSet - this is the spelling of the dataset standard of W3c
    eXchange - the official spelling of the Statistical Data and Metadata eXchange
    findability - not in dicationary but a key word of FAIR metadata
    reusability - also FAIR metadata <https://www.go-fair.org/fair-principles/>

* URL: https://sdmx.org/
    From: inst/doc/metadata.html
          inst/doc/motivation.html
          README.md
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: unable to get local issuer certificate
      	(Status without verification: OK)
      	
SDMX is a very important reference point and it would not be a good solution to leave these links out of the vignettes.
