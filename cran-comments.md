## Test environments

* Windows10 x86_64-w64-mingw32 (64-bit), R version 4.2.1, locally.
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Windows Server 2022, R-devel, 64 bit

## 0.1.6: resubmission of 0.1.5

Apart from the correction requested by CRAN, I made some improvements in the documentation test that does not change the functionality since 0.1.5.

Found the following (possibly) invalid file URIs:
    URI: Guidelines for using resource identifiers in Dublin Core metadata and IEEE LOM
-> The http://www.ukoln.ac.uk/metadata/dcmi-ieee/identifiers/index.html is the correct URI for this resource, but I changed the `http` protocol `https`




## On some testing environments I get 1 spelling NOTE

* This is the first release of an experimental package that is aimed to provide a better underpinning for reproducible research packages in the rOpenGov collective in terms of 
documentation and data release on open science repositories or semantic web applications.

* Possibly misspelled words in DESCRIPTION:
    DataCite - this is the spelling of the standard
    DataSet - this is the spelling of the dataset standard of W3c
    datacube - this is the data model of SDMX
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
