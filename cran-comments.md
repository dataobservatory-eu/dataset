## Test environments

* Windows10 x86_64-w64-mingw32 (64-bit), R version 4.2.1, locally.
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Windows Server 2022, R-devel, 64 bit

## 0.1.8: resubmission of 0.1.7

This is the third resubmission
* Please always write package names, software names and API (application
programming interface) names in single quotes in title and description.
e.g: --> 'dataset'; 'R';... - > corrected

* Missing Rd-tags '\value': attributes_measures.Rd ... etc -> added

* Some minor improvements in presentation, documentation and unit testing from [ropensci/software-review#553](https://github.com/ropensci/software-review/issues/553#issuecomment-1244076662)

## On some testing environments I get 1 spelling NOTE

The spelling is rather thoroughly checked.  There are many false positives that are domain specific abbreviation which look common words.

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
