## Test environments

Local:
* Windows10 x86_64-w64-mingw32 (64-bit), R version 4.5.0, locally.

r_hub:
* latest release: macos, linux, windows (Windows Server 2022)
* devel: atlas (Fedora Linux 38)
* R-oldrelease win-builder.r-project.org

rOpenSci:
* ubuntu-latest with stricter checks.

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Comment:
This release contains many improvements for the methods of the s3 classes as 
well as their documentation, based on reviewer comments from rOpenSci. It also 
corrects a failing test marked by CRAN due to the change of how utils::bibentry()
handles institutional legal person names.

There are false negative errors on some platforms:

Found the following (possibly) invalid URLs:
  URL: http://example.com/dataset#eg:1
    From: inst/doc/rdf.html
    Status: 404
    Message: Not Found
  URL: http://example.com/dataset#eg:2
    From: inst/doc/rdf.html
    Status: 404
    Message: Not Found
  URL: http://example.com/dataset#eg:3
  
The domain `http://example.com/` is retained by the World Wide Web Consortium
for applications to use a valid but non-existing URL for demonstrating the 
use of URLs, or in this case, URIs. As the package aims for semantic web
interoperability, these standard URLs should be retained. The 
`http://example.com/dataset#eg:3` is not used in the vignette as an URL, 
but as an URI and only in textual examples

