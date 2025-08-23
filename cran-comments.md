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

## Resubmission

This resubmission addresses the following URL check notes:

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
    From: inst/doc/rdf.html
    Status: 404
    Message: Not Found

The domain `http://example.com/` is reserved by the IETF/W3C for use in
documentation and examples. These IRIs were included in the vignette to
demonstrate RDF/Linked Data usage, and are valid by design even though they do
not resolve.

However, to facilitate CRAN publication and avoid repeated URL check notes, I
replaced these with example URIs hosted on the package website that do return
HTTP 200 and are published in both HTML and Turtle format. This removes the 404
check while retaining the intended illustration of example IRIs.


