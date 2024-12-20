## Test environments

Local:
* Windows10 x86_64-w64-mingw32 (64-bit), R version 4.4.0, locally.

r_hub:
* macos (latest release)
* macos-arm64
* windows-latest (release)
* windows x86_64-w64-mingw32 with R version 4.3.3 (2024-02-29 ucrt)
* ubuntu-latest (release) - ubuntu-latest pipelines will use ubuntu-24.04 soon. 
* ubuntu-latest (oldrel)  - ubuntu-latest pipelines will use ubuntu-24.04 soon.
* ubuntu-latest (devel)
* atlas

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Comment:
The 0.3.1 version of this package was on CRAN but archived due to a documentation problem.
- From the previous submission resolved issues:
  Version contains large components (0.3.3009) -> 0.3.4

- Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
  Missing Rd-tags:
     var_label.Rd: \value
  Value tags were very thoroughly checked, and hopefully not only there is no missing but the
  wording is more meaningful when objects are invisibly returned (because only attributes are 
  changed)
     
- Some code lines in examples are commented out. Please never do that. Ideally find toy examples that can be regularly executed and checked. Lengthy examples (> 5 sec), can be wrapped in \donttest{}.
  Examples in comments in:
      xsd_convert.Rd
  This was a copying mistake and did not find any further instances, this is corrected, of course.

- \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please replace \dontrun with \donttest.
   Examples within \dontrun in var_unit.Rd
   This was also unnecessary and there are no \dontrun{} tags anymore.

