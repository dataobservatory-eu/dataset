
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StatCodelists

<!-- badges: start -->
<!-- badges: end -->

The goal of StatCodelists is to …

## Installation

You can install the development version of StatCodelists like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

Cross-domain concepts in the SDMX framework describe concepts relevant
to many, if not all, statistical domains. SDMX recommends using these
concepts whenever feasible in SDMX structures and messages to promote
the reuse and exchange of statistical information and related metadata
between organisations.

Code lists are predefined sets of terms from which some statistical
coded concepts take their values. SDMX cross-domain code lists are used
to support cross-domain concepts. The use of common code lists will help
users to work even more efficiently, easing the maintenance of and
reducing the need for mapping systems and interfaces delivering data and
metadata to them. Therefore, a choice over code lists has a great impact
on the efficiency of data sharing.

StatCodelists helps the use of the latest codelist in your R workflow.

``` r
library(StatCodelists)
data("codebooks")
codebooks
#>                                                                               concept
#> 1                                                                            Activity
#> 2                                                                                 Age
#> 3                                                             Civil or marital status
#> 4              Classification of Individual Consumption According to Purpose (COICOP)
#> 5                               Classification of the Functions of Government (COFOG)
#> 6              Classification of the Outlays of Producers According to Purpose (COPP)
#> 7  Classification of the Purposes of Non-Profit Institutions Serving Households COPNI
#> 8                                                              Confidentiality status
#> 9                                                                            Currency
#> 10                                                                           Decimals
#> 11                                                             Degree of Urbanisation
#> 12                                                                          Frequency
#> 13                                                                 Geographical areas
#> 14                                                                 Observation status
#> 15                                                                         Occupation
#> 16                                                              Organisation concepts
#> 17                                                                Seasonal adjustment
#> 18                                                                                Sex
#> 19                                                                        Time format
#> 20                                                           Time period – collection
#> 21                                                                    Unit multiplier
#>               codebook authority
#> 1          CL_ACTIVITY      SDMX
#> 2               CL_AGE      SDMX
#> 3      CL_CIVIL_STATUS      SDMX
#> 4            CL_COICOP      SDMX
#> 5             CL_COFOG      SDMX
#> 6              CL_COPP      SDMX
#> 7             CL_COPNI      SDMX
#> 8       CL_CONF_STATUS      SDMX
#> 9          CL_CURRENCY      SDMX
#> 10         CL_DECIMALS      SDMX
#> 11          CL_DEG_URB      SDMX
#> 12             CL_FREQ      SDMX
#> 13             CL_AREA      SDMX
#> 14       CL_OBS_STATUS      SDMX
#> 15       CL_OCCUPATION      SDMX
#> 16     CL_ORGANISATION      SDMX
#> 17  CL_SEASONAL_ADJUST      SDMX
#> 18              CL_SEX      SDMX
#> 19      CL_TIME_FORMAT      SDMX
#> 20 CL_TIME_PER_COLLECT      SDMX
#> 21        CL_UNIT_MULT      SDMX
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
get_codelist(codelist="CL_FREQ", var_name = "FREQ")
#>    FREQ                  name
#> 1     A                Annual
#> 2    A2              Biennial
#> 3    A3             Triennial
#> 4    A4           Quadrennial
#> 5    A5          Quinquennial
#> 6   A10             Decennial
#> 7   A20           Bidecennial
#> 8   A30          Tridecennial
#> 9   A_3    Three times a year
#> 10    S Half-yearly, semester
#> 11    Q             Quarterly
#> 12    M               Monthly
#> 13   M2             Bimonthly
#> 14  M_2           Semimonthly
#> 15  M_3   Three times a month
#> 16    W                Weekly
#> 17   W2              Biweekly
#> 18   W3             Triweekly
#> 19   W4           Four-weekly
#> 20  W_2            Semiweekly
#> 21  W_3    Three times a week
#> 22    D                 Daily
#> 23  D_2           Twice a day
#> 24    H                Hourly
#> 25   H2              Bihourly
#> 26   H3             Trihourly
#> 27    B Daily – business week
#> 28    N              Minutely
#> 29    I             Irregular
#> 30   OA     Occasional annual
#> 31   OM    Occasional monthly
#> 32   _O                 Other
#> 33   _U           Unspecified
#> 34   _Z        Not applicable
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                          description
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                           To be used for data collected or disseminated every year
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                      To be used for data collected or disseminated every two years
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                    To be used for data collected or disseminated every three years
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                     To be used for data collected or disseminated every four years
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                     To be used for data collected or disseminated every five years
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                      To be used for data collected or disseminated every ten years
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                   To be used for data collected or disseminated every twenty years
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                   To be used for data collected or disseminated every thirty years
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                   To be used for data collected or disseminated three times a year
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                      To be used for data collected or disseminated every semester
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                       To be used for data collected or disseminated every quarter
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                         To be used for data collected or disseminated every month
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                    To be used for data collected or disseminated every two months
#> 14                                                                                                                                                                                                                                                                                                                                                                                                                       To be used for data collected or disseminated twice a month
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                 To be used for data collected or disseminated three times a month
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                          To be used for data collected or disseminated every week
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                     To be used for data collected or disseminated every two weeks
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                   To be used for data collected or disseminated every three weeks
#> 19                                                                                                                                                                                                                                                                                                                                                                                                                    To be used for data collected or disseminated every four weeks
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                        To be used for data collected or disseminated twice a week
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                  To be used for data collected or disseminated three times a week
#> 22                                                                                                                                                                                                                                                                                                                                                                                                                           To be used for data collected or disseminated every day
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                         To be used for data collected or disseminated twice a day
#> 24                                                                                                                                                                                                                                                                                                                                                                                                                          To be used for data collected or disseminated every hour
#> 25                                                                                                                                                                                                                                                                                                                                                                                                                     To be used for data collected or disseminated every two hours
#> 26                                                                                                                                                                                                                                                                                                                                                                                                                   To be used for data collected or disseminated every three hours
#> 27                                                                                                                                                                                                                                                                                                    Similar to "daily", however there are no observations for Saturdays and Sundays (so, neither “missing values” nor “numeric values” should be provided for Saturday and Sunday)
#> 28                                                                          While N denotes "minutely", usually, there may be no observations every minute (for several series the frequency is usually "irregular" within a day/days). And though observations may be sparse (not collected or disseminated every minute), missing values do not need to be given for the minutes when no observations exist: in any case the time stamp determines when an observation is observed
#> 29                                                                                                                                                 To be used with irregular time series that stores data for a sequence of arbitrary timepoints. Irregular time series are appropriate when the data arrives unpredictably, such as when the application records every stock trade or when random events are recorded (the interval between each element can be a different length)
#> 30 The event occurs occasionally with an infrequent update that could span from 1 year to several years between events. It implies a survey that experiences a gap for several years prior to the next survey update (this is commonly linked to funding available to run a specific survey (i.e. health surveys), whereas a regular annual survey refers typically to ‘programs’ that are funded regularly and fall under the Statistics Act, and therefore never experience a gap)
#> 31                                                                                                                                                                                                                                                              The event occurs occasionally with an infrequent update that could span from 1 month to several months between events. It implies a survey that experiences a gap for several months prior to the next survey update
#> 32                                                                                                                                                                                                                                               To be used when the qualitative or quantitative values that a variable takes in a data set is associated to multiple occurrences with frequency other than the already defined ones (for example every 5 hours and 32 minutes etc.)
#> 33                                                                                                                                                                              To be used when a set of values are reported within a time range but not associated to sub ranges. Often this could happen in case of missing or sparse information. (Let’s say we have two observations for 2020 but we do not know if they are part of a monthly reporting or quarterly reporting)
#> 34                                                                                                                                                                                                                                                                              To be used when the qualitative or quantitative values that a variable takes in a data set is not associated to multiple occurrences (only single occurrence exists) one can use the _Z as frequency
#>    name_locale description_locale
#> 1           en                 en
#> 2           en                 en
#> 3           en                 en
#> 4           en                 en
#> 5           en                 en
#> 6           en                 en
#> 7           en                 en
#> 8           en                 en
#> 9           en                 en
#> 10          en                 en
#> 11          en                 en
#> 12          en                 en
#> 13          en                 en
#> 14          en                 en
#> 15          en                 en
#> 16          en                 en
#> 17          en                 en
#> 18          en                 en
#> 19          en                 en
#> 20          en                 en
#> 21          en                 en
#> 22          en                 en
#> 23          en                 en
#> 24          en                 en
#> 25          en                 en
#> 26          en                 en
#> 27          en                 en
#> 28          en                 en
#> 29          en                 en
#> 30          en                 en
#> 31          en                 en
#> 32          en                 en
#> 33          en                 en
#> 34          en                 en
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

## Code of Conduct

Please note that the StatCodelists project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
