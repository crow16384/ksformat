library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(stringr)

devtools::load_all()

dm <- tribble(
  ~USUBJID, ~SUBJID, ~RFICDTC,      
"MIRACLE-01001", "01001", "2021-02-09T08:05",
"MIRACLE-01001", "01026", "2024-01-16T08:14",
"MIRACLE-01003", "01025", "2023-08-08T08:20",
"MIRACLE-01003", "01003", "2020-12-11T08:00",
"MIRACLE-01010", "01010", "2021-08-13T07:50",
"MIRACLE-01011", "01011", "2021-08-27T08:35",
"MIRACLE-01012", "01012", "2021-08-15T07:50",
"MIRACLE-01013", "01013", "2021-09-03T08:15",
"MIRACLE-01014", "01014", "2021-11-22T08:01",
"MIRACLE-01015", "01015", "2021-11-11T09:30",
"MIRACLE-01016", "01016", "2021-12-03T09:45",
"MIRACLE-01017", "01017", "2021-11-27T09:30",
"MIRACLE-01018", "01018", "2021-12-03T08:50",
"MIRACLE-01019", "01019", "2021-12-04T08:14",
"MIRACLE-01020", "01020", "2022-03-22T09:01",
"MIRACLE-01021", "01021", "2021-12-24T08:05",
"MIRACLE-01022", "01022", "2021-12-24T08:04",
"MIRACLE-01023", "01023", "2022-01-17T08:02",
"MIRACLE-01024", "01024", "2022-01-24T07:50"
)

# Date lookup (value type — reverse defaults to FALSE)
# setNames(values, keys): names = input keys, values = output Date objects
fmt_date_lookup <- with(
  dm,
  setNames(as.Date(RFICDTC, format="%Y-%m-%d", optional=TRUE), paste(USUBJID, SUBJID, sep = "|"))) 
fmt_date_lookup

fmt_date_lookup %>%
  fnew(ignore_case = TRUE, .other=NA, type="Date", name = "icdtn")

# Character lookup — same setNames(values, keys) pattern with reverse=FALSE
fmt_char_lookup <- with(
  dm,
  setNames(RFICDTC, paste(USUBJID, SUBJID, sep = "|")))
fmt_char_lookup

fmt_char_lookup %>%
  fnew(ignore_case = TRUE, .other='ERROR', type="character", reverse = FALSE, name = "icdtc")

fprint('icdtn')
fprint('icdtc')
