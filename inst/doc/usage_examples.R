## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ksformat)

## ----discrete-----------------------------------------------------------------
fnew(
  "M" = "Male",
  "F" = "Female",
  .missing = "Unknown",
  .other = "Other Gender",
  name = "sex"
)

gender_codes <- c("M", "F", "M", NA, "X", "F")
formatted_genders <- fput(gender_codes, "sex")

data.frame(
  code = gender_codes,
  label = formatted_genders
)

fprint("sex")

## ----ranges-------------------------------------------------------------------
fparse(text = '
VALUE age (numeric)
  [0, 18)     = "Child"
  [18, 65)    = "Adult"
  [65, HIGH]  = "Senior"
  .missing    = "Age Unknown"
;
')

ages <- c(5, 15.3, 17.9, 18, 45, 64.99, 65, 85, NA)
age_groups <- fputn(ages, "age")

data.frame(
  age = ages,
  group = age_groups
)

## ----bmi----------------------------------------------------------------------
fparse(text = '
VALUE bmi (numeric)
  [0, 18.5)    = "Underweight"
  [18.5, 25)   = "Normal"
  [25, 30)     = "Overweight"
  [30, HIGH]   = "Obese"
  .missing     = "No data"
;
')

bmi_values <- c(16.2, 18.5, 22.7, 25, 29.9, 35.1, NA)
bmi_labels <- fputn(bmi_values, "bmi")

data.frame(
  bmi = bmi_values,
  category = bmi_labels
)

## ----bounds-------------------------------------------------------------------
fparse(text = '
VALUE score (numeric)
  (0, 50]    = "Low"
  (50, 100]  = "High"
  .other     = "Out of range"
;
')

scores <- c(0, 1, 50, 51, 100, 101)
score_labels <- fputn(scores, "score")

data.frame(
  score = scores,
  label = score_labels
)

## ----invalue------------------------------------------------------------------
finput(
  "Male" = 1,
  "Female" = 2,
  name = "sex_inv"
)

labels <- c("Male", "Female", "Male", "Unknown", "Female")
codes <- finputn(labels, "sex_inv")

data.frame(
  label = labels,
  code = codes
)

## ----bidirectional------------------------------------------------------------
status_bi <- fnew_bid(
  "A" = "Active",
  "I" = "Inactive",
  "P" = "Pending",
  name = "status"
)

# Forward: code -> label
status_codes <- c("A", "I", "P", "A")
status_labels <- fputc(status_codes, "status")
data.frame(code = status_codes, label = status_labels)

# Reverse: label -> code
test_labels <- c("Active", "Pending", "Inactive")
test_codes <- finputc(test_labels, "status_inv")
data.frame(label = test_labels, code = test_codes)

## ----multiparse---------------------------------------------------------------
fparse(text = '
// Study format definitions

VALUE race (character)
  "W" = "White"
  "B" = "Black"
  "A" = "Asian"
  .missing = "Unknown"
;

INVALUE race_inv
  "White" = 1
  "Black" = 2
  "Asian" = 3
;
')

fprint()

## ----export-------------------------------------------------------------------
bmi_fmt <- format_get("bmi")
cat(fexport(bmi = bmi_fmt))

## ----sas-put-input------------------------------------------------------------
# fputn — apply numeric format by name
fputn(c(5, 30, 70), "age")

# fputc — apply character format by name
fputc(c("M", "F"), "sex")

# finputn — apply numeric invalue by name
finputn(c("White", "Black"), "race_inv")

## ----df-format----------------------------------------------------------------
df <- data.frame(
  id = 1:6,
  sex = c("M", "F", "M", "F", NA, "X"),
  age = c(15, 25, 45, 70, 35, NA),
  stringsAsFactors = FALSE
)

sex_f <- format_get("sex")
age_f <- format_get("age")

df_formatted <- fput_df(
  df,
  sex = sex_f,
  age = age_f,
  suffix = "_label"
)

df_formatted

## ----missing------------------------------------------------------------------
# With .missing label
fput(c("M", "F", NA), "sex")

# With keep_na = TRUE
fput(c("M", "F", NA), sex_f, keep_na = TRUE)

# is_missing() checks
is_missing(NA)
is_missing(NaN)
is_missing("")   # TRUE — empty strings are treated as missing

## ----date-formats-------------------------------------------------------------
today <- Sys.Date()

data.frame(
  format = c("DATE9.", "MMDDYY10.", "DDMMYY10.", "YYMMDD10.",
             "MONYY7.", "WORDDATE.", "YEAR4.", "QTR."),
  result = c(
    fputn(today, "DATE9."),
    fputn(today, "MMDDYY10."),
    fputn(today, "DDMMYY10."),
    fputn(today, "YYMMDD10."),
    fputn(today, "MONYY7."),
    fputn(today, "WORDDATE."),
    fputn(today, "YEAR4."),
    fputn(today, "QTR.")
  )
)

# Multiple dates
dates <- as.Date(c("2020-01-15", "2020-06-30", "2020-12-25"))
fputn(dates, "DATE9.")

## ----date-numeric-------------------------------------------------------------
r_days <- as.numeric(as.Date("2025-01-01"))
r_days
fputn(r_days, "DATE9.")
fputn(r_days, "MMDDYY10.")

## ----time-formats-------------------------------------------------------------
seconds <- c(0, 3600, 45000, 86399)

data.frame(
  seconds = seconds,
  TIME8 = fputn(seconds, "TIME8."),
  TIME5 = fputn(seconds, "TIME5."),
  HHMM = fputn(seconds, "HHMM.")
)

## ----datetime-formats---------------------------------------------------------
now <- Sys.time()

data.frame(
  format = c("DATETIME20.", "DATETIME13.", "DTDATE.", "DTYYMMDD."),
  result = c(
    fputn(now, "DATETIME20."),
    fputn(now, "DATETIME13."),
    fputn(now, "DTDATE."),
    fputn(now, "DTYYMMDD.")
  )
)

# From numeric R-epoch seconds
r_secs <- as.numeric(as.POSIXct("2025-06-15 14:30:00", tz = "UTC"))
fputn(r_secs, "DATETIME20.")

## ----fnew-date----------------------------------------------------------------
# SAS-named format
fnew_date("DATE9.", name = "bday_fmt")
birthdays <- as.Date(c("1990-03-25", "1985-11-03", "2000-07-14"))
fput(birthdays, "bday_fmt")

# Custom strftime pattern (e.g. DD.MM.YYYY)
fnew_date("%d.%m.%Y", name = "ru_date", type = "date")
fput(birthdays, "ru_date")

# Custom pattern with missing label
fnew_date("MMDDYY10.", name = "us_date", .missing = "NO DATE")
mixed <- c(as.Date("2025-01-01"), NA, as.Date("2025-12-31"))
fput(mixed, "us_date")

fprint("bday_fmt")

## ----date-df------------------------------------------------------------------
patients <- data.frame(
  id = 1:4,
  visit_date = as.Date(c("2025-01-10", "2025-02-15", "2025-03-20", NA)),
  stringsAsFactors = FALSE
)

visit_fmt <- fnew_date("DATE9.", name = "visit_fmt", .missing = "NOT RECORDED")
fput_df(patients, visit_date = visit_fmt)

## ----date-parse---------------------------------------------------------------
fparse(text = '
VALUE enrldt (date)
  pattern = "DATE9."
  .missing = "Not Enrolled"
;

VALUE visit_time (time)
  pattern = "TIME8."
;

VALUE stamp (datetime)
  pattern = "DATETIME20."
;
')

fput(as.Date("2025-03-01"), "enrldt")
fput(36000, "visit_time")
fput(as.POSIXct("2025-03-01 10:00:00", tz = "UTC"), "stamp")

# Export back to text
enrl_obj <- format_get("enrldt")
cat(fexport(enrldt = enrl_obj))

fclear()

## ----multilabel-basic---------------------------------------------------------
fnew(
  "0,5,TRUE,TRUE"    = "Infant",
  "6,11,TRUE,TRUE"   = "Child",
  "12,17,TRUE,TRUE"  = "Adolescent",
  "0,17,TRUE,TRUE"   = "Pediatric",
  "18,64,TRUE,TRUE"  = "Adult",
  "65,Inf,TRUE,TRUE" = "Elderly",
  "18,Inf,TRUE,TRUE" = "Non-Pediatric",
  name = "age_categories",
  type = "numeric",
  multilabel = TRUE
)

ages <- c(3, 14, 25, 70)

# fput returns first match only
fput(ages, "age_categories")

# fput_all returns ALL matching labels
all_labels <- fput_all(ages, "age_categories")
for (i in seq_along(ages)) {
  cat("Age", ages[i], "->", paste(all_labels[[i]], collapse = ", "), "\n")
}

## ----multilabel-missing-------------------------------------------------------
fnew(
  "0,100,TRUE,TRUE"   = "Valid Score",
  "0,49,TRUE,TRUE"    = "Below Average",
  "50,100,TRUE,TRUE"  = "Above Average",
  "90,100,TRUE,TRUE"  = "Excellent",
  .missing = "No Score",
  .other = "Out of Range",
  name = "score_ml",
  type = "numeric",
  multilabel = TRUE
)

scores <- c(95, 45, NA, 150)
ml_result <- fput_all(scores, "score_ml")

for (i in seq_along(scores)) {
  cat("Score", ifelse(is.na(scores[i]), "NA", scores[i]),
      "->", paste(ml_result[[i]], collapse = ", "), "\n")
}

## ----multilabel-parse---------------------------------------------------------
fparse(text = '
VALUE risk (numeric, multilabel)
  [0, 3]   = "Low Risk"
  [0, 7]   = "Monitored"
  (3, 7]   = "Medium Risk"
  (7, 10]  = "High Risk"
;
')

risk_scores <- c(2, 5, 9)
risk_labels <- fput_all(risk_scores, "risk")
for (i in seq_along(risk_scores)) {
  cat("Score", risk_scores[i], "->",
      paste(risk_labels[[i]], collapse = " | "), "\n")
}

## ----multilabel-export--------------------------------------------------------
risk_obj <- format_get("risk")
cat(fexport(risk = risk_obj))

fprint("risk")

## ----ae-grading---------------------------------------------------------------
fnew(
  "1,1,TRUE,TRUE" = "Mild",
  "2,2,TRUE,TRUE" = "Moderate",
  "3,3,TRUE,TRUE" = "Severe",
  "4,4,TRUE,TRUE" = "Life-threatening",
  "5,5,TRUE,TRUE" = "Fatal",
  "3,5,TRUE,TRUE" = "Serious",
  "1,2,TRUE,TRUE" = "Non-serious",
  name = "ae_grade",
  type = "numeric",
  multilabel = TRUE
)

grades <- c(1, 2, 3, 4, 5)
ae_labels <- fput_all(grades, "ae_grade")
for (i in seq_along(grades)) {
  cat("Grade", grades[i], ":",
      paste(ae_labels[[i]], collapse = " + "), "\n")
}

fclear()

## ----nocase-------------------------------------------------------------------
sex_nc <- fnew(
  "M" = "Male",
  "F" = "Female",
  .missing = "Unknown",
  name = "sex_nc",
  type = "character",
  ignore_case = TRUE
)

input <- c("m", "F", "M", "f", NA)
fput(input, sex_nc)

# Note the [nocase] flag
fprint("sex_nc")

# Also works with fputc
fputc("m", "sex_nc")

fclear()

## ----expr-sprintf-------------------------------------------------------------
stat_fmt <- fnew(
  "n"   = "sprintf('%s', .x1)",
  "pct" = "sprintf('%.1f%%', .x1 * 100)",
  name = "stat",
  type = "character"
)

types  <- c("n",  "pct",  "n",   "pct")
values <- c(42,   0.053,  100,   0.255)

fput(types, stat_fmt, values)

## ----expr-twoargs-------------------------------------------------------------
ratio_fmt <- fnew(
  "ratio" = "sprintf('%s/%s', .x1, .x2)",
  name = "ratio",
  type = "character"
)

fput("ratio", ratio_fmt, 3, 10)
fput(c("ratio", "ratio"), ratio_fmt, c(3, 7), c(10, 20))

## ----expr-ifelse--------------------------------------------------------------
sign_fmt <- fnew(
  "val" = "ifelse(.x1 > 0, paste0('+', .x1), as.character(.x1))",
  name = "sign",
  type = "character"
)

nums <- c(5, 0, -3)
fput(rep("val", 3), sign_fmt, nums)

## ----expr-mixed---------------------------------------------------------------
mixed_fmt <- fnew(
  "header" = "HEADER",
  "n"      = "sprintf('N=%s', .x1)",
  "pct"    = "sprintf('%.1f%%', .x1 * 100)",
  name = "mixed",
  type = "character"
)

keys <- c("header", "n", "pct", "header", "n")
vals <- c(0,        42,  0.15,  0,        100)
fput(keys, mixed_fmt, vals)

## ----expr-other---------------------------------------------------------------
known_fmt <- fnew(
  "ok" = "OK",
  .other = "sprintf('Error(%s)', .x1)",
  name = "err_fmt",
  type = "character"
)

codes   <- c("ok", "E01", "ok", "E99")
details <- c("",   "timeout", "", "overflow")
fput(codes, known_fmt, details)

## ----expr-recycle-------------------------------------------------------------
label_fmt <- fnew(
  "val" = "sprintf('%s (N=%s)', .x1, .x2)",
  name = "recycle",
  type = "character"
)

fput(c("val", "val"), label_fmt, c(42, 55), 100)

fclear()

## ----vectorized---------------------------------------------------------------
# Dispatch format: maps type code to format name
fnew("1" = "groupx", "2" = "groupy", "3" = "groupz",
     name = "typefmt", type = "numeric")

# Per-group character formats
fnew("positive" = "agree",  "negative" = "disagree", "neutral" = "notsure",
     name = "groupx", type = "character")
fnew("positive" = "accept", "negative" = "reject",   "neutral" = "possible",
     name = "groupy", type = "character")
fnew("positive" = "pass",   "negative" = "fail",     "neutral" = "retest",
     name = "groupz", type = "character")

type     <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
response <- c("positive", "negative", "neutral",
              "positive", "negative", "neutral",
              "positive", "negative", "neutral")

# Step 1: map type -> format name
respfmt <- fput(type, "typefmt")

# Step 2: apply per-element format
word <- fputc(response, respfmt)

data.frame(type = type, response = response, respfmt = respfmt, word = word)

fclear()

## ----dates-putn---------------------------------------------------------------
# Format that maps key codes to date format names
fnew("1" = "date9.", "2" = "mmddyy10.",
     name = "writfmt", type = "numeric")

fnew_date("date9.")
fnew_date("mmddyy10.")

# Input data (R date numbers = days since 1970-01-01)
number <- c(12103, 10899)
key    <- c(1, 2)

# Look up format name per observation
datefmt <- fputn(key, "writfmt")

# Apply per-element date format
date <- fputn(number, datefmt)

data.frame(number = number, key = key, datefmt = datefmt, date = date)

fclear()

## ----cntlout-import-----------------------------------------------------------
csv_path <- system.file("extdata", "test_cntlout.csv", package = "ksformat")

## ----cntlout-use--------------------------------------------------------------
imported <- fimport(csv_path)
names(imported)

fprint()

## ----cntlout-apply------------------------------------------------------------
# Character format (GENDER)
gender_codes <- c("M", "F", NA, "X")
data.frame(
  code = gender_codes,
  label = fputc(gender_codes, "GENDER")
)

# Numeric format (AGEGRP)
ages <- c(5, 17, 18, 45, 65, 100, NA, -1)
data.frame(
  age = ages,
  group = fputn(ages, "AGEGRP")
)

# Numeric format (BMICAT)
bmi_values <- c(15.0, 18.5, 22.3, 25.0, 28.7, 30.0, 35.5)
data.frame(
  bmi = bmi_values,
  category = fputn(bmi_values, "BMICAT")
)

# Invalue (RACEIN)
race_labels <- c("White", "Black", "Asian", "Other")
data.frame(
  label = race_labels,
  code = finputn(race_labels, "RACEIN")
)

## ----cntlout-df---------------------------------------------------------------
df <- data.frame(
  id = 1:5,
  sex = c("M", "F", "M", NA, "F"),
  age = c(10, 30, 70, NA, 50),
  stringsAsFactors = FALSE
)

gender_fmt <- imported[["GENDER"]]
age_fmt    <- imported[["AGEGRP"]]

fput_df(df, sex = gender_fmt, age = age_fmt, suffix = "_label")

## ----cntlout-export-----------------------------------------------------------
cat(fexport(AGEGRP = age_fmt))
cat(fexport(GENDER = gender_fmt))

## ----cntlout-manual-----------------------------------------------------------
fclear()

manual <- fimport(csv_path, register = FALSE)

# Library should be empty
fprint()

# Use directly from returned list
fput(c("M", "F"), manual[["GENDER"]])

fclear()

