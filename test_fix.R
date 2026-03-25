library(ksformat)

fparse('
VALUE stat
  "mean" = "sprintf(\"%.2f\", .x1)"
;
')

# Test 1: scalar x, scalar extra arg (should still work)
cat("Test 1:", fput("mean", "stat", 1.23456), "\n")

# Test 2: scalar x, vector extra arg (the bug fix)
cat("Test 2:", fput("mean", "stat", c(1.23456, 2.34567, 3.45678)), "\n")

# Test 3: matching-length x and extra arg
cat("Test 3:", fput(c("mean","mean","mean"), "stat", c(1.23456, 2.34567, 3.45678)), "\n")

# Test 4: mismatched lengths (should error)
tryCatch(
  fput(c("mean","mean"), "stat", c(1.23456, 2.34567, 3.45678)),
  error = function(e) cat("Test 4 (expected error):", conditionMessage(e), "\n")
)
