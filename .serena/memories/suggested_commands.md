# Suggested Commands

## Development
```bash
# Install package locally
R CMD INSTALL .

# Install to user library
R CMD INSTALL --library=~/R/x86_64-pc-linux-gnu-library/4.5 .

# Build source tarball
R CMD build .

# Check package (CRAN-style)
R CMD check .
```

## Using devtools (from R console)
```r
# Generate/update documentation from roxygen2 comments
devtools::document()

# Run tests
devtools::test()

# Full CRAN check
devtools::check()

# Install from source
devtools::install()

# Load package for interactive development
devtools::load_all()
```

## Testing
```bash
# Run tests via R command line
Rscript -e "testthat::test_dir('tests/testthat')"

# Or via devtools
Rscript -e "devtools::test()"
```

## R Package Installation (dependencies)
```r
# Install dev dependencies (use project CRAN mirror)
install.packages(c("roxygen2", "testthat", "devtools", "cli"), repos = "https://mirror.truenetwork.ru/CRAN/")

# Install to user library if no write access to system library
install.packages("pkg", lib = Sys.getenv("R_LIBS_USER"), repos = "https://mirror.truenetwork.ru/CRAN/")
```

## System Utilities
```bash
# Standard Linux tools available
git, ls, cd, grep, find, cat, head, tail, wc

# R is at
/usr/local/bin/R
/usr/local/bin/Rscript

# uv/uvx are at
/home/rdev/.local/bin/uv
/home/rdev/.local/bin/uvx
```
