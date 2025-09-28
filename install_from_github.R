# Install semScreenR from GitHub
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install from GitHub
devtools::install_github("MohamedaliS/semScreenR")

# Test the installation
library(semScreenR)
cat("Package version:", as.character(packageVersion("semScreenR")), "\n")
cat("Package location:", find.package("semScreenR"), "\n")
