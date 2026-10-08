lib <- .Library
cat("Installation dans:", lib, "\n")
pk <- c("shiny","shinydashboard","shinyWidgets","DT","readxl","dplyr",
        "stringr","stringdist","openxlsx","lubridate","knitr","pagedown")
install.packages(pk, lib = lib, repos = "https://cloud.r-project.org")
ok <- sapply(pk, function(p) dir.exists(file.path(lib, p)))
print(ok)
if (!all(ok)) quit(status = 1)
cat("TOUT EST OK\n")
