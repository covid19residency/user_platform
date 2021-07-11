# install.packages("pysd2r")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("reshape2")


my_packages = c("pysd2r", "ggplot2", "readxl", "reshape2")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))