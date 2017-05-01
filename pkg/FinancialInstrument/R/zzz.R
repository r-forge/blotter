.onAttach <- function(libname, pkgname) {
  repo <- "https://github.com/braverock/FinancialInstrument"
  packageStartupMessage(
    "WARNING: this package was installed from R-Forge, but development has\n",
    "moved to GitHub. Please re-install the package using the GitHub repo at:\n",
    repo, ".")
}
