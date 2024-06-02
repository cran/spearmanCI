.onAttach <- function(libname, pkgname) {
  packageStartupMessage("## ======================================================================= ##")
  RFver <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                    fields = "Version")
  packageStartupMessage(paste
                       ("##", pkgname, RFver, "                                                         ##"))
  packageStartupMessage("## ----------------------------------------------------------------------- ##")
  packageStartupMessage("##  To cite in publications use:                                           ##")
  packageStartupMessage("##  1) de Carvalho, M. (2024). spearmanCI: Jackknife Euclidean / Empirical ##")
  packageStartupMessage("##     Likelihood Inference for Spearman Rho. R package version 1.1.       ##")
  packageStartupMessage("##                                                                         ##")
  packageStartupMessage("##  2) de Carvalho, M. & Marques, F. (2012).                               ##")
  packageStartupMessage("##     'Jackknife Euclidean Likelihood-Based Inference for Spearman's Rho' ##")
  packageStartupMessage("##     North American Actuarial Journal, 16, 487-492.                      ##")
  packageStartupMessage("## ======================================================================= ##")
  packageStartupMessage("")
}
