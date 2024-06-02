##  ========================================================================  ##
##  Miguel de Carvalho                                                        ##
##  Copyright (C) 2018                                                        ##
##  ------------------------------------------------------------------------  ##
##  This program is free software; you can redistribute it and/or modify      ##
##  it under the terms of the GNU General Public License as published by      ##
##  the Free Software Foundation; either version 2 of the License, or         ##
##  (at your option) any later version.                                       ##
##                                                                            ##
##  This program is distributed in the hope that it will be useful,           ##
##  but WITHOUT ANY WARRANTY; without even the implied warranty of            ##
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             ##
##  GNU General Public License for more details.                              ##
##                                                                            ##
##  You should have received a copy of the GNU General Public License         ##
##  along with this program; if not, a copy is available at                   ##
##  http://www.r-project.org/Licenses/                                        ##
##  ========================================================================  ##

spearmanCI <- function(x, y, level = 0.95, method = "Euclidean", plot = FALSE)
    UseMethod("spearmanCI")

spearmanCI.default <- function(x, y, level = 0.95, method = "Euclidean",
                               plot = FALSE) {
    nx <- length(x)
    ny <- length(y)
    
    ## Run a basic input validation
    if (is.vector(x) == FALSE | is.vector(x) == FALSE | nx != ny)
        stop('x and y must be vectors of the same length')
    if (sum(is.na(x)) != 0 | sum(is.na(y)) != 0)
        stop('missing values are not allowed')
    if (method == "Euclidean")
        m <- 0
    else if (method == "empirical")
        m <- 1
    else 
        stop('method must be "Euclidean" or "empirical')

    U <- cor(x, y, method = "spearman")
    n <- nx
    Z <- as.double()    
    for(i in 1:n)
        Z[i] <- n * U - (n - 1) * cor(x[-i], y[-i],
                                        method = "spearman")
    
    if (method == "Euclidean") {
        s <- function(theta)
            1 / n * sum((Z - theta)^2)
        
        g <- function(theta) 
            n * (U - theta)^2 / s(theta) - qchisq(level, 1)        
        
        l <- uniroot(g, interval = c(-1, U), tol = .1*10^{-10})$root
        u <- uniroot(g, interval = c(U, 1), tol = .1*10^{-10})$root

        if (plot == TRUE) {
            e_loglike <- function(theta)
                n * (U - theta)^2 / s(theta)
            theta <- seq(l - .1, u + .1, 0.01)
            plot(theta, sapply(theta, e_loglike), type = "l", lwd = 3,
                 ylab = expression(paste(-2%*% "Jackknife Euclidean Loglikelihood")),
                 xlab = "Spearman's Correlation", cex.axis = 1.4, cex.lab = 1.1)
            abline(h = qchisq(level, 1), lwd = 3, lty = 2)
        }
    }

    if (method == "empirical") {        
        pelr <- function(theta) 
            prod(el.test(Z, theta)$wts) 
        
        l <- uniroot(function(theta) pelr(theta) - exp(-qchisq(level, 1) / 2),
                     interval = c(min(Z), mean(Z)), tol = .1*10^{-10})$root
        u <- uniroot(function(theta) pelr(theta) - exp(-qchisq(level, 1) / 2),
                     interval = c(mean(Z), max(Z)), tol = .1*10^{-10})$root
        
        if (plot == TRUE) {
            e_loglike <- function(theta)
                -2 * log(pelr(theta))
            theta <- seq(l - .1, u + .1, 0.01)
            plot(theta, sapply(theta, e_loglike), type = "l", lwd = 3,
                 ylab = expression(paste(-2%*% "Jackknife Empirical Loglikelihood")),
                 xlab = "Spearman's Correlation", cex.axis = 1.4, cex.lab = 1.1)
            abline(h = qchisq(level, 1), lwd = 3, lty = 2)                       
        }        
    }
    
    summaries <- matrix(NA, 1, 2)
    colnames(summaries) <- c(paste((1 - level) / 2 * 100, "%"),
                             paste((level + (1 - level) / 2) * 100, "%"))
    rownames(summaries) <- c("")    
    summaries[1, ] <- c(l, u)
    
    cat("confidence interval \n")
    print(summaries, quote = FALSE)
    estimate <- matrix (U, 1, 1)
    cat("sample estimate")
    colnames(estimate) <- c("")
    rownames(estimate) <- c("")
    print(estimate, quote = FALSE)
}
