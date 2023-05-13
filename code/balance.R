bstat <- function (rawdata, mdata, rtrt, mtrt, wts, estimand = "ATT"){
    covnames <- colnames(rawdata)
    K <- length(covnames)
    diff.means <- matrix(NA, K, 5)
    var.t <- numeric(K)
    var.c <- numeric(K)
    std.denom <- numeric(K)
    binary <- rep(1, K)
    for (i in 1:K) {
        diff.means[i, 1] <- mean(rawdata[rtrt == 1, i])
        diff.means[i, 2] <- mean(rawdata[rtrt == 0, i])
        var.t[i] <- var(rawdata[(rtrt == 1), i])
        var.c[i] <- var(rawdata[(rtrt == 0), i])
        if (estimand == "ATE") {
            std.denom[i] <- sqrt((var.t[i] + var.c[i])/2)
        }
        else {
            std.denom[i] <- ifelse(estimand == "ATT", sqrt(var.t[i]), sqrt(var.c[i]))
        }
        diff.means[i, 3] <- diff.means[i, 1] - diff.means[i,2]
        diff.means[i, 4] <- abs(diff.means[i, 3]/std.denom[i])
        if (length(unique(rawdata[, covnames[i]])) > 2) {
            binary[i] = 0
        }
    }
    dimnames(diff.means) <- list(covnames, c("treat", "control", "unstd.diff", "abs.std.diff", "ratio"))
    diff.means.matched = matrix(NA, K, 5)
    for (i in 1:K) {
        wts0 <- wts[mtrt == 0]
        diff.means.matched[i, 1] <- mean(mdata[mtrt == 1,i])
        diff.means.matched[i, 2] <- weighted.mean(mdata[mtrt ==0, i], w = wts0)
        var.t[i] <- var(mdata[mtrt == 1, i])
        var.c[i] <- as.numeric(stats::cov.wt(mdata[mtrt == 0, i, drop = FALSE], wt = wts0)$cov)
        diff.means.matched[i, 3] <- diff.means.matched[i, 1] - diff.means.matched[i, 2]
        diff.means.matched[i, 4] <- abs(diff.means.matched[i,3])/std.denom[i]
        if (length(unique(rawdata[, covnames[i]])) > 2) {
            diff.means.matched[i, 5] <- sqrt(var.c[i]/var.t[i])
        }
    }
    dimnames(diff.means.matched) <- list(covnames, c("treat","control", "unstd.diff", "abs.std.diff","ratio"))
    out <- list(diff.means.raw = diff.means, diff.means.matched = diff.means.matched, covnames = covnames, binary = binary)
    class(out) <- "balance"
    return(out)
}


plotBalance <- function (x, longcovnames = NULL, which.covs = "mixed",
    v.axis = TRUE, cex.main = 1, cex.vars = 1, cex.pts = 1, mar = c(4,
        3, 5.1, 2), plot = TRUE, x.max = NULL, main="Absolute Standardized Difference in Means",...)
{
    covnames <- x$covnames
    if (!is.null(x.max)) {
        x.range = c(0, x.max)
    }
    if (which.covs == "mixed") {
        pts <- x$diff.means.raw[, 4]
        pts2 <- x$diff.means.matched[, 4]
        K <- length(pts)
        idx <- 1:K
        main = main
    }
    if (which.covs == "binary") {
        pts <- abs(x$diff.means.raw[x$binary == TRUE, 3])
        pts2 <- abs(x$diff.means.matched[x$binary == TRUE, 3])
        K <- length(pts)
        idx <- 1:K
        main = main
        covnames = covnames[x$binary == TRUE]
    }
    if (which.covs == "cont") {
        pts <- x$diff.means.raw[x$binary == FALSE, 4]
        pts2 <- x$diff.means.matched[x$binary == FALSE, 4]
        K <- length(pts)
        idx <- 1:K
        main = main
        covnames = covnames[x$binary == FALSE]
    }
    cat(pts, "\n")
    par(mar = mar)
    if (is.null(longcovnames)) {
        longcovnames <- covnames
        maxchar <- max(sapply(longcovnames, nchar))
    }
    else {
        maxchar <- max(sapply(longcovnames, nchar))
    }
    min.mar <- par("mar")
    mar[2] <- max(min.mar[2], trunc(mar[2] + maxchar/10)) + mar[2] +
        0.5
    par(mar = mar)
    pts = rev(pts)
    pts2 = rev(pts2)
    longcovnames = rev(longcovnames)
    if (plot) {
        if (is.null(x.max)) {
            plot(c(pts, pts2), c(idx, idx), bty = "n",
                xlab = "", ylab = "", xaxt = "n",
                yaxt = "n", type = "n", main = main,
                cex.main = cex.main)
        }
        if (!is.null(x.max)) {
            plot(c(pts, pts2), c(idx, idx), bty = "n",
                xlab = "", ylab = "", xaxt = "n",
                yaxt = "n", type = "n", xlim = x.range,
                main = main, cex.main = cex.main)
        }
        abline(v = 0, lty = 2)
        points(pts, idx, cex = cex.pts)
        points(pts2, idx, pch = 19, cex = cex.pts)
        if (v.axis) {
            axis(3)
        }
        if (is.null(longcovnames)) {
            axis(2, at = 1:K, labels = covnames[1:K], las = 2,
                hadj = 1, lty = 0, cex.axis = cex.vars)
        }
        else {
            axis(2, at = 1:K, labels = longcovnames[1:K], las = 2,
                hadj = 1, lty = 0, cex.axis = cex.vars)
        }
    }
    else {
        plot(c(pts, pts2), c(idx, idx), bty = "n", xlab = "",
            ylab = "", xaxt = "n", yaxt = "n",
            type = "n", axes = FALSE, main = "",
            cex.main = cex.main, ...)
    }
    return(list(raw = pts, matched = pts2))
}
