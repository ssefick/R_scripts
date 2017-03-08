spec.cor <- function (dat, r, ...) 
{
    dat <- dat[, sapply(dat, FUN = is.numeric)]
    x <- cor(dat, ...)
    x[upper.tri(x, TRUE)] <- NA
    i <- which(abs(x) >= r, arr.ind = TRUE)
    out <- data.frame(matrix(colnames(x)[as.vector(i)], ncol = 2), value = x[i])
    out[order(abs(as.numeric(out[, 3])), decreasing = T), ]
}
spec.cor2 <- function (dat, r, VAR, ...) 
{
    dat <- dat[, sapply(dat, FUN = is.numeric)]
    spec.cor(dat, r)[c(which(spec.cor(dat, r)[, 1] == VAR), which(spec.cor(dat, r)[, 2] == VAR)), ]
}
