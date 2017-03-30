MNK_frame <- function(F_tbl) {
    m = ncol(F_tbl) - 1
    n = nrow(F_tbl)
    
    F_tbl <- as.matrix(F_tbl)
    y <- F_tbl[, ncol(F_tbl)]
    
    X <- list()
    for (i in 1:m)
        X[[i]] <- as.matrix(F_tbl[, 1:i])
    D <- list()
    for (i in 1:m)
        D[[i]] <-
        diag(n) - X[[i]] %*% solve(t(X[[i]]) %*% X[[i]]) %*% t(X[[i]])
    h <- list(NA)
    for (i in 2:m)
        h[[i]] <- t(X[[i - 1]]) %*% F_tbl[, i]
    eta <- list()
    for (i in 1:m)
        eta[[i]] <- t(F_tbl[, i]) %*% F_tbl[, i]
    gamma <- list()
    for (i in 1:m)
        gamma[[i]] <- t(F_tbl[, i]) %*% y
    
    theta <- list(t(F_tbl[, 1]) %*% y / t(F_tbl[, 1]) %*% F_tbl[, 1])
    H_inv <- list(1 / t(F_tbl[, 1]) %*% F_tbl[, 1])
    beta <- list(NA)
    rss <- c(t(y) %*% D[[1]] %*% y)
    Cp <- c(rss[1] + 2)
    FPE <- c(rss[1] * (n + 1) / (n - 1))
    
    for (s in 1:(m - 1)) {
        beta[[s + 1]] <-
            eta[[s + 1]] - t(h[[s + 1]]) %*% H_inv[[s]] %*% h[[s +
                                                                   1]]
        theta[[s + 1]] <- as.matrix(c(
            theta[[s]] - as.numeric(gamma[[s + 1]] - t(h[[s + 1]]) %*% theta[[s]]) *
                H_inv[[s]] %*% h[[s + 1]] / as.numeric(beta[[s + 1]]),
            (gamma[[s + 1]] - t(h[[s + 1]]) %*% theta[[s]]) / beta[[s + 1]]
        ),
        nrow = s + 1)
        r1 <-
            cbind(
                H_inv[[s]] - H_inv[[s]] %*% h[[s + 1]] %*% t(h[[s + 1]]) %*% H_inv[[s]] /
                    as.numeric(beta[[s + 1]]),
                -H_inv[[s]] %*% h[[s + 1]] / as.numeric(beta[[s +
                                                                  1]])
            )
        r2 <-
            cbind(-t(h[[s + 1]]) %*% H_inv[[s]] / as.numeric(beta[[s + 1]]),
                  1 / as.numeric(beta[[s +
                                           1]]))
        H_inv[[s + 1]] <- rbind(r1, r2)
        rss[[s + 1]] <-
            rss[[s]] - (t(F_tbl[, s + 1]) %*% D[[s]] %*% y) ^ 2 / (t(F_tbl[, s + 1]) %*%
                                                                       D[[s]] %*% F_tbl[, s + 1])
        Cp[s + 1] <- rss[s + 1] + 2 * s
        FPE[s + 1] <- c(rss[s + 1] * (n + s) / (n - s))
    }
    return(list(
        Theta = unlist(theta[[length(theta)]]),
        RSS = rss,
        Cp = Cp,
        FPE = FPE
    ))
}
