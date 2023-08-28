
n_surv <- nrow(d_surv)
fast <- d_surv$lowtag[d_surv$right_age_r == d_surv$left_age_e]
d_fit <- data.frame(matrix(NA,nr = n_surv + sum(d_surv$censored == 0), ncol = ncol(d_surv)))
d_fit[1:n_surv,] <- d_surv
n_fit <- nrow(d_fit)
d_temp <- d_surv[d_surv$censored == 0,]
d_temp$censored  <- 1
d_fit[(n_surv + 1):n_fit,] <- d_temp
names(d_fit) <- names(d_surv)
d_fit <- d_fit[order(d_fit$lowtag),]
d_fit$left_age = d_fit$left_age_e
d_fit$left_age[d_fit$censored == 0] <- d_fit$right_age_r[d_fit$censored == 0]
d_fit$left_period <- d_fit$left_period_e - nT_period_precollar_ext
d_fit$left_period[d_fit$censored == 0] <- d_fit$right_period_r[d_fit$censored == 0]- nT_period_precollar_ext

d_fit$right_age <- d_fit$right_age_r
d_fit$right_age[d_fit$censored == 0] <- d_fit$right_age_s[d_fit$censored == 0]
d_fit$right_period <- d_fit$right_period_r- nT_period_precollar_ext
d_fit$right_period[d_fit$censored == 0] <- d_fit$right_period_s[d_fit$censored == 0]- nT_period_precollar_ext

d_fit$age2date <- d_fit$left_period - d_fit$left_age
rm <- which(d_fit$lowtag %in% fast & d_fit$censored == 1)
d_fit <- d_fit[-rm,]
n_fit <- nrow(d_fit)

