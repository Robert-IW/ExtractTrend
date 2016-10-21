library(doMC) # multi-core
require(mgcv)
library(plyr)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)

# the non-linear GAMM -----------------------------------------------------
gamm_non_fun <- function(df) {
  out <- tryCatch({
    model <-
      gamm(
        sst ~ s(d.o.y., bs = "cc", k = 20) + s(days, bs = "cr"),
        correlation = corARMA(form = ~ 1 | year, p=1),                          # note p=3 better, p=2 error
        method = "REML",
        data = df, na.action = na.omit
      )
    return(model)
  },
  error = function(cond) {
    stats <- data.frame(
      DT_model = NA,
      se_trend = NA,
      sd_initial = round(sd(df$sst, na.rm = TRUE), 2),
      sd_residual = NA,
      r = NA,
      R2 = NA,
      p_trend = NA,
      p_seas = NA,
      length = length(df$sst),
      model = "gamm_non"
    )
    return(stats)
  })
  rm(model)
  return(out)
}

############################### Test model residuals
#layout(matrix(1:2, ncol = 2))
#res <- resid(modelAR3$lme, type = "normalized")
#acf(res, lag.max = 36, main = "ACF - AR(3) errors")
#pacf(res, lag.max = 36, main = "pACF- AR(3) errors")
#layout(1)