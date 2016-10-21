## Generalized Least Squares with autocorrelation order 2

# the GLS-AR2 -------------------------------------------------------------
gls_fun <- function(df) {
  out <- tryCatch({
    model <- gls(
      sst ~ num,
      correlation = corARMA(form = ~ 1, p = 2),      # time covariate with no grouping factor, autoreg order 2
      method = "REML",
      data = df, na.action = na.exclude
    )
    stats <-
      data.frame(
        DT_model = round(as.numeric(coef(model)[2]) * 120, 3),
        se_trend = round(summary(model)[["tTable"]][[2, 2]], 10),
        sd_initial = round(sd(df$temp, na.rm = TRUE), 2),
        sd_residual = round(sd(model$residuals), 2),
        r = NA,
        R2 = NA,
        p_trend = summary(model)[["tTable"]][[2, 4]],
        p_seas = NA,
        length = length(df$sst),
        model = "gls"
      )
    return(stats)
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
      model = "gls"
    )
    return(stats)
  })
  rm(model)
  return(out)
}