## ----setup, include=FALSE-----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
set.seed(597)


## ----state_model, fig.height=4, fig.width=8, out.height="100%", out.width="100%", fig.align='center'--------------
par(mai=c(0.8,0.8,0,0), omi=rep(0,4))
## boundaries
ss <- 5
nn <- 7
rr <- ss*3
cc <- ss*nn
## mid-points
xm <- ss/2 + seq(0,cc-ss,ss)
ymt <- rr - ss/2
ymb <- ss/2
## arrow locs
x0t <- seq(ss, by=2*ss, len=3)
x1t <- x0t + ss
## empty plot space
plot(c(0,cc), c(0,rr), type="n", xlab="", ylab="",
     xaxt="n", yaxt="n", bty="n")
## top row: state
symbols(x=xm[c(1,3,5,7)], y=rep(ymt,4), circles=rep(ss/2,4),
        lty="solid",  fg=NA, bg="#488fdf",
        inches=FALSE, add=TRUE, lwd=3)
text("Truth", x=-ss, y=ymt, adj=c(0,0.5), xpd=NA,
     cex=2, col="#488fdf")
arrows(x0=x0t,x1=x1t,y0=ymt, col="#488fdf", lwd=3, length=0.12)
## Time or space
arrows(x0=ss/2, x1=cc-ss/2, y0=-ss/3+ss*2,
       length=0.12, lwd=3, xpd=NA)
text("Time", x=cc/2, y=-ss/2+ss*2, xpd=NA, pos=1, cex=2)


## ----obs_diag, fig.height=4, fig.width=8, out.height="100%", out.width="100%", fig.align='center'-----------------
par(mai=c(0.8,0.8,0,0), omi=rep(0,4))
## arrow locs
x0t <- seq(ss, by=2*ss, len=3)
x1t <- x0t + ss
y0b <- rr - ss
y1b <- ss
## empty plot space
plot(c(0,cc), c(0,rr), type="n", xlab="", ylab="",
     xaxt="n", yaxt="n", bty="n")
## top row: state
symbols(x=xm[c(1,3,5,7)], y=rep(ymt,4), circles=rep(ss/2,4),
        lty="solid",  fg=NA, bg="#488fdf",
        inches=FALSE, add=TRUE, lwd=3)
text("Truth", x=-ss, y=ymt, adj=c(0,0.5), xpd=NA,
     cex=2, col="#488fdf")
## arrows
arrows(x0=x0t,x1=x1t,y0=ymt, col="#488fdf", lwd=3, length=0.12)
## bottom row: obs
symbols(x=xm[c(1,3,5,7)], y=rep(ss/2,4), circles=rep(ss/2,4),
        lty="solid",  fg=NA, bg="#844870",
        inches=FALSE, add=TRUE, lwd=3)
text("Data", x=-ss, y=ss/2, adj=c(0,0.5), xpd=NA,
     cex=2, col="#844870")
## arrows
arrows(x0=xm[c(1,3,5,7)], y0=y0b, y1=y1b,
       col="#c10101", lwd=3, length=0.12)
## Time or space
arrows(x0=ss/2, x1=cc-ss/2, y0=-ss/3,
       length=0.12, lwd=3, xpd=NA)
text("Time", x=cc/2, y=-ss/2, xpd=NA, pos=1, cex=2)


## ----ex_bias_rw---------------------------------------------------------------------------------------------------
TT <- 30

uu <- -0.2
  
ww <- xx <- rnorm(TT)
for(t in 2:TT) {
  xx[t] <- xx[t-1] + uu + ww[t]
}

ee <- rnorm(TT)
yy <- xx + ee

par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, ylim = range(xx,yy),
        lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870",
        las = 1, ylab = expression(italic(y[t])))


## ----linear_regr--------------------------------------------------------------------------------------------------
theta <- coef(lm(yy ~ seq(TT)))
y_obs <- theta[1] + theta[2] * seq(TT)

par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, ylim = range(yy, y_obs), 
        cex = 1.5, col = "#844870",
        las = 1, ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(y_obs, lwd = 2, type = "o", pch = 16, col = "#488fdf")


## ----regr_errors--------------------------------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, ylim = range(yy, y_obs),  
        cex = 1.5, col = "#844870",
        las = 1, ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(y_obs, lwd = 2, type = "o", pch = 16, col = "#488fdf")
segments(seq(TT), y0 = y_obs, y1 = yy, col = "red")


## ----biased_rw_fit------------------------------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870",
        las = 1, ylab = expression(italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, col = "#488fdf")


## ----biased_rw_errors---------------------------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870",
        las = 1, ylab = expression(italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, col = "#488fdf")
segments(seq(2,TT), y0 = yy[-TT], y1 = yy[-1], col = "red")


## ----brw_states_obs-----------------------------------------------------------------------------------------------
xw <- rbind(cbind(seq(TT), xx), cbind(seq(TT)+0.5, xx+uu))
xw <- xw[order(xw[,1]),]
xw[,1] <- c(1, rep(seq(2, TT), ea = 2), TT)
  
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
## blank slate
plot.ts(yy, ylim = range(xx,yy), type = "n",
        las = 1, ylab = expression(italic(x[t])))
lines(xw[,1], xw[,2], lwd = 2, col = "gray")
lines(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf")


## ----brw_states_obs_2---------------------------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
## blank slate
plot.ts(yy, ylim = range(xx,yy), type = "n",
        las = 1, ylab = expression(italic(x[t])))
## lines(xw[,1], xw[,2], type = "o", pch = 16, lwd = 2, col = "gray")
lines(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf")


## ----brw_states_obs_errors----------------------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, type = "n",
        las = 1, ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf")
lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")
segments(seq(TT), y0 = xx, y1 = yy, col = "red")


## ----brw_states_obs_errors_2--------------------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
## blank slate
plot.ts(yy, ylim = range(xx,yy), type = "n",
        las = 1, ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(xw[,1], xw[,2], lwd = 2, col = "gray")
lines(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf")
segments(seq(TT), y0 = xx, y1 = yy, col = "red")
lines(yy, lwd = 2, type = "o", pch = 16, col = "#844870", cex = 1.5)


## ----ex_proc_obs_errors, fig.align="center"-----------------------------------------------------------------------
xp <- matrix(NA, TT, 3)
xp[1,] <- c(0,0,0)

for(t in 2:TT) {
  xp[t,] <- xp[t-1,] + uu + rnorm(3)
}

yp <- xp[,1] + matrix(rnorm(TT*3), TT, 3)

par(mfcol = c(2,3), mai = c(0.6,0.6,0.1,0), omi = c(0,0,0.2,0))
for(i in 1:3) {
  plot.ts(xp[,i], xlab = "", ylab = expression(italic(x[t])))
  if(i == 2) {
    mtext(side = 3, " Different realizations of same process",
          line = 0.5, xpd = NA)
    }
  plot.ts(yp[,i], ylab = expression(italic(y[t])))
  if(i == 2) {
    mtext(side = 3, " Different observations of same process",
          line = 0.5, xpd = NA)
    }
}


## ----rw_sim, echo = TRUE, eval = TRUE-----------------------------------------------------------------------------
## number of time steps
TT <- 40
## bias term
uu <- 0.3
## time series of process errors with SD = 1
ww <- rnorm(TT, 0, sqrt(1))
## initialize state & set x0 = w0
xx <- ww
## loop over time steps
for(t in 2:TT) {
  xx[t] <- uu + xx[t-1] + ww[t]
}


## ----rw_obs_error, echo=TRUE--------------------------------------------------------------------------------------
## obs errors with var = 0.5
vv <- rnorm(TT, 0, sqrt(0.5))
## obs data
yy <- xx + vv


## ----rw_plot_code, echo=TRUE, eval=FALSE--------------------------------------------------------------------------
## plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
##         las = 1, ylim = c(min(xx,yy), max(xx,yy)),
##         ylab = expression(italic(x[t])~~or~~italic(y[t])))
## lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")


## ----rw_plot_out, align.fig="center"------------------------------------------------------------------------------
par(mai = c(0.8,0.8,0,0), omi = c(0,0,0,0))
plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
        las = 1, ylim = c(min(xx,yy), max(xx,yy)),
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")


## ---- echo=TRUE, eval=FALSE---------------------------------------------------------------------------------------
## MARSS(y, model = NULL, inits = NULL, control = NULL, ...)


## ---- echo=TRUE---------------------------------------------------------------------------------------------------
mod_list <- list(
  ## state model
  B = matrix(1), U = matrix("u"), Q = matrix("q"),
  ## obs model
  Z = matrix(1), A = matrix(0), R = matrix("r")
  )


## ---- echo=TRUE, eval=FALSE---------------------------------------------------------------------------------------
## ## load MARSS package
## library(MARSS)
## ## define the data as an N (rows) x T (cols) matrix
## dat <- matrix(yy, nrow = 1, ncol = TT)
## ## fit the model
## mod_fit <- MARSS(y = dat, model = mod_list)


## ----brw_fit, echo=FALSE, eval=TRUE-------------------------------------------------------------------------------
## load MARSS package
library(MARSS)
## define the data as an N (rows) x T (cols) matrix
dat <- matrix(yy, nrow = 1, ncol = TT)
## fit the model
mod_fit <- MARSS(y = dat, model = mod_list)


## ----brw_MARSS_output_detail_NLL, eval = FALSE, echo = TRUE-------------------------------------------------------
## ## Success! abstol and log-log tests passed at 61 iterations.
## ## Alert: conv.test.slope.tol is 0.5.
## ## Test with smaller values (<0.1) to ensure convergence.
## ##
## ## MARSS fit is
## ## Estimation method: kem
## ## Convergence test: conv.test.slope.tol = 0.5, abstol = 0.001
## ## Estimation converged in 61 iterations.
## ### <b>
## ## Log-likelihood: -68.66275
## ## AIC: 145.3255   AICc: 146.4684
## ### </b>
## ##
## ##       Estimate
## ## R.r     0.4893
## ## U.u     0.0313
## ## Q.q     0.9798
## ## x0.x0   0.5812
## ## Initial states (x0) defined at t=0
## ##
## ## Standard errors have not been calculated.
## ## Use MARSSparamCIs to compute CIs and bias estimates.


## ----brw_MARSS_output_detail_pars, eval = FALSE, echo = TRUE------------------------------------------------------
## ## Success! abstol and log-log tests passed at 61 iterations.
## ## Alert: conv.test.slope.tol is 0.5.
## ## Test with smaller values (<0.1) to ensure convergence.
## ##
## ## MARSS fit is
## ## Estimation method: kem
## ## Convergence test: conv.test.slope.tol = 0.5, abstol = 0.001
## ## Estimation converged in 61 iterations.
## ## Log-likelihood: -68.66275
## ## AIC: 145.3255   AICc: 146.4684
## ##
## ### <b>
## ##       Estimate
## ## R.r     0.4893
## ## U.u     0.0313
## ## Q.q     0.9798
## ## x0.x0   0.5812
## ## Initial states (x0) defined at t=0
## ### </b>
## ##
## ## Standard errors have not been calculated.
## ## Use MARSSparamCIs to compute CIs and bias estimates.


## ---- echo = TRUE, eval = TRUE------------------------------------------------------------------------------------
## T x 1 (transposed) vector of states
mod_fits <- t(mod_fit$states)

## T x 1 (transposed) vector of SE's
mod_fits_SE <- t(mod_fit$states.se)


## ----brw_CI, eval = TRUE, echo = TRUE-----------------------------------------------------------------------------
## upper 95% CI
mod_fits_CI_hi <- mod_fits + qt(p = 0.975, df = TT - 1) * mod_fits_SE

## lower 95% CI
mod_fits_CI_lo <- mod_fits - qt(p = 0.975, df = TT - 1) * mod_fits_SE


## ----rw_plot_fit_code, echo=TRUE, eval=FALSE----------------------------------------------------------------------
## plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
##         las = 1, ylim = c(min(xx,yy), max(xx,yy)),
##         ylab = expression(italic(x[t])~~or~~italic(y[t])))
## lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")
## lines(mod_fits, lwd = 2, type = "l", cex = 1.5, col = "black")
## lines(mod_fits_CI_hi, lwd = 2, type = "l", cex = 1.5, col = "gray")
## lines(mod_fits_CI_lo, lwd = 2, type = "l", cex = 1.5, col = "gray")


## ----rw_plot_fit, align.fig="center"------------------------------------------------------------------------------
par(mai = c(0.8,0.8,0,0), omi = c(0,0,0,0))
plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
        las = 1, ylim = c(min(xx,yy), max(xx,yy)),
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")
lines(mod_fits, lwd = 2, type = "l", cex = 1.5, col = "black")
lines(mod_fits_CI_hi, lwd = 2, type = "l", cex = 1.5, col = "gray")
lines(mod_fits_CI_lo, lwd = 2, type = "l", cex = 1.5, col = "gray")


## ----rw_plot_fit_nostate, align.fig="center"----------------------------------------------------------------------
par(mai = c(0.8,0.8,0,0), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870",
        las = 1, ylim = c(min(xx,yy), max(xx,yy)),
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(mod_fits, lwd = 2, type = "l", cex = 1.5, col = "black")
lines(mod_fits_CI_hi, lwd = 2, type = "l", cex = 1.5, col = "gray")
lines(mod_fits_CI_lo, lwd = 2, type = "l", cex = 1.5, col = "gray")


## ----rw_model, echo=TRUE------------------------------------------------------------------------------------------
mod_list <- list(
  ## state model
  B = matrix(1), U = matrix(0), Q = matrix("q"),
  ## obs model
  Z = matrix(1), A = matrix(0), R = matrix("r")
  )


## ----rw_fit, echo = TRUE, eval=TRUE-------------------------------------------------------------------------------
## fit the model
mod_fit_2 <- MARSS(y = dat, model = mod_list)


## ----compare_AICc, echo = TRUE, eval = TRUE-----------------------------------------------------------------------
## biased RW
mod_fit$AICc

## unbiased RW
mod_fit_2$AICc


## ----gompertz_sim, echo=TRUE--------------------------------------------------------------------------------------
## number of time steps
TT <- 40
## strength of density-dependence (0 < b < 1)
bb <- 0.6
## time series of process errors with SD = 1
ww <- rnorm(TT, 0, sqrt(1))
## initialize state & set x0 = w0
xx <- ww
## loop over time steps
for(t in 2:TT) {
  xx[t] <- bb * xx[t-1] + ww[t]
}


## ----gomp_obs_error, echo=TRUE------------------------------------------------------------------------------------
## obs errors with var = 0.5
vv <- rnorm(TT, 0, sqrt(0.5))
## obs data
yy <- xx + vv


## ---- echo=TRUE, eval=FALSE---------------------------------------------------------------------------------------
## plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
##         las = 1, ylim = c(min(xx,yy), max(xx,yy)),
##         ylab = expression(italic(x[t])~~or~~italic(y[t])))
## lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")


## ----gomp_plot, align.fig="center"--------------------------------------------------------------------------------
par(mai = c(0.8,0.8,0,0), omi = c(0,0,0,0))
plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
        las = 1, ylim = c(min(xx,yy), max(xx,yy)),
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")


## ----gomp_model, echo=TRUE----------------------------------------------------------------------------------------
mod_list <- list(
  ## state model
  B = matrix("b"), U = matrix(0), Q = matrix("q"),
  ## obs model
  Z = matrix(1), A = matrix(0), R = matrix("r")
  )


## ---- echo = TRUE, eval = FALSE-----------------------------------------------------------------------------------
## ## define the data as an N (rows) x T (cols) matrix
## dat <- matrix(yy, nrow = 1, ncol = TT)
## ## fit the model
## mod_fit <- MARSS(y = dat, model = mod_list)


## ----gomp_fit, echo = FALSE, eval = TRUE--------------------------------------------------------------------------
## define the data as an N (rows) x T (cols) matrix
dat <- matrix(yy, nrow = 1, ncol = TT)
## fit the model
mod_fit <- MARSS(y = dat, model = mod_list)


## ----MARSS_output_detail_NLL, eval = FALSE, echo = TRUE-----------------------------------------------------------
## ## Success! abstol and log-log tests passed at 61 iterations.
## ## Alert: conv.test.slope.tol is 0.5.
## ## Test with smaller values (<0.1) to ensure convergence.
## ##
## ## MARSS fit is
## ## Estimation method: kem
## ## Convergence test: conv.test.slope.tol = 0.5, abstol = 0.001
## ## Estimation converged in 61 iterations.
## ### <b>
## ## Log-likelihood: -65.54171
## ## AIC: 139.0834   AICc: 140.2263
## ### </b>
## ##
## ##       Estimate
## ## R.r      0.430
## ## B.b      0.625
## ## Q.q      1.007
## ## x0.x0    0.947
## ## Initial states (x0) defined at t=0
## ##
## ## Standard errors have not been calculated.
## ## Use MARSSparamCIs to compute CIs and bias estimates.


## ----MARSS_output_detail_pars, eval = FALSE, echo = TRUE----------------------------------------------------------
## ## Success! abstol and log-log tests passed at 61 iterations.
## ## Alert: conv.test.slope.tol is 0.5.
## ## Test with smaller values (<0.1) to ensure convergence.
## ##
## ## MARSS fit is
## ## Estimation method: kem
## ## Convergence test: conv.test.slope.tol = 0.5, abstol = 0.001
## ## Estimation converged in 61 iterations.
## ## Log-likelihood: -65.54171
## ## AIC: 139.0834   AICc: 140.2263
## ##
## ### <b>
## ##       Estimate
## ## R.r      0.430
## ## B.b      0.625
## ## Q.q      1.007
## ## x0.x0    0.947
## ## Initial states (x0) defined at t=0
## ### </b>
## ##
## ## Standard errors have not been calculated.
## ## Use MARSSparamCIs to compute CIs and bias estimates.


## ----states_covars------------------------------------------------------------------------------------------------
ww <- xx <- xy <- rnorm(TT)

bb <- 0.7

CC <- DD <- 2
cc <- dd <- sin(2*pi*seq(TT)/12)
  
for(t in 2:TT) {
  xx[t] <- bb * xx[t-1] + CC * cc[t] + ww[t]
  xy[t] <- bb * xy[t-1] + ww[t]
}

ee <- rnorm(TT)
yy <- xy + DD * dd # + ee

par(mai = c(0.9,0.9,0.5,0.1), omi = c(0,0,0,0))
plot.ts(xx, ylim = range(xx,yy),
        lwd = 2, type = "o", pch = 16, col = "#488fdf",
        las = 1, ylab = expression(italic(x[t])))
mtext(side = 3, expression(italic(x[t])==italic(b)~italic(x[t-1])~+~italic(C)~italic(c[t])~+~italic(w[t])),
      line = 0.5, adj = 0)

