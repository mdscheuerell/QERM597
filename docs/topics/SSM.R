## ----setup, include=FALSE-----------------------------------------------------------------------------
set.seed(206)


## ----ex_bias_rw---------------------------------------------------------------------------------------
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


## ----linear_regr--------------------------------------------------------------------------------------
theta <- coef(lm(yy ~ seq(TT)))
y_obs <- theta[1] + theta[2] * seq(TT)

par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, ylim = range(yy, y_obs),
        cex = 1.5, col = "#844870",
        las = 1, ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(y_obs, lwd = 2, type = "o", pch = 16, col = "#488fdf")

par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, ylim = range(yy, y_obs),  
        cex = 1.5, col = "#844870",
        las = 1, ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(y_obs, lwd = 2, type = "o", pch = 16, col = "#488fdf")
segments(seq(TT), y0 = y_obs, y1 = yy, col = "red")


## ----brw_states_obs_0---------------------------------------------------------------------------------
xw <- rbind(cbind(seq(TT), xx), cbind(seq(TT)+0.5, xx+uu))
xw <- xw[order(xw[,1]),]
xw[,1] <- c(1, rep(seq(2, TT), ea = 2), TT)
  
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
## blank slate
plot.ts(yy, ylim = range(xx,yy), type = "n",
        las = 1, ylab = expression(italic(x[t])))
lines(xw[,1], xw[,2], lwd = 2, col = "gray")
lines(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf")


## ----ex_biased_rw, fig.dim = c(8,4.5)-----------------------------------------------------------------
x1 <- cumsum(rnorm(30, 0.1, 0.3)) + 10

x2 <- cumsum(rnorm(30, -0.1, 0.3)) + 10

clr1 <- c("#f7fbff",
          "#deebf7",
          "#c6dbef",
          "#9ecae1",
          "#6baed6",
          "#4292c6",
          "#2171b5",
          "#08519c",
          "#08306b")

clr2 <- c("#fff5f0",
          "#fee0d2",
          "#fcbba1",
          "#fc9272",
          "#fb6a4a",
          "#ef3b2c",
          "#cb181d",
          "#a50f15",
          "#67000d")

par(mfrow = c(1,2), mai = c(1, 1, 0.75, 0))
plot.ts(x1, las = 1, col = "dodgerblue", lwd = 2, ylim = c(8, 16),
        ylab = expression(italic(x[t])), main = "")
mtext("Positive bias", side = 3, line = 1, cex = 1.5)
for(i in 9:3) {
  lines(cumsum(rnorm(30, 0.1, 0.3)) + 10,
        col = clr1[i], lwd = 2)
}

plot.ts(x2, las = 1, col = "indianred", lwd = 2,  ylim = c(4, 12),
        ylab = expression(italic(x[t])), main = "")
mtext("Negative bias", side = 3, line = 1, cex = 1.5)
for(i in 9:3) {
  lines(cumsum(rnorm(30, -0.1, 0.3)) + 10,
        col = clr2[i], lwd = 2)
}


## ----biased_rw_fit------------------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870",
        las = 1, ylab = expression(italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, col = "#488fdf")


## ----biased_rw_errors---------------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870",
        las = 1, ylab = expression(italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, col = "#488fdf")
segments(seq(2,TT), y0 = yy[-TT], y1 = yy[-1], col = "red")


## ----brw_states_obs-----------------------------------------------------------------------------------
xw <- rbind(cbind(seq(TT), xx), cbind(seq(TT)+0.5, xx+uu))
xw <- xw[order(xw[,1]),]
xw[,1] <- c(1, rep(seq(2, TT), ea = 2), TT)
  
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
## blank slate
plot.ts(yy, ylim = range(xx,yy), type = "n",
        las = 1, ylab = expression(italic(x[t])))
lines(xw[,1], xw[,2], lwd = 2, col = "gray")
lines(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf")


## ----brw_states_obs_2---------------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
## blank slate
plot.ts(yy, ylim = range(xx,yy), type = "n",
        las = 1, ylab = expression(italic(x[t])))
## lines(xw[,1], xw[,2], type = "o", pch = 16, lwd = 2, col = "gray")
lines(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf")


## ----brw_states_obs_errors----------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(yy, type = "n",
        las = 1, ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf")
lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")
segments(seq(TT), y0 = xx, y1 = yy, col = "red")


## ----brw_states_obs_errors_2--------------------------------------------------------------------------
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))
## blank slate
plot.ts(yy, ylim = range(xx,yy), type = "n",
        las = 1, ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(xw[,1], xw[,2], lwd = 2, col = "gray")
lines(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf")
segments(seq(TT), y0 = xx, y1 = yy, col = "red")
lines(yy, lwd = 2, type = "o", pch = 16, col = "#844870", cex = 1.5)


## ----ex_proc_obs_errors, fig.align="center"-----------------------------------------------------------
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


## ----rw_sim, echo = TRUE, eval = TRUE-----------------------------------------------------------------
## number of time steps
TT <- 40
## bias term
uu <- 0.2
## time series of process errors with var = 1
ww <- rnorm(TT, 0, sqrt(1))
## initialize state & set x0 = w0
xx <- ww
## loop over time steps
for(t in 2:TT) {
  xx[t] <- uu + xx[t-1] + ww[t]
}


## ----rw_obs_error, echo=TRUE--------------------------------------------------------------------------
## obs errors with var = 0.3
vv <- rnorm(TT, 0, sqrt(0.3))
## obs data
yy <- xx + vv


## ----rw_plot_out, align.fig="center"------------------------------------------------------------------
par(mai = c(0.8,0.8,0,0), omi = c(0,0,0,0))
plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
        las = 1, ylim = c(min(xx,yy), max(xx,yy)),
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")


## ---- echo=TRUE---------------------------------------------------------------------------------------
mod_list <- list(
  ## state model
  B = matrix(1), U = matrix("u"), Q = matrix("q"),
  ## obs model
  Z = matrix(1), A = matrix(0), R = matrix("r")
  )


## ----brw_fit, echo=FALSE, eval=TRUE-------------------------------------------------------------------
## load MARSS package
library(MARSS)
## define the data as an N (rows) x T (cols) matrix
dat <- matrix(yy, nrow = 1, ncol = TT)
## fit the model
mod_fit <- MARSS(y = dat, model = mod_list)


## ---- echo = TRUE, eval = TRUE------------------------------------------------------------------------
## T x 1 (transposed) vector of states
mod_fits <- t(mod_fit$states)

## T x 1 (transposed) vector of SE's
mod_fits_SE <- t(mod_fit$states.se)


## ----brw_CI, eval = TRUE, echo = TRUE-----------------------------------------------------------------
## upper 95% CI
mod_fits_CI_hi <- mod_fits + qt(p = 0.975, df = TT - 1) * mod_fits_SE

## lower 95% CI
mod_fits_CI_lo <- mod_fits - qt(p = 0.975, df = TT - 1) * mod_fits_SE


## ----rw_plot_fits_states, align.fig="center"----------------------------------------------------------
par(mai = c(0.8,0.8,0,0), omi = c(0,0,0,0))
plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
        las = 1, ylim = c(min(xx,yy), max(xx,yy)),
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
# lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")
lines(mod_fits, lwd = 2, type = "l", cex = 1.5, col = "black")
lines(mod_fits_CI_hi, lwd = 2, type = "l", cex = 1.5, col = "gray")
lines(mod_fits_CI_lo, lwd = 2, type = "l", cex = 1.5, col = "gray")


## ----rw_plot_fit_nostate, align.fig="center"----------------------------------------------------------
par(mai = c(0.8,0.8,0,0), omi = c(0,0,0,0))
plot.ts(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870",
        las = 1, ylim = c(min(xx,yy), max(xx,yy)),
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(mod_fits, lwd = 2, type = "l", cex = 1.5, col = "black")
lines(mod_fits_CI_hi, lwd = 2, type = "l", cex = 1.5, col = "gray")
lines(mod_fits_CI_lo, lwd = 2, type = "l", cex = 1.5, col = "gray")


## ----rw_plot_fit, align.fig="center"------------------------------------------------------------------
par(mai = c(0.8,0.8,0,0), omi = c(0,0,0,0))
plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
        las = 1, ylim = c(min(xx,yy), max(xx,yy)),
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")
lines(mod_fits, lwd = 2, type = "l", cex = 1.5, col = "black")
lines(mod_fits_CI_hi, lwd = 2, type = "l", cex = 1.5, col = "gray")
lines(mod_fits_CI_lo, lwd = 2, type = "l", cex = 1.5, col = "gray")


## ----rw_model, echo=TRUE------------------------------------------------------------------------------
mod_list <- list(
  ## state model
  B = matrix(1), U = matrix(0), Q = matrix("q"),
  ## obs model
  Z = matrix(1), A = matrix(0), R = matrix("r")
  )


## ----rw_fit, echo = TRUE, eval=TRUE-------------------------------------------------------------------
## fit the model
mod_fit_2 <- MARSS(y = dat, model = mod_list)


## ----compare_AICc, echo = TRUE, eval = TRUE-----------------------------------------------------------
## biased RW
mod_fit$AICc

## unbiased RW
mod_fit_2$AICc


## ----gompertz_sim, echo = TRUE, eval= TRUE------------------------------------------------------------
## number of time steps
TT <- 40
## strength of density-dependence (0 < b < 1)
bb <- 0.5
## time series of process errors with SD = 1
ww <- rnorm(TT, 0, 0.5)
## initialize state & set x0 = w0
xx <- ww
## loop over time steps
for(t in 2:TT) {
  xx[t] <- bb * xx[t-1] + ww[t]
}


## ----gomp_obs_error, echo = TRUE, eval= TRUE----------------------------------------------------------
## obs errors with SD = 0.5
vv <- rnorm(TT, 0, 0.5)
## obs data
yy <- xx + vv


## ----gomp_plot, align.fig="center"--------------------------------------------------------------------
par(mai = c(0.8,0.8,0,0), omi = c(0,0,0,0))
plot.ts(xx, lwd = 2, type = "o", pch = 16, col = "#488fdf",
        las = 1, ylim = c(min(xx,yy), max(xx,yy)),
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(yy, lwd = 2, type = "o", pch = 16, cex = 1.5, col = "#844870")


## ---- echo=TRUE---------------------------------------------------------------------------------------
mod_list <- list(
  ## state model
  B = matrix("b"), U = matrix(0), Q = matrix("q"),
  ## obs model
  Z = matrix(1), A = matrix(0), R = matrix("r")
  )


## ----gomp_fit, echo = FALSE, eval = TRUE--------------------------------------------------------------
## define the data as an N (rows) x T (cols) matrix
dat <- matrix(yy, nrow = 1, ncol = TT)
## fit the model 
mod_fit <- MARSS(y = dat, model = mod_list)


## ----states_covars------------------------------------------------------------------------------------
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

