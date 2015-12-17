
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEdelta** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml
Name of QuantLet: SFEdelta

Published in: Statistics of Financial Markets

Description: 'Plots the Delta of a call option as a function of the time to maturity and the asset
price.'

Keywords: asset, black-scholes, call, european-option, financial, graphical representation, greeks,
option, option-price, plot

See also: SFEvanna, SFEvolga, SFEgamma, SFEvega, SFEtheta, SFEspeed, SFEcharmcall, SFEcolor,
SFEultima, SFEvomma, SFEzomma, SFEdvegadtime

Author: Ying Chen, Christian M. Hafner

Submitted: Sun, December 04 2011 by Dedy Dwi Prastyo

Example: 'For given [lower, upper] bound of Asset price S like [50,150] and [lower, upper] bound of
time to maturity tau like [0.01, 1] a plot of the Delta of a call option is produced.'
```

![Picture1](SFEdelta-1.png)


```r

# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("lattice")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# parameter settings
S_min   = 50          # lower bound of Asset Price
S_max   = 150         # upper bound of Asset Price 
tau_min = 0.01        # lower bound of Time to Maturity
tau_max = 1           # upper bound of Time to Maturity
K       = 100         # exercise price
r       = 0.1         # riskfree interest rate                  
sig     = 0.85        # volatility               
d       = 0.2         # dividend rate                
steps   = 60          # steps 

Tau = seq(tau_min, tau_max, by = (tau_max - tau_min)/(steps - 1))
S   = seq(S_max, S_min, by = -(S_max - S_min)/(steps - 1))

delta = function(Tau, S, K, r, d, sig) {
    y = (log(S/K) + (r - d + sig^2/2) * Tau)/(sig * sqrt(Tau))
    return(pnorm(y + sig * sqrt(Tau)))
}

mesh = outer(Tau, sort(S), delta, K = K, r = r, d = d, sig = sig)
title = bquote(expression(paste("Strike price is ", .(K), ", interest rate is ", 
    .(r), ", dividend rate is ", .(d), ", annual volatility is ", .(sig))))

# Plot
wireframe(mesh, drape = T, main = expression(paste("Delta as function of the time to maturity ", 
    tau, " and the asset price S")), sub = title, scales = list(arrows = FALSE, 
    col = "black", distance = 1, tick.number = 8, cex = 0.7, x = list(labels = round(seq(tau_min, 
        tau_max, length = 7), 1)), y = list(labels = round(seq(S_min, S_max, length = 7), 
        1))), xlab = list(expression(paste("Time to Maturity  ", tau)), rot = 30, 
    cex = 1.2), ylab = list("Asset Price S", rot = -40, cex = 1.2), zlab = list("Delta", 
    cex = 1.1))

```
