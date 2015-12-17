
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEBSCopt2** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml
Name of QuantLet: SFEBSCopt2

Published in: Statistics of Financial Markets

Description: 'Computes the Black-Scholes price of a European call option. Optionally, option
parameters may be given interactively as user input.'

Keywords: asset, black-scholes, call, european-option, financial, option, option-price, normal
approximation, normal-distribution, option

See also: SFEBSCopt1, SFENormalApprox1, SFENormalApprox2, SFENormalApprox3, SFENormalApprox4

Author: Felix Jung

Submitted: Wed, April 09 2014 by Felix Jung

Example: 'For [spot price, strike price, interest rate]= [98, 100, 0.05],[cost of carry b,
volatility sig, tau]= [0.05, 0.20, 20/52], the price of the European call option is given.'

```


```r

# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

ReadConsoleInput = function(prompt.message, bounds) {
    input = NA
    while (!is.numeric(input)) {
        input = as.numeric(sub(",", ".", readline(prompt = prompt.message)))
        if (!missing(bounds)) {
            if (length(bounds) > 1) {
                if (input < bounds[1] | input > bounds[2]) {
                  input = NA
                  cat("Error: value must lie between", bounds[1], "and", bounds[2], 
                    "n")
                }
            } else {
                if (input < bounds[1]) {
                  input = NA
                  cat("Error: value must lie at or above", bounds[1], "n")
                }
            }
        }
    } 
    return(input)
}

# Set defaults
K   = 100
S   = 98
r   = 1/20
b   = 1/20
tau = 20/52
sig = 1/5

# Check whether user wants to use defaults
use.defaults = TRUE
cat("The default option parameters are:n")
cat("S =", S, "n")
cat("K =", K, "n")
cat("r =", r, "n")
cat("b =", b, "n")
cat("tau =", tau, "n")
cat("sig =", sig, "n")

while (TRUE) {
    user.input = readline("Would you like to use these default values (y/n)? ")
    if (!(user.input %in% c("y", "n"))) {
        cat("Invalid input: please use y or n to answer the question.n")
    } else {
        if (user.input == "y") {
            use.defaults = TRUE
        } else {
            use.defaults = FALSE
        }
        break
    }
}

if (use.defaults == FALSE) {
    S   = ReadConsoleInput("Please enter a stock price S: ", bounds = c(0))
    K   = ReadConsoleInput("Please enter a strike price K: ", bounds = c(0))
    r   = ReadConsoleInput("Please enter the risk free rate r: ", bounds = c(0))
    b   = ReadConsoleInput("Please enter the cost of carry b: ", bounds = c(-1, 1))
    sig = ReadConsoleInput("Please enter the stock price volatility sigma: ", bounds = c(0))
    tau = ReadConsoleInput("Please enter the time to maturity tau: ", bounds = c(0))
}

set.seed(0)

# Main computation
y = (log(S/K) + (b - (sig^2)/2) * tau)/(sig * sqrt(tau))
c = exp(-(r - b) * tau) * S * pnorm(y + sig * sqrt(tau)) - exp(-r * tau) * K * 
    pnorm(y)

# Output
cat("Price of the European Call: ")
cat(formatC(c, format = "f", digits = 4), "n")

```
