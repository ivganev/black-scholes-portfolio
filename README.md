# Black-Scholes-Merton Greeks for a Portfolio

This code allows one to compute the greeks of a portfolio based on the Black-Scholes-Merton pricing formula. It also produces plots for each of the greeks as the price of the underlying changes. 

### Structs

The struct ```Portfolio``` encodes the relevant information of the portfolio. This includes the spot price and volatility of the underlying asset, the number of contracts in the underlying asset, the interest rate, and a list of options. 

Each option is an instance of the struct ```Opt``` which includes the kind of option (call or put), strike price, quantity, and time to expiration. 

### Portfolio methods

The portfolio method ```.summary``` prints the content of the portfolio. 

The portfolio method ```.greeks``` computes the greeks of the portfolio, namely the current theoretical price, the delta, the gamma, the theta, the vega, and the rho. 

Finally, the portfolio method ```.plot_over_underlying``` produces a chart of how a chosen greek changes as the price of the underlying changes within three standard deviations of the current price. For example, here is the chart of the change in gamma:

![alt text](https://github.com/ivganev/black-scholes-portfolio//blob/main/src/figs/gamma.png?raw=true)
