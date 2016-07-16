HFT, A high-frequency trading simulation system in R.
=======
The goal of HFT is to make it easy to write and test high-frequency trading strategies.

This package provides a simulated environment with most of the realworld operating rules. It also offers varies of order submitting, order flow tracking and summarization functions to simplify strategy writting process. After the simualtion, all intermediate information can be easily fethed and analyzed.

##### Installation
```R
library(devtools)
install_github("chenhaotian/High-Frequency-Trading-Simulation-System")
```
##### Getting Started
Let's first show how the simulator works by a demo market-making strategy:

Assume a market-maker believe that the order flow of 6/16 treasury future contract(TF1606) has a strong positive auto-correlation. To avoid risk exposure in his positions, the market maker plan to place a buy/sell limit order at the bid1/ask1 price whenever there is a relatively large seller/buyer initiated transaction just been made, and cancel the order if it hasn't been executed for 10 seconds. The strategy should be written as follow:

```R
## a unrealistic demo strategy
demostg <- function(){

}
```