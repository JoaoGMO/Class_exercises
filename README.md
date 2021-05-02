
# Class exercise - Bank Portfolio

> Calculate the Mean/Volatility of the porfolio composed by three bank stock prices in one-month period using the historical scenario mothodology. The period and the monetary position in each stock were given.  
**Banks**: Bradesco, Banco do Brasil, Itaú Unibanco


```{r, message=FALSE, warning=FALSE}
library(quantmod)
library(ggplot2)
library(PortfolioAnalytics) 
library(corrplot) 
library(rmarkdown)
```


 - Packages that will be used in the exercise.
 
All three stock will be downloaded from `quantomod`'s with `getSymbols()` which uses the Yahoo! Finance  to obtain the OHLC (Open, High, Low, Close) prices plus the volume and the adjusted price. The one month period and the banks were determined by the Prof.

```{r, include = FALSE}
banks_tickers <- c("BBDC3.SA", "BBAS3.SA", "ITUB3.SA")
getSymbols(banks_tickers, auto.assign = TRUE, from = "2017-07-31",
           to = "2017-08-31")

new_names <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(BBAS3.SA) <- new_names
colnames(BBDC3.SA) <- new_names
colnames(ITUB3.SA) <- new_names
```

- Here we can take a look at the format of the data `getSymbols()` returns. The columns were renamed in order to keep simple the forward analysis.


```{r, echo = FALSE, message=FALSE, warning=FALSE}
paged_table(as.data.frame(BBAS3.SA))
```
 
I want only the adjusted price, since banks in Brazil are dividend companies and it may have a significant distortion in closing price compared to it's real value. This plot will help to visualize the difference.  

```{r, message=FALSE, warning=FALSE}
ggplot(BBAS3.SA, aes(x = index(BBAS3.SA)))+
  geom_line(aes(y = BBAS3.SA$Close), color = "black")+
  geom_line(aes(y = BBAS3.SA$Adjusted), color = "blue")+
  labs(title = "Close X Adjusted Price \nBanco do Brasil", x = "Time",
       y = "Price (R$)") + theme_light()
```
  
  
Now it's time to create a xts object only with adjusted prices.
This task would require to move the stocks into a list and loop over it to
gather only the "Adjusted" columns. Since I'm dealing only with 3 companies
(thanks prof), the job will be much easier. These are the statistics of the merged xts object.  
```{r, include = FALSE} 

(banks_price <- xts(order.by = index(BBAS3.SA)))

banks_price <- merge(banks_price, BBAS3.SA$Adjusted)
banks_price <- merge(banks_price, BBDC3.SA$Adjusted)
banks_price <- merge(banks_price, ITUB3.SA$Adjusted)

head(banks_price)

colnames(banks_price) <- c("Brasil", "Bradesco", "Itau")
```

```{r}
summary(banks_price)
```

```{r, echo=FALSE} 
ggplot(banks_price, aes(x = index(banks_price)))+
  geom_line(aes(y = Brasil, col = "Brasil"))+
  geom_line(aes(y = Bradesco, col = "Bradesco"))+
  geom_line(aes(y = Itau, col = "Itau"))+
  labs(title = "Prices", x = "Time", y = "Prices (R$)")+ theme_light()
```

  
  After have seen some statistics, ensure that there are no NA's in the sample and plotting the price evolution, the next step is to calculate the returns in order to continue the portfolio analysis. It will be calculated in 2 ways: with and without `Return.calculate()` function.[^1]  

  [^1]: In fact, there are others ways to perform this calculation with less code lines, one is already in the code script version of this file and, right now, I can think in the matrix notation that could solve this exercise in a much easier way. I'll update these approaches or perform them in another exercises.
  
  - Without `Return.Calculate()`:  
```{r}
no_func_prices <- as.data.frame(banks_price)
l_no_func_prices <- log(no_func_prices)
return_no_func <- data.frame(Brasil = NA, Bradesco = NA, Itau = NA)
for (i in 2:nrow(l_no_func_prices)) {
  return_no_func[i,] <- l_no_func_prices[i,]-l_no_func_prices[i-1,]  
}
return_no_func <- as.xts(return_no_func, order.by = index(BBAS3.SA))
```

```{r, message = FALSE, warning=FALSE}
head(return_no_func, n = 5)
```
  
  - With `Return.Calculate()`:  
```{r}
pt_returns <- Return.calculate(banks_price, method = "discrete")
head(pt_returns, n = 5)
```
  
    
Here there is a nice consideration to talk about: the calculation method.

- In the second point I picked the discrete calculation that follows the equation  
$$R_t = \frac{P_t}{P_{t-1}} - 1$$  
where R is the return and P is the price, the subscription denotes the period of time such as *t* is the present and *t-1* is the period before.  
  
- Other approach with the same goal is to find the returns in a Log way which is very common in time series to reduce the time-varying variance of series. It's also useful when thinking that prices (X variable) cannot be negative. This follows the equation  
$$R_t = \log(\frac{P_t}{P_{t-1}}) $$  
where all variables are the same as before and log is the logarithm in e base.  

The results shown were very similar, with the differences in the 4th decimal case or even after it. Both of these techniques are practiced in the literature and, in the original exercise, the class had to perform the discrete method. Due to this, I will precede with this methodology for the next steps.  
  

### Sample Distribution

- Here it's time to check some characteristics of the returns distribution in the three stocks. This will be done by plotting a Boxplot to verify any outlier occurrence (if so, let's check where in the Price graph it occurs) and latter by plotting the histogram and the density distribution line.  

```{r, echo=FALSE}
boxplot(coredata(pt_returns), col = "white", border = "black",
        ylim = c(-.05,.05), xlab = "Name", ylab = "Return")
```
  
     
   As we can see, only "Brasil" shows two outliers in the upper side of the box (represented by two circles). The maximum return occurred in 08/22/2017 when the price went up almost 4,5%.
   
```{r, echo=FALSE}
pt_returns[which.max(pt_returns$Brasil), ]
ggplot(banks_price, aes(x = index(banks_price)))+
  geom_line(aes(y = Brasil, col = "Brasil"))+
  geom_line(aes(y = Bradesco, col = "Bradesco"))+
  geom_line(aes(y = Itau, col = "Itau"))+
  geom_vline(xintercept = as.Date("2017-08-22"))+
  labs(title = "Greatest Return BB", x = "Time", y = "Prices (R$)")+
  theme_light()
```
  
  After have seen these representation we can try to approximate the return distribution to any standard distribution (such as the Normal, *t-student*, chi squared, etc...) but, as we will see, due to the lack of observations it may be quite difficult. The following plots encourage me to don't even perform a hypotheses test or print kurtosis/skewness of the dists.
  
```{r, echo=FALSE}
par(mfrow = c(1,3))
hist(pt_returns$Brasil, prob = TRUE, xlab = "", main = "Banco do Brasil", 
     breaks = 10, col = "lightblue")
lines(density(pt_returns$Brasil, na.rm = TRUE), col = "red", lwd = 2)

hist(pt_returns$Bradesco, prob = TRUE, xlab = "", main = "Bradesco", 
     breaks = 10, col = "lightblue")
lines(density(pt_returns$Bradesco, na.rm = TRUE), col = "red", lwd = 2)

hist(pt_returns$Itau, prob = TRUE, xlab = "", main = "Itaú", 
     breaks = 10, col = "lightblue")
lines(density(pt_returns$Itau, na.rm = TRUE), col = "red", lwd = 2)
```
  
  
### Correlation & Conclusion
  
  
  The last step in this simple exercise is to check if there is any sign of diversification in the portfolio but, since all three stocks represent three expressive banks in the Brazilian scenario (in the BVSP and in the whole market), it may not occur. My initial guess was that all 3 correlations would be in the [0.4 ; 0.6] interval.  
  
  Also, the monetary position in each stock is given to perform the historical scenario of the return.  
  

```{r, echo=FALSE}
par(mfrow = c(1,2))
corrplot(cor(pt_returns[-1]), method = "number", type = "lower")
corrplot(cor(pt_returns[-1]), method = "square", type = "upper")
```


```{r}
positions <- c("Banco do Brasil" = 4197500, 
               "Bradesco" = 4605000,
               "Itaú" = 5245500)

BBAS3 <- pt_returns[,1]*positions[1]
BBDC3 <- pt_returns[,2]*positions[2]
ITUB3 <- pt_returns[,3]*positions[3]

```

```{r}
bank_portfolio <- BBAS3 + BBDC3 + ITUB3
(pt_stats <- c("Mean" = mean(bank_portfolio, na.rm = TRUE),
               "Volatility" = sd(bank_portfolio, na.rm = TRUE)))
```


### Summing Up

  Here is a glance of what I did in this document: 
  
- Given the stock prices in the one month period and how the money was allocated in these three, the class had to calculate the mean and volatility of the portfolio using the historical scenario routine.  
- I took one step further in this and calculated other measures that may be additional information to the portfolio such as the correlation among the stocks, the distributions and the outlier occurrences.  
- As commented, the analysis could get much more sophisticated by performing other kinds of calculation, plotting some other graphics, backtesting the portfolio performance and rebalance the asset weights. I will do this in other opportunity with other stocks of Ibovespa.
