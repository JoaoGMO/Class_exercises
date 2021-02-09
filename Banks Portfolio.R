#     Recreating a classroom exercise
#     
#     Build a portfolio with three banks given the monetary positions
#     (plus some extra analysis)
#
#     Banks: Bradesco, Banco do Brasil, Itaú Unibanco
# 

library(quantmod)
library(ggplot2)
library(PortfolioAnalytics) 
library(corrplot) 

#The one month period and the banks were determined by the prof

banks_tickers <- c("BBDC3.SA", "BBAS3.SA", "ITUB3.SA")
getSymbols(banks_tickers, auto.assign = TRUE, from = "2017-07-31",
           to = "2017-08-31")




#Taking a glance on the data before starting the exercise

colnames(BBAS3.SA)
colnames(BBDC3.SA)
colnames(ITUB3.SA)

new_names <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(BBAS3.SA) <- new_names
colnames(BBDC3.SA) <- new_names
colnames(ITUB3.SA) <- new_names

#   I want only the adjusted price, since banks in Brazil are dividend 
# companies it may have a significant distortion in closing price compared
# to it's real value. This plot will help to visualize the difference.

ggplot(BBAS3.SA, aes(x = index(BBAS3.SA)))+
  geom_line(aes(y = BBAS3.SA$Close), color = "black")+
  geom_line(aes(y = BBAS3.SA$Adjusted), color = "blue")+
  labs(title = "Close X Adjusted Price \nBanco do Brasil", x = "Time",
       y = "Price (R$)") + theme_light()

#   Now it's time to create a xts object only with adjusted prices.
# This task would require to move the stocks into a list and loop over it to
# gather only the "Adjusted" columns. Since I'm dealing only with 3 companies
# (thanks prof), the job will be much easier.

(banks_price <- xts(order.by = index(BBAS3.SA)))

banks_price <- merge(banks_price, BBAS3.SA$Adjusted)
banks_price <- merge(banks_price, BBDC3.SA$Adjusted)
banks_price <- merge(banks_price, ITUB3.SA$Adjusted)

head(banks_price)

colnames(banks_price) <- c("Brasil", "Bradesco", "Itau")
head(banks_price)

summary(banks_price)

ggplot(banks_price, aes(x = index(banks_price)))+
  geom_line(aes(y = Brasil, col = "Brasil"))+
  geom_line(aes(y = Bradesco, col = "Bradesco"))+
  geom_line(aes(y = Itau, col = "Itau"))+
  labs(title = "Prices", x = "Time", y = "Prices (R$)")+ theme_light()

#   After have seen some statistics, ensure that there are no NA's in the
# sample and plotting the price evolution, the next step is to calculate the
# returns in order to continue the portfolio analysis. It will be calculated in 
# ways: with and without Return.calculate() function.

# 1) Without Return.calculate():

no_func_prices <- as.data.frame(banks_price)
l_no_func_prices <- log(no_func_prices)
return_no_func <- data.frame(Brasil = NA, Bradesco = NA, Itau = NA)
for (i in 2:nrow(l_no_func_prices)) {
  return_no_func[i,] <- l_no_func_prices[i,]-l_no_func_prices[i-1,]  
}

head(return_no_func)

return_no_func <- as.xts(return_no_func, order.by = index(BBAS3.SA))
head(return_no_func)

#   This leads us with the full table of log-returns. It took me 9 lines 
# of code (certainly there may be others optimal ways to perform 
# what I'm proposing). 

# 2) With Return.calculate() from Portfolio.Analytics

pt_returns <- Return.calculate(banks_price, method = "discrete")
head(pt_returns)


#   Now it took only 2 lines, 1 to create a variable and calculate the
# returns and other to verify the results. The difference on the results
# is due to the method argument in the function that calculates the returns
# in a descrete way (R = Pt/Pt-1 - 1). Either way, it was a minimum and
# tolerable difference.

#   Moving forward with the analysis, it's time to look at characteristics
# of the return data collected.

summary(pt_returns)

# Looking for outliers
boxplot(coredata(pt_returns), col = "white", border = "black",
        ylim = c(-.05,.05), xlab = "Name", ylab = "Return")

# Plotting the same price graph with the vertical line in the day that
# the outlier occurred 
pt_returns[which.max(pt_returns$Brasil), ]
ggplot(banks_price, aes(x = index(banks_price)))+
  geom_line(aes(y = Brasil, col = "Brasil"))+
  geom_line(aes(y = Bradesco, col = "Bradesco"))+
  geom_line(aes(y = Itau, col = "Itau"))+
  geom_vline(xintercept = as.Date("2017-08-22"))+
  labs(title = "Greatest Return BB", x = "Time", y = "Prices (R$)")+
  theme_light()

# Checking out the distribution of returns
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

#   After Observing the plots of density it's quite conclusive that it may
# be difficult to approximate the curves to a Normal (or any other) dist.

#   Next step is to calculate the allocations with other information given in
# class. Also, it'd be nice to check the diversification level (which is low,
# since all assets are pretty much similar).


positions <- c("Banco do Brasil" = 4197500, 
               "Bradesco" = 4605000,
               "Itaú" = 5245500)


par(mfrow = c(1,2))
corrplot(cor(pt_returns[-1]), method = "number", type = "lower")
corrplot(cor(pt_returns[-1]), method = "square", type = "upper")


BBAS3 <- pt_returns[,1]*positions[1]
BBDC3 <- pt_returns[,2]*positions[2]
ITUB3 <- pt_returns[,3]*positions[3]

bank_portfolio <- BBAS3 + BBDC3 + ITUB3
colnames(bank_portfolio) <- "Banks"

(pt_stats <- c("Mean" = mean(bank_portfolio, na.rm = TRUE),
              "Volatility" = sd(bank_portfolio, na.rm = TRUE)))


