# Banks Portfolio

This is an exercise provided by my Finance professor in the 6th period. It consisted in calculate the mean and volatility of an arbitraty portfolio with 3 stock from Brazil's stock market: Banco do Brasil, Banco Bradesco, Banco Itau Unibanco. These are quite big and expressive banks in the IBovespa (BVSP Index) so it is just a methodic exercise, with no investment insights or recomendations.

To discuss some observations from the analysis we can clearly see that there are no sound of diversification in this portfolio, as imagined. Due to the size of the sample (it contains daily prices from July 31, 2017 to August 31, 2017) it's not statistically recommended to assume a Normal distribution, and, by observing the histograms and density curves I spare possible test to ensure the (non) normality.

- Some tests that check the normality of the sample are the kurtosis and skewness analysis and the Shapiro-Wilk hypotesis test.

Continuing the discussion, one important aspect that deserves the attention is that banks are dividends companies, which means that they commonly cause prices distortions because of dividend yield. quantmod::getSymbols provides us the OHLC (Open, High, Low, Close) prices with the transaction volume and the adjusted prices. The last mentioned is already corrected for dividend yields, splits and inplits and any other possible price distortion. In the 1st plot on the R Script it's possible to check the difference in Close x Adjusted prices of Banco do Brasil. For the analysis it was used the Adjusted price of all 3 banks.

Last but not Least, is nice to comment the two approaches to calculate the return of stocks: by log and discretly. Both methods give us the numeric return with just a little difference. In this exercise I desconsidered this difference that occured in the 4th or 5th decimal
- Log return: It is quite common in time series to use LN to smooth the time-varying variance of the data. The financial approach of this methodology is due to Log facilitations (yes, it does facilitate a lot) in use the subtraction property tha makes Log(A) - Log(B) = Log(A/B). Using the current price and the last one, we can calculate the periodic return of the asset

- Discrete Return: It is the most intuitive way to calculate variation of data in certain period. We just divide the difference of last and first observation by the value of the first observation such as (A - B)/B; another way to look at this is A/B - 1. 


