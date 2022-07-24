# based on: http://uc-r.github.io/ts_moving_averages
library(tidyverse)
library(lubridate)
install.packages("fpp2")
library(fpp2)
library(zoo)

# data
head(economics, 10)

# focus on personal savings rate (psavert)
# using mutate and rollmean to compute 13, 25, ..., 121 month moving average values and add this data back
# to the df 
#   Note: we need to explicitly state to fill any years that cannot be computed (due to lack of data) with NA

savings <- economics %>%
  select(date, srate = psavert)

head(savings, 10)
class(savings)

savings <- savings %>%
  mutate(srate_ma01 = rollmean(srate, k = 13, fill = NA),
         srate_ma02 = rollmean(srate, k = 25, fill = NA),
         srate_ma03 = rollmean(srate, k = 37, fill = NA),
         srate_ma05 = rollmean(srate, k = 61, fill = NA),
         srate_ma10 = rollmean(srate, k = 121, fill = NA))

savings        

# now we can go ahead and plot these values and compare the actual data to the different moving average smoothers
savings$metric

savings %>%
  gather(metric, value, srate:srate_ma10) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line()

# we can observe that the number of points used for the average correlates with a smoother curve
#> deciding on a value for k requires balance between eliminating noise and capturing the data's true structure
#> for this dataset, 10 year moving average (k = 121) eliminates most of the pattern (i.e., is probably too smooth)
#> while a 1 year moving average (k = 13) offers little more than just observing the data itself
#> we can see this by zooming into the 2000-2015 time range:
head(savings, 10)
savings %>%
  gather(metric, value, srate:srate_ma10) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line() +
  coord_cartesian(xlim = c(date("2000-01-01"), date("2015-04-01")), ylim = c(0, 11))

# to understand how these different moving averages compare, we can compute the MSE and MAPE
#> both error rates will increase as you choose a larger k to average over;
#> however, if you or leadership are indifferent between a 6-9% error rate, then you may want to
#> illustrate trends with a 3 year moving average rather than a 1 year moving average
?gather()
class(savings)
head(savings, 10)
savings %>%
  gather(metric, value, srate_ma01:srate_ma10) %>%
  group_by(metric) %>%
  summarise(MSE = mean((srate - value)^2, na.rm = T),
            MAPE = mean(abs((srate - value)/srate), na.rm = T))

savings1 <- savings %>%
  gather(metric, value, srate_ma01:srate_ma10)
savings1
# using the fpp2 package ----------------------------

# a simple moving average can also be plotted using autoplot() contained in "fpp2"
#>  helpful if data is already in a time series data object
#>  e.g., if our savings rate data were already converted to a time series object as here...
savings.ts <- economics %>%
  select(srate = psavert) %>%
  ts(start = c(1967, 7), frequency = 12)

head(savings.ts, 30)

# we can plot this data with autoplot
#>  here data is plotted in line 1 of the following code
#>  while moving average (calculated with ma()) is plotted in second layer
autoplot(savings.ts, series = "Data") +
  autolayer(ma(savings.ts, 13), series = "1 yr MA") +
  autolayer(ma(savings.ts, 61), series = "5 yr MA") +
  autolayer(ma(savings.ts, 121), series = "10 yr MA") +
  xlab("Date") +
  ylab("Savings Rate")

# forecasting with trailing moving averages --------------------------

# centered moving averages (as above) are computed by averaging across data both in the past and future
#>  they cannot be used for forecasting
#>  instead, forecasting requires trailing moving averages, where window of k periods is placed over the most recent
#>  available k values of the series
#>    e.g., if we have data up to time t, we can predict the value for t+1 by averaging
#>    over k periods prior to t+1

# if we wanted to predict next month's savings rate based on previous year's average,
#>  can use rollmean with the align = "right" argument for trailing moving average
#>  we can see that if we wanted to predict the savings rate for 2015-05-01 based on the last 12 months
#>  our prediction would be 5.06% (12 month average for 2015-04-01)
#>  this is similar to naive forecasting, but with an averaged value rather than most recent actual value
savings_tma <- economics %>%
  select(date, srate = psavert) %>%
  mutate(srate_tma = rollmean(srate, k = 12, fill = NA, align = "right"))

# can visualize how the 12-month trailing moving average predicts future savings rate in the following plot
#>  it's easy to see that trailing moving averages have a delayed reaction to changes in patterns and trends
savings_tma %>%
  gather(metric, value, -date) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line()

# moving averages of moving averages ----------------------------

# concept of simple moving averages can be extended to taking the moving average of moving averages
#>  this technique is often employed with an even number of data points so the final product is
#>  symmetric around each point
#>    e.g., let's look at the built-in dataset electsales in fpp2 package
#>    for our first example, we convert to a df
#>    this df is even numbered (20 rows)

# convert to df
elecsales.df <- data.frame(year = time(elecsales), sales = elecsales)

nrow(elecsales.df)

# an even-numbered moving average is unbalanced
#>  for our purposes the unbalancing will be in favor of more recent observations
#>    e.g., to calculate a 4-MA, the equation is as follows:
#>        yhat = (yt-1 + yt + yt+1 + yt+2)/4

# to make moving average symmetric (and more accurate as a result) we take a 2-MA of the 4-MA to create a 2x4-MA
#>  for this, we average the current and previous moving averages, thus resulting in an estimate of:
#>      yhat = 1/8(yt-2) + 1/4(yt-1) + 1/4(yt) + 1/4(yt+1) + 1/8(yt+2)

# this two-step process can be easily done with the ma function by setting order = 4 and centre = TRUE
elecsales.df %>%
  mutate(ma4 = ma(sales, order = 4, centre = TRUE)) %>%
  head()

# to compare this moving average to regular moving average, we plot the two outputs:

# compute 2 and 2x4 moving averages
elecsales.df %>%
  mutate(ma2 = rollmean(sales, k = 2, fill = NA),
         ma2x4 = ma(sales, order = 4, centre = TRUE)) %>%
  gather(ma, value, ma2:ma2x4) %>%
  ggplot(aes(x = year)) +
  geom_point(aes(y = sales)) +
  geom_line(aes(y = value, color = ma))

# this 2x4-MA process produces the best fit so far
#>  it massages out some of the noise, while maintaining the data trend with reasonable fidelity
#>  other combinations of moving averages are possible, such as 3x3
#>  to maintain symmetry, if your first moving average is an even number of points, the follow-up MA should also
#>  contain an even number.
#>  likewise, if first MA uses odd number of points, so should the follow-up
#>  keep in mind that moving averages of moving averages will lose information
#>  because you do not retain as many data points




# using the fpp2 package (moving average of moving average) --------------------------

# if your data is already in a time series data object, you can apply the ma function directly to that object with
#>  order = 4 and centre = TRUE
#>      e.g., the built-in elecsales dataset is a time series object:
class(elecsales)

# we can compute the 2x4 moving average directly:
ma(elecsales, order = 4, centre = 4)

# we can use autoplot to plot the 2x4 moving average against the raw data:
autoplot(elecsales, series = "Data") +
  autolayer(ma(elecsales, order = 4, centre = TRUE), series = "2x4-MA") +
  labs(x = "Year", y = "Sales") +
  ggtitle("Annual electricity sales: South Australia")




# Weighted moving Averages -----------------------------

# a moving average of a moving average can be thought of as a symmetric MA with different weights on each nearby observation
#>  e.g., the 2x4-MA discussed above is equivalent to a weighted 5-MA with weights given by [1/8, 1/4, 1/4, 1/4, 1/8]
#>  in general, a weighted m_MA can be written as That of t = sum of ajyt+j
#>      where k = (m - 1)/2 and the weights are given by [a-k, ..., ak]
#>  it is important that the weights all sum to one and they are symmetric so that aj = a-j
#>    this simple m-MA is a special case where all the weights are equal to 1/m
#>  A major advantage of weighted MAs is that they yield a smoother estimate of the trend cycle
#>    instead of observations entering and leaving the calculation at full weights, their weights are
#>    slowly increased and then slowly decreased (thus producing a smoother curve)
#>      Some specific sets of weights are widely used, such as the following: [see website for table]

#> for example, the AirPassengers data contains an entry for every month in a 12 year span, so a time period would consist of 12 time units
#>  a 2 x 12-MA set-up is the preferred method for such data
#>  the observation itself, as well as the 5 observations immediately before and after it, receives weight 1/12 = 0.083
#>  while the data point for that month last year and that month the following year both receive weight 1/24 = 0.042

# we can produce this weighted moving average using the ma function like last section
ma(AirPassengers, order = 12, centre = TRUE)

#and to compare this MA to the actual time series:
autoplot(AirPassengers, series = "Data") +
  autolayer(ma(AirPassengers, order = 12, centre = T), series = "2x12-MA") +
  ggtitle("Monthly Airline Passengers (1949-1960)") +
  labs(x = NULL, y = "Passengers")
# can see we've smoothed out seasonality, but captured the overall trend
#> interesting note to self: the humps are what seem to indicate seasonality ************************

# Exercises -------------------------
# Using the economics dataset provided by ggplot2:


# 1. ----------------------------
# compute and plot the 1, 3, and 5 year moving average for the personal consumption expenditures

head(economics, 10)

?ts

pce <- economics %>%
  select(date, expenditure = pce)
pce

pce <- pce %>%
  mutate(
    expenditure.ma01 = rollmean(expenditure, k = 13, fill = NA), # 1 year moving average (k = 12 months + 1)
    expenditure.ma03 = rollmean(expenditure, k = 37, fill = NA), # 3 year MA using same k formula
    expenditure.ma05 = rollmean(expenditure, k = 61, fill = NA)  # 5 year MA using similar formula
  )

head(pce, 10)


pce %>%
  gather(metric, value, expenditure:expenditure.ma05) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line()



# that just doesn't look right to me... the lines are way too close together
# part of me suspects the weird date units
# (update) the reason it's difficult to see difference in trends is because the y range is so large (i.e., c(500, 12500)) compared to
# the savings y range

# reference 1 (Normal plot)
#:::::::::::::::::::::::::::
savings %>%
  gather(metric, value, srate:srate_ma10) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line()

# reference 2 (Zooming in)
#:::::::::::::::::::::::::::
savings %>%
  gather(metric, value, srate:srate_ma10) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line() +
  coord_cartesian(xlim = c(date("2000-01-01"), date("2015-04-01")), ylim = c(0, 11))

# Expenditures (present example) Zooming in (attempt)
#::::::::::::::::::::::::::
pce %>%
  gather(metric, value, expenditure:expenditure.ma05) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line() +
  coord_cartesian(xlim = c(date("2000-01-01"), date("2015-04-01")), ylim = c(500, 12500))

view(savings)
view(pce)

# 2. ------------------------------------
#> compute the mean square error of these moving averages

# reference (savings)
#:::::::::::::::::::::::::::::::
savings %>%
  gather(metric, value, srate_ma01:srate_ma10) %>%
  group_by(metric) %>%
  summarise(MSE = mean((srate - value)^2, na.rm = T),
            MAPE = mean(abs((srate - value)/srate), na.rm = T))

# expenditures (present example)
#:::::::::::::::::::::::::::::::
pce %>%
  gather(metric, value, expenditure.ma01:expenditure.ma05) %>%
  group_by(metric) %>%
  summarise(MSE = mean((expenditure - value)^2, na.rm = T),
            MAPE = mean(abs((expenditure - value)/expenditure), na.rm = T))

# 3. -------------------------------------
#>  forecast the personal consumption expenditure for 2015-05-01 using a 1, 3, and 5 year trailing moving average

# reference (savings)
#:::::::::::::::::::::::::::::::
# if we wanted to predict next month's savings rate based on previous year's average,
#>  can use rollmean with the align = "right" argument for trailing moving average
#>  we can see that if we wanted to predict the savings rate for 2015-05-01 based on the last 12 months
#>  our prediction would be 5.06% (12 month average for 2015-04-01)
#>  this is similar to naive forecasting, but with an averaged value rather than most recent actual value
savings_tma <- economics %>%
  select(date, srate = psavert) %>%
  mutate(srate_tma = rollmean(srate, k = 12, fill = NA, align = "right"))

# expenditures (present example)
#::::::::::::::::::::::::::::::
expenditures_tma <- economics %>%
  select(date, pce) %>%
  mutate(
    pce_tma01 = rollmean(pce, k = 12, fill = NA, align = "right"),
    pce_tma03 = rollmean(pce, k = 36, fill = NA, align = "right"),
    pce_tma05 = rollmean(pce, k = 60, fill = NA, align = "right"))

# reference (savings)
#::::::::::::::::::::::::::::::
# can visualize how the 12-month trailing moving average predicts future savings rate in the following plot
#>  it's easy to see that trailing moving averages have a delayed reaction to changes in patterns and trends
savings_tma %>%
  gather(metric, value, -date) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line()

# expenditures (present example)
#::::::::::::::::::::::::::::::
expenditures_tma %>%
  gather(metric, value, -date) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line()


# 4. -------------------------------------
#> compute and plot a 2x12 weighted smoothing average


# reference (savings)
#:::::::::::::::::::::::::::::::::::::::::
#> for example, the AirPassengers data contains an entry for every month in a 12 year span, so a time period would consist of 12 time units
#>  a 2 x 12-MA set-up is the preferred method for such data
#>  the observation itself, as well as the 5 observations immediately before and after it, receives weight 1/12 = 0.083
#>  while the data point for that month last year and that month the following year both receive weight 1/24 = 0.042

# we can produce this weighted moving average using the ma function like last section
ma(AirPassengers, order = 12, centre = TRUE)

#and to compare this MA to the actual time series:
autoplot(AirPassengers, series = "Data") +
  autolayer(ma(AirPassengers, order = 12, centre = T), series = "2x12-MA") +
  ggtitle("Monthly Airline Passengers (1949-1960)") +
  labs(x = NULL, y = "Passengers")

# expenditures (present example)
#::::::::::::::::::::::::::::::::::::::::
# ***convert df/tibble to ts

# read in pce as a fresh dataset

pce <- economics %>%
  select(date, pce)

pce <- as.ts(pce)

ma(pce, order = 12, centre = TRUE)

# compare this MA to actual ts:
autoplot(pce, series = "Data") +
  autolayer(ma(pce, order = 12, centre = T), series = "2x12-MA") +
  ggtitle("Monthly Expenditures") +
  labs(x = NULL, y = "Expenditures")


