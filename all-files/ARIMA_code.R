# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)
library(scales)

ts_data <- as.numeric(co_data$Oats[62:157])
#length(co_data$Wheat)-12
#ts_data <- co_data$Wheat[1:157]

# Step 1: Convert numeric vector to time series
ts_series <- ts(ts_data, start = c(2015, 8), frequency = 12)  # adjust as needed

# Step 2: Plot the time series
autoplot(ts_series) +
  ggtitle("Original Time Series") +
  xlab("Year") +
  ylab("Value")

# Step 3: Check for stationarity - Augmented Dickey-Fuller test
adf_test <- adf.test(ts_series)
print(adf_test)

# If not stationary, difference the series
ts_diff <- diff(ts_series)

# Plot differenced series
autoplot(ts_diff) +
  ggtitle("Differenced Series (1st Order)") +
  xlab("Year") +
  ylab("Differenced Value")

# Step 4: Check ACF and PACF
par(mfrow = c(1,2))
acf(ts_series, main="ACF of Differenced Series")
pacf(ts_series, main="PACF of Differenced Series")
par(mfrow = c(1,1))

# Step 5: Based on ACF/PACF, choose ARIMA(p,d,q)
# For example, let's assume you see PACF cuts off after lag 1 (AR(1)), and ACF tails off → ARIMA(1,1,0)

#model_manual <- arima(ts_series, order = c(1,0,0))

model_auto <- auto.arima(ts_series)
summary(model_auto)

# Step 6: Forecast 12 periods ahead
forecast_12 <- forecast(model_auto, h = 12)

# Step 7: Plot the forecast
autoplot(forecast_12) +
  ggtitle("ARIMA Forecast - Wheat") +
  xlab("Year") +
  ylab("Value") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2010:2024) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line()
  )

checkresiduals(model_auto)

forecast_table <- data.frame(
  Time = as.numeric(time(forecast_12$mean)),  # Time index for forecasted values
  Point_Forecast = forecast_12$mean           # Point forecasts
)

model_auto$arma

p <- model_auto$arma[1]
d <- model_auto$arma[6]
P <- model_auto$arma[3]
D <- model_auto$arma[4]
Q <- model_auto$arma[5]
s <- model_auto$arma[7]

# Now set q = 3 (your custom change)
q <- 3

# Check if a constant can be included
include_constant <- (d + D) <= 1

# Fit model safely
model_custom <- Arima(ts_data,
                      order = c(p, d, q),
                      seasonal = list(order = c(P, D, Q), period = s),
                      include.constant = include_constant)

summary(model_custom)

checkresiduals(model_custom)

# Step 6: Forecast 12 periods ahead
forecast_12 <- forecast(model_custom, h = 16)

# Step 7: Plot the forecast
autoplot(forecast_12) +
  ggtitle("ARIMA Forecast - Wheat") +
  xlab("Year") +
  ylab("Value") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2010:2026) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line()
  )
