library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(plotly)
library(DT)
library(scales)
library(tidyr)
library(tseries)
library(ggcorrplot)



# Load the dataset
df <- read_excel("C:/Users/pslbh/OneDrive/Documents/College Work/Minor Degree project (PA)/Trafic metrics.xlsx")

# Preview the data
View(df)
head(df)
str(df)

# Rename columns for easier use
colnames(df) <- c(
  "slice_id", "timestamp", "device_id", "traffic_load",
  "traffic_type", "network_utilization", "latency",
  "packet_loss", "signal_strength", "bandwidth_utilization",
  "slice_failure", "qos_throughput", "overload_status",
  "device_type", "region", "failure_count", "time_of_day",
  "weather"
)

# Fix timestamp column
df$timestamp <- as.POSIXct(df$timestamp)
df$hour      <- hour(df$timestamp)
df$date      <- as.Date(df$timestamp)

# Create Congestion Score (composite 0-100 index)
df$congestion_score <- with(df, (
  rescale(latency,               to = c(0, 30)) +
    rescale(packet_loss,           to = c(0, 25)) +
    rescale(bandwidth_utilization, to = c(0, 25)) +
    rescale(network_utilization,   to = c(0, 20))
))

# Confirm it worked
summary(df$congestion_score)
nrow(df)

# ── Graph 1: Overload Status Count ──
ggplot(df, aes(x = factor(overload_status,
                          labels = c("Normal", "Congested")),
               fill = factor(overload_status))) +
  geom_bar(width = 0.5) +
  scale_fill_manual(values = c("0" = "#00C853", "1" = "#FF4444")) +
  labs(title = "Network Overload Status Distribution",
       x = "Status", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# ── Graph 2: Congestion Score Over Time ──
ggplot(df, aes(x = date, y = congestion_score)) +
  geom_line(color = "#00B4D8", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "#FF4444", se = FALSE) +
  labs(title = "Congestion Score Over Time",
       x = "Date", y = "Congestion Score (0-100)") +
  theme_minimal()

# ── Graph 3: Boxplot by Time of Day ──
ggplot(df, aes(x = factor(time_of_day,
                          labels = c("Night", "Day", "Evening")),
               y = congestion_score,
               fill = factor(time_of_day))) +
  geom_boxplot() +
  labs(title = "Congestion Score by Time of Day",
       x = "Time of Day", y = "Congestion Score") +
  theme_minimal() +
  theme(legend.position = "none")

# ── Graph 4: Correlation Heatmap ──
num_vars <- df[, c("traffic_load", "network_utilization", "latency",
                   "packet_loss", "bandwidth_utilization",
                   "qos_throughput", "overload_status")]
corr_matrix <- cor(num_vars, use = "complete.obs")
ggcorrplot(corr_matrix, lab = TRUE, lab_size = 3,
           colors = c("#0077B6", "white", "#FF4444"),
           title = "Metric Correlation Heatmap")

# Create a time series object (hourly data, 24 hours = 1 day cycle)
ts_congestion <- ts(df$congestion_score, frequency = 24)

# ── Graph 5: Decomposition (Trend + Seasonality + Residuals) ──
decomp <- stl(ts_congestion, s.window = "periodic")
plot(decomp,
     main = "Time Series Decomposition of Congestion Score")

# ── Graph 6: Autocorrelation (ACF) ──
acf(ts_congestion, lag.max = 48,
    main = "ACF — Congestion Score",
    col  = "#00B4D8", lwd = 2)

# ── Graph 7: Partial Autocorrelation (PACF) ──
pacf(ts_congestion, lag.max = 48,
     main = "PACF — Congestion Score",
     col  = "#FF4444", lwd = 2)

# ── Stationarity Test (ADF) ──
adf_result <- adf.test(ts_congestion)
print(adf_result)

# ── Fit ARIMA model automatically ──
arima_model <- auto.arima(ts_congestion,
                          seasonal    = TRUE,
                          stepwise    = FALSE,
                          approximation = FALSE)

# Print model summary
summary(arima_model)

# ── Forecast next 24 hours ──
arima_forecast <- forecast(arima_model, h = 24, level = c(80, 95))

# ── Graph 8: Forecast Plot ──
plot(arima_forecast,
     main   = "ARIMA Forecast — Congestion Score (Next 24 Hours)",
     xlab   = "Time (Hours)",
     ylab   = "Congestion Score",
     col    = "#00B4D8",
     fcol   = "#FF4444",
     shadecols = c("#FFE0E0", "#FFCCCC"))

# ── Graph 9: Residual Diagnostics ──
checkresiduals(arima_model)

# ── Accuracy Metrics ──
accuracy(arima_forecast)