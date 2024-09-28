# Load necessary libraries
library(ggplot2)
library(lubridate)
library(dplyr)

# Set directories and file list
dataDirectory <- "./AllStationsDailySalTemp_20240723"
file_list <- list.files(path = dataDirectory, pattern = "*Temp.txt", full.names = TRUE)

titleStr <- "Sea Surface Temperature Anomalies at BC Lighthouses"
startYear <- 1935
stopYear <- 2023
climstartYear <- 1935
climstopYear <- 2023
nYears <- stopYear - startYear
nclimYears <- climstopYear - climstartYear + 1

# Initialize an array to store year and annual anomalies for each station for years
arrayAll <- matrix(NA, nrow = length(file_list), ncol = nYears + 1)

for (nstn in 1:length(file_list)) {

  file_name <- file_list[nstn]
  data <- read.table(file_name, skip = 4, header = TRUE)
  names(data) <- c("Year", "Month", "Day", "Temp", "Sal")
  year <- data$Year
  month <- data$Month
  day <- data$Day
  temp <- data$Temp
  sal <- data$Sal

  temp[temp > 40] <- NA
  sal[sal > 50] <- NA

  dtg <- ymd(paste(year, month, day, sep = "-"))

  if (dtg[1] > ymd(paste(startYear, "01", "01", sep = "-"))) {
    ndays <- as.numeric(difftime(dtg[1], ymd(paste(startYear, "01", "01", sep = "-")), units = "days"))
    dtgSupp <- seq.Date(ymd(paste(startYear, "01", "01", sep = "-")), by = "day", length.out = ndays)
    tempSupp <- rep(NA, ndays)
    temp <- c(tempSupp, temp)
    dtg <- c(dtgSupp, dtg)
  } else {
    qq <- which(dtg > ymd(paste(startYear, "01", "01", sep = "-")))
    dtg <- dtg[qq]
    temp <- temp[qq]
  }

  year <- year(dtg)
  month <- month(dtg)

  nyears <- stopYear - startYear + 1
  jyear <- seq(startYear, startYear + nyears - 1)

  tarr <- matrix(NA, nrow = nyears, ncol = 13)
  narr <- matrix(NA, nrow = nyears, ncol = 13)

  for (j in 1:nyears) {
    for (k in 1:12) {
      q <- which(year == (startYear + j - 1) & month == k)
      tarr[j, 1] <- jyear[j]
      if (length(q) > 20) {
        tarr[j, k + 1] <- mean(temp[q], na.rm = TRUE)
      }
    }
  }

  mon_avg <- colMeans(tarr[, -1], na.rm = TRUE)

  seas_avg <- c(
    mean(mon_avg[c(12, 1, 2)], na.rm = TRUE),
    mean(mon_avg[3:5], na.rm = TRUE),
    mean(mon_avg[6:8], na.rm = TRUE),
    mean(mon_avg[9:11], na.rm = TRUE)
  )

  ann_avg <- mean(seas_avg, na.rm = TRUE)

  dmon <- tarr
  for (j in 1:nyears) {
    for (k in 1:12) {
      dmon[j, k + 1] <- tarr[j, k + 1] - mon_avg[k]
    }
  }

  dseas <- matrix(NA, nrow = nyears, ncol = 5)
  dseas[, 1] <- jyear

  for (j in 1:nyears) {
    if (j == 1) {
      q1 <- NA; q2 <- dmon[j, 2]; q3 <- dmon[j, 3]
    } else {
      q1 <- dmon[j - 1, 13]; q2 <- dmon[j, 2]; q3 <- dmon[j, 3]
    }
    dseas[j, 2] <- mean(c(q1, q2, q3), na.rm = TRUE)

    q4 <- dmon[j, 4]; q5 <- dmon[j, 5]; q6 <- dmon[j, 6]
    dseas[j, 3] <- mean(c(q4, q5, q6), na.rm = TRUE)

    q7 <- dmon[j, 7]; q8 <- dmon[j, 8]; q9 <- dmon[j, 9]
    dseas[j, 4] <- mean(c(q7, q8, q9), na.rm = TRUE)

    q10 <- dmon[j, 10]; q11 <- dmon[j, 11]; q12 <- dmon[j, 12]
    dseas[j, 5] <- mean(c(q10, q11, q12), na.rm = TRUE)
  }

  dann <- matrix(NA, nrow = nyears, ncol = 2)
  dann[, 1] <- dseas[, 1]

  for (j in 1:nyears) {
    q1 <- dseas[j, 2]; q2 <- dseas[j, 3]; q3 <- dseas[j, 4]; q4 <- dseas[j, 5]
    dann[j, 2] <- mean(c(q1, q2, q3, q4), na.rm = TRUE)
  }

  seas <- dseas
  seas[, 2] <- dseas[, 2] + seas_avg[1]
  seas[, 3] <- dseas[, 3] + seas_avg[2]
  seas[, 4] <- dseas[, 4] + seas_avg[3]
  seas[, 5] <- dseas[, 5] + seas_avg[4]

  ann <- dann
  ann[, 2] <- ann_avg + dann[, 2]

  q <- which(ann[, 1] >= climstartYear & ann[, 1] <= climstopYear)
  avgClim <- mean(ann[q, 2], na.rm = TRUE)

  q <- which(ann[, 1] >= startYear)
  arrAnom <- ann[q, 2] - avgClim
  arrayAll[nstn, ] <- arrAnom
}

# Create a plot
arrYear <- ann[q, 1]
z <- colMeans(arrayAll, na.rm = TRUE)
zs <- apply(arrayAll, 2, sd, na.rm = TRUE)

data <- data.frame(
  Year = rep(arrYear, 2),
  Anomaly = c(z[z < 0], z[z >= 0]),
  Category = factor(rep(c("Below Average", "Above Average"), times = c(sum(z < 0), sum(z >= 0))))
)


data2 <- data.frame(Year = arrYear, Anomaly = z)

ggp <- ggplot(data2, aes(x = Year, y = Anomaly)) +
  geom_bar(aes(fill = Anomaly > 0), stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Sea Surface Temperature Anomalies - BC Lighthouses",
    x = "Year",
    y = "Temperature Anomaly [°C]"
  ) +
  theme(legend.position = "none")

# fit a linear model to the anomaly data
model <- lm(data2$Anomaly ~ data2$Year)
# estimate the 95% confidence interval
confidence_95 <- confint(model, level = 0.95)

# save plot to file
ggsave("BC_lighthouse_T.png", width = 6, height = 3, units = "in", scale = 1, dpi = 1200)

#ggp <- ggplot(data, aes(x = Year, y = Anomaly, fill = Category)) +
#  geom_bar(stat = "identity") +
#  geom_errorbar(aes(ymin = Anomaly - zs, ymax = Anomaly + zs), width = 0.2, color = "gray") +
#  geom_smooth(method = "lm", se = FALSE, color = "black") +
#  scale_fill_manual(values = c("Below Average" = "blue", "Above Average" = "red")) +
#  labs(
#    title = "Sea Surface Temperature Anomalies at BC Lighthouses",
#    x = "Year",
#    y = "Temperature Anomaly [°C]",
#    fill = "Category"
#  ) +
#  theme_minimal() +
# theme(legend.position = "bottom")

