## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(dateutils)
library(data.table)
fred_quarterly <- agg_to_freq(fred, frq = "quarter")
print(fred_quarterly[1:5])

## -----------------------------------------------------------------------------
fred_monthly_wide <- agg_to_freq_wide(fred, frq = "month")
print(fred_monthly_wide$dt[1:6])

## -----------------------------------------------------------------------------
fred_sa <- seas_df_long(fred_quarterly, sa_names = c("gdp constant prices", "advance retail sales"),
                        transfunc = 'auto')
gdp <- rbind(fred_quarterly[series_name == "gdp constant prices", .(ref_date, series_name, value)],
             fred_sa$values_sa[series_name == "gdp constant prices sa"])
gdp <- dcast(gdp, ref_date ~ series_name, value.var = "value")
matplot(gdp$ref_date, gdp[,-1,with=FALSE], type = 'l')

## -----------------------------------------------------------------------------
tail(fred[series_name == "gdp constant prices"], 2)

## -----------------------------------------------------------------------------
tail(fred[series_name == "advance retail sales"], 4)

## -----------------------------------------------------------------------------
tail(fred[series_name == "initial jobless claims"])

## -----------------------------------------------------------------------------
tail(fred[series_name == "t bill spread 10y 3m"], 3)

## -----------------------------------------------------------------------------
MF <- process_MF(fred[series_name == "gdp constant prices"], fred[series_name != "gdp constant prices"],
                 LHS_lags = 3, RHS_lags = 3) 

## -----------------------------------------------------------------------------
library(data.table)
dt_wide <- dcast(MF, ref_date ~ series_name, value.var = "value")
tail(dt_wide)

## -----------------------------------------------------------------------------
print(fredlib)

## -----------------------------------------------------------------------------
dt_processed <- process(MF, fredlib)
print(dt_processed[1:4])

## -----------------------------------------------------------------------------
X <- dcast(dt_processed, ref_date ~ series_name, value.var = "value")
out <- lm(`gdp constant prices 0` ~ `advance retail sales 0` + 
          `initial jobless claims 0` + `t bill spread 10y 3m 0`, data = X)
summary(out)

## -----------------------------------------------------------------------------
ls("package:dateutils")

## -----------------------------------------------------------------------------
dates <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2021-06-30"), by = "day")
weekvals <- end_of_period(dates, period = "week")
unique(weekdays(weekvals))

## -----------------------------------------------------------------------------
monthvals <- end_of_period(dates, period = "month", shift = 1)
unique(monthvals)

