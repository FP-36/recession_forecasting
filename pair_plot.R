library(ggplot2)
library(GGally)
library(tidyr)
library(tidyverse)
library(scales)
months_bill <- read.csv('./TB3MS.csv')
capacity_utilization <- read.csv('./CUMFNS.csv')
industrial_prod <- read.csv('./INDPRO.csv')
cpi_index <- read.csv('./CPIAUCNS.csv')
year_rate_10 <- read.csv('./GS10.csv')
unemployment_rate <- read.csv('./UNRATE.csv')
us_recession <- read.csv('./USREC.csv')

colnames(months_bill)
colnames(capacity_utilization)
colnames(industrial_prod)
colnames(cpi_index)
colnames(year_rate_10)
colnames(unemployment_rate)
colnames(us_recession)

#All have common col called DATE, and since our target variable is recession we have to get data filtered according to the available DATE in recession table.

data <- merge(x = year_rate_10, 
      y = merge(x = unemployment_rate, 
                y = merge(x = capacity_utilization, 
                          y = merge(x = months_bill, 
                                    y = merge(x = industrial_prod, 
                                              y = merge(x = cpi_index, 
                                                        y = us_recession), by = 'DATE'), by = 'DATE'), by = 'DATE'), by = 'DATE'), by = 'DATE')
which(is.na(data))

#All Factors Bar chart

recession_dates <- filter(data, USREC == "1")

recession_years <- unique(lubridate::year(as.Date(recession_dates$DATE)))

data_copy <- data
data_copy$USREC <- data_copy$USREC * 100
data_copy$DATE <- lubridate::year(as.Date(data_copy$DATE))

tidy_data <- data_copy %>% pivot_longer(cols = -c("DATE","USREC"), names_to = "Factors") |> mutate(Factors = forcats::fct_reorder2(Factors,DATE, value))

ggp <- ggplot(tidy_data, aes(DATE, value, color = Factors)) +
  geom_line() +
  ggtitle("Factors and Recession Relation") +  labs (x = "", y = "Value") +
  theme_grey(16) +
  theme(legend.title = element_blank()) + scale_y_continuous(limits = c(0, 100,5)) 
ggp +  geom_vline(xintercept = recession_years, alpha = 0.2) 

  
#Relation between the factors
Recession <- as.factor(data$USREC)
ggpairs(data,legend = 1,columns=2:7,aes(color= Recession, alpha = 0.4),
        upper = list(continuous = "blank", combo = "box_no_facet"),
        lower = list(continuous = wrap("points", alpha = 0.3,    size=0.3), 
                     combo = wrap("dot", alpha = 0.4,            size=0.3) ), title = "Correlation Between factors") + 
  theme(legend.position = "right")


#Time series
ggplot(data_copy, 
       aes(DATE, GS10)) + 
  geom_line() 

ggplot(data_copy, 
       aes(DATE, UNRATE)) + 
  geom_line() 

ggplot(data_copy, 
       aes(DATE, TB3MS)) + 
  geom_line() 
