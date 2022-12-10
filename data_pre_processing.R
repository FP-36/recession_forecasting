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
#write.csv(data, './recession_forecasting_data.csv')
