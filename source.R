## pkg_loading
pkg = c('data.table', 'magrittr', 'quantmod', 'rvest', 'httr', 'jsonlite',
        'readr', 'readxl', 'stringr', 'lubridate', 'dplyr',
        'tidyr', 'ggplot2', 'corrplot', 'dygraphs',
        'highcharter', 'plotly', 'PerformanceAnalytics',
        'nloptr', 'quadprog', 'RiskPortfolios', 'cccp',
        'timetk', 'broom', 'stargazer', 'timeSeries', "httr", "jsonlite", "rvest","xml2")

for(i in 1:length(pkg)){
  if(require(pkg[i], character.only = T)){
    library(pkg[i], character.only = T)
  } else {
    install.packages(paste0(pkg[i]))
  }
}
rm(list=ls())