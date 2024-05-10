# Carica la libreria readxl

library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

# import df

path <- "/Users/pedrocchiedoardo/Desktop/r analisi/Dataset.xlsx"

df <- read_excel(path)

#Adjust the df
df$`NOMINAL GDP` <- as.character(df$`NOMINAL GDP`)
df$`NOMINAL GDP` <- as.numeric(gsub(",", "", df$`NOMINAL GDP`))

df$Year <- as.numeric(df$Year)

#EDA
summary(df)

# visualization

plot_variable <- function(data, variable_name) {
ggplot(data, aes(x = Year, y = .data[[variable_name]])) +
geom_line(color = "blue") +
labs(x = "Year", y = variable_name, title = paste(variable_name))

}
plot_variable(df, "NOMINAL GDP")

plot_variable(df, "DEFLATOR")
plot_variable(df, "INFLATION")
plot_variable(df, "UNEMPLOYMENT")
plot_variable(df, "GINI INDEX")
plot_variable(df, "NET LENDING_BORROWING")
plot_variable(df, "INTEREST PAYABLE")
plot_variable(df, "GROSS DEBIT")
plot_variable(df, "TOTAL EXPENDITURE")
plot_variable(df, "TOTAL  REVENUE")

#correlation#
df_corr <- na.omit(df) #eliminate null value
#graph#
correlation_matrix <- cor(df_corr[,2:ncol(df)])
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.cex = 0.7, tl.col = "black")
#table#
correlation_table <- round(correlation_matrix, 2)
print(correlation_table)
