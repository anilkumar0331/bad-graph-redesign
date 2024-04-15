library(tidyverse)
library(reshape2)
library(readxl)

gdp_data = read_excel("/Users/Anil/Desktop/First_Semester/STAT515/Mid Project/GDP/gdp-data.xlsx")

melted_compare_data <- melt(data = gdp_data,
                            id.vars = c("Rank","Country"),
                            variable.name = "Variable",
                            value.name = "Value")

ggplot(melted_compare_data, aes(fill=Variable, y=Value, x= reorder(Country,Rank))) + 
  geom_col(width = 0.51,position=position_dodge(0.511)) +
  theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title ="GDP prediction of top 10 countries",x= "Countries",y = "GDP in Trillion USD") +
  geom_text(aes(label = round(Value,2)), position=position_dodge(0.51), vjust= -0.2, colour = "Black") +
  scale_y_continuous(breaks=seq(0, max(melted_compare_data$Value),5))
