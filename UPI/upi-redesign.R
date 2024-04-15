library(tidyverse)
library(gridExtra)

month_year = str_c(upi_data$Month,"-", upi_data$Year)

date = str_c(upi_data$Date,"/01/",upi_data$Year)

date_format = as.Date(date, format="%d/%m/%Y")

Month_in_Year = reorder(month_year,date_format)

# ======================================================================================================
  
banks_graph = ggplot(upi_data, aes(x=Month_in_Year, y=Number_of_banks, fill=Year)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,400,50)) +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1),
        panel.background  = element_blank(),
        plot.background = element_rect(fill="gray96", colour=NA),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key = element_rect(fill="transparent", colour=NA),
        plot.margin = margin(0,2.2,0,0, "cm"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l =0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l =15)),
        legend.position = "none") + 
  labs(x="Month",y="Number of banks") +
  scale_fill_gradientn(colors=rainbow(7))

volume_graph = ggplot(upi_data, aes(x=Month_in_Year, y=Volume, fill=Year, group = 1)) +
  geom_line(size = 1.5, color="darkslategrey") +
  geom_point(shape = 21, color = 'black', size = 3.3) +
  scale_y_continuous(breaks = seq(0,7000,1000)) +
  theme(axis.text.x = element_blank(),
        panel.background  = element_blank(),
        plot.background = element_rect(fill="gray96", colour=NA),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key = element_rect(fill="transparent", colour=NA),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l =10)),
        legend.margin = margin(b=-9,unit = "cm")) +
  labs(x="",y="Volume of Transactions(in Millions)") +
  scale_fill_gradientn(colors = rainbow(7)) 

first_grid = grid.arrange(volume_graph,banks_graph, top = "Relationship between Number of banks live on UPI and total amount of transactions")

# ============================================================================================================================

amount_graph = ggplot(upi_data,aes(x=Month_in_Year, y = Value_in_USD, fill=Year)) +
  geom_col(width = 1)+
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1),
        panel.background  = element_blank(),
        plot.background = element_rect(fill="gray96", colour="gray"),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key = element_rect(fill="transparent", colour=NA),
        plot.margin = margin(0,2.5,0,0, "cm"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l =0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l =10)),
        legend.position = "none") + 
  labs(x="Month",y="Amount in USD(in billions)") +
  scale_fill_gradientn(colors=rainbow(7))

growth_graph = ggplot(upi_data,aes(x=Month_in_Year, y = Growth, fill=Year)) +
  geom_col()+
  theme(axis.text.x = element_blank(),
        panel.background  = element_blank(),
        plot.background = element_rect(fill="gray96", colour="gray"),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key = element_rect(fill="transparent", colour=NA),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l =5))) +
  labs(x="",y="Growth in percentage") +
  scale_fill_gradientn(colors = rainbow(7))

change_in_growth_graph = ggplot(upi_data,aes(x=Month_in_Year, y = Change_in_growth, group = 1, color=Year)) +
  geom_point()+
  geom_line(size = 1.2)+
  theme(axis.text.x = element_blank(),
        panel.background  = element_blank(),
        plot.background = element_rect(fill="gray96", colour="gray"),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key = element_rect(fill="transparent", colour=NA),
        plot.margin = margin(0,2.5,0,0, "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l =15)),
        legend.position = "none") +
  labs(x="",y="Change in growth(%)")+
  scale_color_gradientn(colors = rainbow(7))

second_grid = grid.arrange(change_in_growth_graph,growth_graph,amount_graph,top="Value of transactions and its growth")


