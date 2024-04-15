library("tidyverse")
library(plotly)

df1 = data.frame(Total = covid_data$State_Code,cases = covid_data$Total_cases)
df2 = data.frame(Active = covid_data$State_Code,cases = covid_data$Active)
df3 = data.frame(Deaths = covid_data$State_Code,cases = covid_data$Deaths)
df4 = data.frame(Discharged = covid_data$State_Code,cases = covid_data$Discharged)

all = list(df1, df2, df3, df4)

all = lapply(all, function(df) {
  df$type = colnames(df)[1]
  colnames(df)[1] = "State"
  df
})

all = do.call(rbind, all)

custom_break_points = function(x) {
  if (max(x) < 16000) seq(0, 15000, 2500) 
  else if(max(x) < 160000) seq(0, 150000, 25000)
  else seq(0, 9000000, 1500000)
}

p = ggplot(all, aes(x=cases,y=reorder(State,-cases),fill=type)) +
  geom_col() +
  facet_wrap(~ type, scales = "free_x", ncol = 4) +
  labs(x= "Number of cases",y="States") + 
  ggtitle("Number of the COVID-19 cases across Indian states and union territories") +
  scale_x_continuous(breaks = custom_break_points) +
  scale_y_discrete(limits=rev) +
  theme(
    panel.background  = element_blank(),
    plot.background = element_rect(fill="gray94", colour="gray"),
    legend.background = element_rect(fill="transparent", colour=NA),
    legend.key = element_rect(fill="transparent", colour=NA),
    legend.position = "none",
    legend.title = element_blank(),
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l =0)),
    plot.title = element_text(hjust = 0.5,margin = margin(t = 0, r = 0, b = 15, l = 0))) +
    scale_fill_manual(values=c("#EED202","red","springgreen4","royalblue4"))

p

ggplotly(p)
