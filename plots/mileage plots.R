library(readr)
library(magrittr)
library(ggplot2)
library(data.table)
library(ggrepel)
library(ggthemes)
library(lubridate)
run <- read_csv("C:/Users/joshu/Downloads/garmin_running data 8-24-20.csv", 
                  col_types = cols(Date = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% as.data.table()
run2 <- read_csv("C:/Users/joshu/Downloads/garmin_running data 8-27-20.csv", 
                col_types = cols(Date = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                 `Avg Pace` = col_character(),
                                 `Best Pace` = col_character(),
                                 `Climb Time` = col_character())) %>% as.data.table()


run[`Activity Type` == "Track Running", Distance := Distance/1600]


run %>% summary()

run[, Date := as.Date(Date)]
run2[, Date := as.Date(Date)]
run2[Date > max(run$Date),]

run <- rbind(run2[Date > max(run$Date),], run)

gg <- run[Date >= as.Date("2020-01-01"), .(sum(Distance)), keyby = Date] 
gg[, Cumulative := cumsum(V1)]

goal <- data.table(date = seq(as.Date("2020-01-01"), today(), by = "day"), goal = 2000/365)
goal[, Cumulative := cumsum(goal)]

gg %>% ggplot(aes(x = Date, y = Cumulative)) + 
  geom_line(size = 2, color = fivethirtyeight_pal()(2)[2]) + 
  geom_line(data = goal, aes(x = date, y = Cumulative), color = fivethirtyeight_pal()(1)[1], size = 2) +
  geom_hline(yintercept = max(gg$Cumulative), linetype = "dashed") +
  geom_hline(yintercept = max(goal$Cumulative)) +
  geom_point(aes(x = today(), y = max(gg$Cumulative)), size = 4, color = fivethirtyeight_pal()(3)[2]) +
  geom_point(aes(x = today(), y = max(goal$Cumulative)), size = 4, color = fivethirtyeight_pal()(3)[1]) +
  annotate("label", 
           x = as.Date("2020-04-01"), 
           y = max(gg$Cumulative), 
           label = paste(max(gg$Cumulative), "Miles YTD"),
           size = 4.5) +
  annotate("text",
           x = as.Date("2020-06-01"), 
           y = max(goal$Cumulative + 50), 
           label = "Pacing for 2000") +
  labs(title = paste0("YTD Running Miles Through ", max(gg$Date)),
       subtitle = paste0(as.Date("2020-12-31") - today(), " days left in 2020 ~~~ ", paste(2000 - max(gg$Cumulative), "miles to 2000"))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
  scale_y_continuous(breaks = c(500, 1000, round(max(goal$Cumulative),0))) +
  theme_fivethirtyeight()+
  theme(axis.text.x = element_text(size = 14, angle = 25, color = "grey55", hjust = 1),
        axis.text.y = element_text(color = "grey55", size = 14))
ggsave("YTD_8_27_2020.png", dpi = 72, units = "in", width = 8, heigh = 4.5)


 #----------HOW LONG DOES IT TAKE TO RUN 1000 MILES EACH YEAR-------------#

ds <- run[Date >= as.Date("2018-01-01"), .(Distance = sum(Distance)), keyby = Date] %>% 
  rbind(data.table(Date = seq(as.Date("2020-07-19"), today(), 1), 
                   Distance = c(8.32, 6.8, 4.01, 7.02, 8.78)))
for (i in c(2018, 2019, 2020)) {
  ds[year(Date) == i, Cumulative := cumsum(Distance)]
  ds[year(Date) == i, Days := Date - as.Date(paste0(i, "-01-01"))]
}
ds[, Year := year(Date)]

points <- ds[Cumulative >= 1000, .(Day = min(Days), Miles = min(Cumulative)), keyby = .(Year = year(Date))]

ds[, Days := as.double.difftime(Days)]

ds[Cumulative < 1009] %>% 
  ggplot(aes(x = Days, y = Cumulative, group = Year, color = as.character(Year))) + 
  geom_line(alpha = .85, size = 1.25) +
  geom_point(data = as.data.frame(points),
             aes(x = Day, y = Miles), size = 3) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +labs(colour= NULL) +
  labs(title = "How Long Does it Take to Run 1000 Miles Each Year?", subtitle = "#beatyesterday") +
  geom_hline(yintercept=1000, linetype="dashed", color = "black") +
  scale_y_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 200)) +
  scale_x_continuous(breaks = c(100, 200, 300), labels = c("100 days", "200 days", "300 days")) +
  geom_label_repel(data = points, aes(x = Day, y = Miles, label = Year), 
             fill = fivethirtyeight_pal()(3), vjust = -.4, color = "white", 
             nudge_x = 20, segment.colour = "black", size = 4.5) +
  theme(legend.position = "none")

ggsave(filename = "days to hit 1k miles.png")
