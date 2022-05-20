### pH plot
# Read data from csv
Activity.pH <- read.csv("activity_by_pH.csv")
Activity.pH$pH <- as.character(round(Activity.pH$pH,2))
Activity.pH$pH <- ifelse(Activity.pH$pH == '5', "5.00", Activity.pH$pH)
Activity.pH$pH <- ifelse(Activity.pH$pH == '6', "6.00", Activity.pH$pH)
library(ggplot2)
# plotting with ggplot2
p <-   ggplot(data = Activity.pH,
              mapping = aes(x = Time, y = Abs,
                            group = pH, color = factor(pH))) +
       geom_point() +
       stat_smooth(method = "loess", formula = "y~x", se = FALSE) +
       facet_wrap(~ Property) +
       scale_y_continuous(breaks = seq(0, 2.0, 0.2)) +
       scale_x_continuous(breaks = seq(0, 75, 15)) +
       labs(x = "Time (s)", y = "Absorption value (Wavelength = 253 nm)") +
       guides(color = guide_legend(title = "pH", nrow = 1)) +
       theme(legend.position = "top", 
             legend.direction = "horizontal",
             plot.caption = element_text(size = 14, hjust = 0.5))
# acquire graphing data
p_plot_data <- ggplot_build(p)$data
p2 <- p_plot_data[[2]]
# output graph
pdf("pH_plot_2.pdf",height=6, width=9)
p
dev.off()

### pH tangent slope
# filter data, keep the first 4 points for each group
library(tidyverse)
p.filtered <- data.frame()
for (pnl in c(1:3)) {
       for (grp in c(1:11)) {
              p.filtered <- rbind(p.filtered, 
                                  head(p2[, c(2, 3, 5, 6)] %>% filter(
                                         group == grp, PANEL == pnl), 4))
       } 
}
# Calculate the slope of the tangent on (0,0)
p.slope <- data.frame(slope = NULL, group = NULL, PANEL = NULL)
for (pnl in c(1:3)) {
       for (grp in c(1:11)) {
              p.tmp <- p.filtered %>% filter(group == grp, PANEL == pnl)
              slope.tmp <- ((p.tmp$y[4] - p.tmp$y[2])/(p.tmp$x[4] - p.tmp$x[2]) + 
                            (p.tmp$y[3] - p.tmp$y[1])/(p.tmp$x[3] - p.tmp$x[1]))/2
              p.slope <- rbind(p.slope, data.frame(slope = slope.tmp,
                                                   group = grp,
                                                   PANEL = pnl))
       }
}
# Calculate enzyme activity
vol <- c(rep(3.2, 11), rep(3.0, 22))
molar_ext_coe <- 600
length <- 1
p.activity <- p.slope$slope * vol * 60 / (molar_ext_coe * length)
p.slope <- cbind(p.slope, p.activity)
colnames(p.slope)[4] <- "activity"
write.csv(p.slope,"pH_activity.csv")


### Temperature plot
Activity.temp <- read.csv("activity_by_temperature.csv")
# library(ggplot2)
t <-   ggplot(data = Activity.temp, 
              mapping = aes(x = Time, y = Abs, 
                            group = Temperature, color = factor(Temperature))) +
       geom_point() + 
       stat_smooth(method = "loess", formula = "y~x", se = FALSE) +
       scale_y_continuous(breaks = seq(0, 2.0, 0.2)) +
       scale_x_continuous(breaks = seq(0, 75, 15)) +
       labs(x = "Time (s)", y = "Absorption value (Wavelength = 253 nm)") +
       guides(color = guide_legend(title = "Temperature (°C)",
                                   nrow = 1)) +
       theme(legend.position = "top", 
             legend.direction = "horizontal", 
             plot.caption = element_text(size = 14, hjust = 0.5))
t_plot_data <- ggplot_build(t)$data
t2 <- t_plot_data[[2]]

pdf("temp_plot_2.pdf", height = 6, width = 6)
t
dev.off()

### temperature tangent slope
library(tidyverse)
t.filtered <- data.frame()

for (grp in c(1:6)) {
       t.filtered <- rbind(t.filtered, head(t2[, c(2, 3, 5)] %>% 
                                            filter(group == grp), 4))
} 

t.slope <- data.frame(slope = NULL, group = NULL)
for (grp in c(1:6)) {
       t.tmp <- t.filtered %>% filter(group == grp)
       slope.tmp <- ((t.tmp$y[4] - t.tmp$y[2])/(t.tmp$x[4] - t.tmp$x[2]) + 
                     (t.tmp$y[3] - t.tmp$y[1])/(t.tmp$x[3] - t.tmp$x[1]))/2
       t.slope <- rbind(t.slope, data.frame(slope = slope.tmp, group = grp))
} 
# Calculate enzyme activity
vol.t = 3.0
t.activity <- t.slope$slope * vol.t * 60 / (molar_ext_coe * length)
t.slope <- cbind(t.slope, t.activity)
colnames(t.slope)[3] <- "activity"
write.csv(t.slope,"temp_activity.csv")
