## BIOL 6692: ggplot excercise ##
## Andrew Blinn - 2/9/2022     ##

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(scales)

# load model data for study streams
my.data <- read.csv("C:/Users/andre/OneDrive/Documents/R/STORMS/all_streams_summary.csv", header = T)
my.data$date <- as.POSIXct(my.data$date)

# load hydrology data from USGS
hydrology <- read.csv("C:/Users/andre/OneDrive/Documents/R/STORMS/full_water_record.csv", header = T)
hydrology$date <- as.POSIXct(hydrology$dateTime)

# create a list of paired colors for points and fills
color <- brewer.pal(n = 10, name = "Paired")

# specify consistent breaks for both plots
breaks <- c(as.POSIXct("2018-10-01"),as.POSIXct("2019-01-01"),as.POSIXct("2019-04-01"),
            as.POSIXct("2019-07-01"),as.POSIXct("2019-10-01"),as.POSIXct("2020-01-01"),
            as.POSIXct("2020-04-01"),as.POSIXct("2020-07-01"),as.POSIXct("2020-10-01"))

# set custom theme and elements to be shared by both plots
plot_theme <- theme_minimal() +
  
  # remove grid lines and add simple border
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill=NA, size = .7),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(1,2,1,1, "cm"))


# select process data from a single stream and remove empty rows
Mill.process <- ggplot(data = my.data[my.data$Site=="Mill"&!is.na(my.data$GPP),]) +
  
  # horizontal line at y=0 as visual aid
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  
  # filled colored points of gross primary production and ecosystem respiration
  geom_point(aes(x = date, y = GPP, color = "GPP"), fill = color[1], shape = 21, size = 2) + 
  geom_point(aes(x = date, y = ER, color = "ER"), fill = color[5], shape = 21, size =2) +
  
  # manually set color of points with paired color scheme used for fill
  scale_color_manual(values = c("ER" = color[6], "GPP" = color[2]), ) +
  
  # make same manual change to fill colors for the legend
  guides(color = guide_legend(reverse=T, override.aes = list(fill = c(color[1],color[5])))) +
  
  # set labels for axis and legend
  labs(title = "Daily Estimates of GPP and ER at Mill Creek", 
       x = "", y = expression("Process Rate (g O"[2]*" m"^-2*" d"^-1*")"),
       color = "Ecosystem Process") +
  
  # format x axis to be easy to read short month and year
  scale_x_datetime(limits = c(min(breaks),max(breaks)),breaks = breaks, date_labels = "%b %Y", expand = c(.01,.01)) +
  
  # use premade plot theme
  plot_theme

Mill.process


# select hydrology data for single stream
Mill.hydrology <- ggplot(data = hydrology[hydrology$Site=="Mill",]) +
  geom_line(aes(x = date, y = X_00060_00000), color = color[2]) +
  
  # set labels for axes and title
  labs(title = "Instantaneous (5-min interval) Stream Discharge at Mill Creek",
       x = "", 
       y = expression("Flow Rate (m"^3*" s"^-1*")")) +
  
  # scale y axis to log base 10 as customary for discharge data
  scale_y_log10(labels = trans_format('log10', math_format(10^.x))) +
  
  # format x axis to be easy to read short month and year
  scale_x_datetime(limits = c(min(breaks),max(breaks)),breaks = breaks, date_labels = "%b %Y", expand = c(.01,.01)) +
  
  # use premade plot theme
  plot_theme

Mill.hydrology

# arrange both plots together to compare trends of process rates with hydrology
panel_plot <- ggarrange(Mill.process, Mill.hydrology,
                        ncol = 1,
                        nrow = 2,
                        widths = 1,
                        heights = 1,
                        common.legend = FALSE,
                        legend = 'bottom')

# save plot (hi-res)
png(file="practice_plot_ajb.png", width=6000, height=4000, units="px", res=600)
panel_plot
dev.off()
