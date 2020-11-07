################################################################################
# Visualization of Google Mobility Data for South Tyrol
#
# (c) Thomas Ludwig
# November 2020
################################################################################

# packages for analysis
library(dplyr) # data manipulation
library(ggplot2) # plots
library(reshape2) # transform to wide/long data format
library(zoo) # calculate moving average

################################################################################

# data download and input preparation

# Option 1: automated download and import of data of interest into R
temp <- tempfile() # create temporary file
download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",temp) # download file into temp file
data <- read.csv(unz(temp, "2020_IT_Region_Mobility_Report.csv")) # read data from temp file into R
unlink(temp) # delete temporary file

# Option 2: Import data from manually downloaded zip file into R
# You can download the data in a zip folder by hand here: https://www.google.com/covid19/mobility/.
# Just safe the data in the data folder and R will extract the data directly into the environment
data <- read.csv(unz("../Data/Region_Mobility_Report_CSVs.zip",
                     "2020_IT_Region_Mobility_Report.csv"))

################################################################################

# start of actual data analysis

data <- data %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) # convert to actual date variable for filtering on date

# filter data on South Tyrol
data_st <- data %>%
  filter(sub_region_2 == "South Tyrol") # filter on South Tyrol data

##########################
# PLot Analysis of Data
##########################

# plot one variable of South Tyrol data
ggplot(data_st) + 
  geom_line(
    mapping = aes(x = date, y = workplaces_percent_change_from_baseline),
    color = "darkgrey", size = 1) +
  theme_minimal() +
  ggtitle("workplaces_percent_change_from_baseline South Tyrol") +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1) +
  geom_hline(yintercept=min(data_st$workplaces_percent_change_from_baseline), 
             linetype="dashed", color = "orangered", size = 1)


# plot and compare all variables in one plot for South Tyrol

# first we have to transform the data to long format as input for the ggplot2 plotting engine
meltdf <- melt(data_st[, c(8:14)],id="date") # select columns of interest for transformation

# plot transformed data
ggplot(meltdf,aes(x=date,
                  y=value,
                  colour=variable,
                  group=variable)) + geom_line() +
  theme_minimal() +
  geom_vline(xintercept = as.Date("2020-02-24"), 
             color = "blue", size=1) +
  geom_text(aes(x=as.Date("2020-02-26"), 
                          label="erster Covid-19 Fall Südtirol", y=100), 
                colour="blue", angle=90) +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1) +
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "blue", size=1) +
  geom_text(aes(x=as.Date("2020-03-13"), 
                label="Beginn Lockdown Italien", y=100), 
            colour="blue", angle=90) +
  geom_vline(xintercept = as.Date("2020-05-04"), 
             color = "blue", size=1) +
  geom_text(aes(x=as.Date("2020-05-06"), 
                label="Ende Lockdown Italien", y=100), 
            colour="blue", angle=90) +
  annotate("rect", xmin = as.Date("2020-03-11"), # add shaded rectangle
           xmax = as.Date("2020-05-04"), 
           ymin = -100, ymax = 300, alpha = .15, fill = "lightblue") +
  ggtitle("Google Mobility Report Daten Südtirol") +
  theme(legend.position="bottom") # change legend position


# plot and compare all variables of South Tyrol since start of September
ggplot(meltdf %>%
         filter(date > "2020-09-01"),aes(x=date,
                  y=value,
                  colour=variable,
                  group=variable)) + geom_line() +
  theme_minimal() +
  theme(legend.position="bottom") + # change legend position
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1)


# plot transit stations and workspace in seperate plots
ggplot(meltdf %>%
         filter(date > "2020-09-01") %>%
         filter(variable == "workplaces_percent_change_from_baseline"),aes(x=date,
                                         y=value,
                                         colour=variable,
                                         group=variable)) + geom_line() +
  theme_minimal() +
  theme(legend.position="bottom") + # change legend position
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1) +
  ggtitle("Google Mobility Data Workplaces")

ggplot(meltdf %>%
         filter(date > "2020-09-01") %>%
         filter(variable == "transit_stations_percent_change_from_baseline"),aes(x=date,
                                                                           y=value,
                                                                           colour=variable,
                                                                           group=variable)) + geom_line() +
  theme_minimal() +
  theme(legend.position="bottom") + # change legend position
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1) +
  ggtitle("Google Mobility Data Transit Stations")

# calculate moving average to denoise the data and better show actual trend line!
# here I calculate the 7 day moving average to avoid day of the week effect, which i
# clearly visible in this data.

data_ma_st <- meltdf %>%
  group_by(variable) %>% # calculate moving average for each variable
  mutate(roll_mean = rollmean(value, 7, na.pad = TRUE, align = "right")) %>% # calculate moving average
  filter(!is.na(roll_mean)) # delete all NAs in the data (created due to MA calculation)

ggplot(data_ma_st,aes(x=date,
                  y=roll_mean,
                  colour=variable,
                  group=variable)) + geom_line() +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1) +
  geom_vline(xintercept = as.Date("2020-02-24"), 
             color = "blue", size=1) +
  geom_text(aes(x=as.Date("2020-02-26"), 
                label="erster Covid-19 Fall Südtirol", y=100), 
            colour="blue", angle=90) +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1) +
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "blue", size=1) +
  geom_text(aes(x=as.Date("2020-03-13"), 
                label="Beginn Lockdown Italien", y=100), 
            colour="blue", angle=90) +
  geom_vline(xintercept = as.Date("2020-05-04"), 
             color = "blue", size=1) +
  geom_text(aes(x=as.Date("2020-05-06"), 
                label="Ende Lockdown Italien", y=100), 
            colour="blue", angle=90) +
  ggtitle("Google Mobility Report Daten Südtirol") +
  theme(legend.position="bottom") # change legend position


# let's look at the situation since the start of September 2020
ggplot(data_ma_st %>%
         filter(date > "2020-09-01"),aes(x=date,
                      y=roll_mean,
                      colour=variable,
                      group=variable)) + geom_line() +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1) +
  theme(legend.position="bottom") # change legend position



# compare actual data with moving average for workplaces_percent_change_from_baseline
ggplot(data_ma_st %>%
         filter(variable == "workplaces_percent_change_from_baseline"), 
       aes(x=date)) + 
  geom_line(aes(y = value), color = "darkred") + 
  geom_line(aes(y = roll_mean), color="steelblue", size = 1) +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1) +
  ggtitle("Raw and Smoothed Data Workplaces from Baseline")

# compare actual data with moving average for workplaces_percent_change_from_baseline since September
ggplot(data_ma_st %>%
         filter(variable == "workplaces_percent_change_from_baseline") %>%
         filter(date > "2020-09-01"), 
       aes(x=date)) + 
  geom_line(aes(y = value), color = "darkred") + 
  geom_line(aes(y = roll_mean), color="steelblue", size = 1) +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1) +
  ggtitle("Raw and Smoothed Data Workplaces from Baseline South Tyrol")

################################################################################

# compare behavior in workplaces across regions in Italy
ggplot(data %>%
         filter(sub_region_1 != ""),aes(x=date,
                  y=workplaces_percent_change_from_baseline,
                  colour=sub_region_1,
                  group=sub_region_1)) + geom_line() +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1)


# daily data is very noisy --> calculate monthly moving average

data_ma <- data %>%
  filter(sub_region_1 != "") %>%
  group_by(sub_region_1) %>% 
  mutate(roll_mean = rollmean(workplaces_percent_change_from_baseline, 30, 
                              na.pad = TRUE, align = "right")) %>%
  filter(!is.na(roll_mean))

ggplot(data_ma,aes(x=date,
                   y=roll_mean,
                   colour=sub_region_1,
                   group=sub_region_1)) + geom_line() +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1)

# define some sub regions we want to focus the analysis on
target <- c("Lombardy", "Trentino-South Tyrol", "Piedmont", "Sicily")

# plot data on selected target regions
data_ma_target <- data %>% 
  filter(sub_region_1 != "") %>%
  group_by(sub_region_1) %>% 
  mutate(roll_mean = rollmean(workplaces_percent_change_from_baseline, 30, 
                              na.pad = TRUE, 
                              align = "right")) %>%
  filter(!is.na(roll_mean)) %>% # exclude NAs (generated by moving average function)
  filter(sub_region_1 %in% target)

# plot for moving average in target regions
ggplot(data_ma_target,aes(x=date,
                          y=roll_mean,
                          colour=sub_region_1,
                          group=sub_region_1)) + geom_line() +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1)

# plot only subregion Trentino South Tyrol
ggplot(data_ma_target %>%
         filter(sub_region_1 == "Trentino-South Tyrol"),aes(x=date,
                                                            y=roll_mean,
                                                            colour=sub_region_1,
                                                            group=sub_region_1)) + geom_line() +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "orangered", size = 1)


################################################################################
################################################################################