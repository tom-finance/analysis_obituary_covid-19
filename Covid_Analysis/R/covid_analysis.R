################################################################################
# Covid Data Analysis South Tyrol

# (c) Thomas Ludwig

################################################################################

# packages
library(ggplot2)
library(dplyr)
library(readxl)
library(EpiEstim)
library(incidence)
library(RcppRoll)

################################################################################

# Information:
# Download daily incidence data from here:
# http://www.provinz.bz.it/sicherheit-zivilschutz/zivilschutz/aktuelle-daten-zum-coronavirus.asp

# DISCLAIMER: All calculations are indicative. All errors are my own.


# to do:
# add automated download from internet to analysis!
# plots with lockdown rectangel: end of lockdown is fixed value!

################################################################################

# import Covid data
data <- read.csv2("../Input/Corona.Data.Detail.csv",
                  na.strings = "null", encoding = "UTF-8")

# import reference data
gemeinden <- read_excel("../Input/gemeindedaten.xlsx") %>%
  select(Gemeinde, Bezirkgsgemeinschaft) %>% 
  add_row(Gemeinde = "Gesamt", Bezirkgsgemeinschaft = "0_Südtirol total")

# quick fix for data join
gemeinden$Gemeinde <- gsub("SANKT ", "ST.", gemeinden$Gemeinde)
data$datum <- gsub(" 00:00:00.0", "", data$datum)
data$datum <- as.Date(data$datum, "%Y-%m-%d")
data$name <- gsub(" Gesamt", "", data$name)

#######################
# Population data
#######################

population <- data.frame(Bezirkgsgemeinschaft = c("Burggrafenamt", "Eisacktal",
                                                  "Pustertal", "Salten Schlern",
                                                  "Überetsch-Südtiroler Unterland", "Vinschgau",
                                                  "Wipptal", "Bozen", "0_Südtirol total"),
                         Einwohner = c(100000, 50000, 80000, 50000, 75000, 35000, 20000,
                                       107000, 521000))

################################################################################

data_clean <- data %>%
  left_join(., gemeinden, by = c("name" = "Gemeinde")) %>%
  filter(name != "Außerhalb der Provinz") %>%
  filter(name != "Gemeinde unbekannt ") %>% 
  group_by(datum, Bezirkgsgemeinschaft) %>% 
  summarise(Sum_Cases = sum(sumPos.pcr.antigen.)) %>%
  filter(!is.na(Sum_Cases)) %>% 
  group_by(Bezirkgsgemeinschaft) %>% 
  mutate(New_cases = Sum_Cases - lag(Sum_Cases)) %>% 
  ungroup() %>% 
  filter(!is.na(New_cases)) %>% 
  mutate(New_cases = case_when(New_cases < 0 ~ 0L, # set incidence to zero if negative
                       TRUE ~.$New_cases))

#############
# Create Plot incl. loess smoothing
#############


plot_smooth <- ggplot(data_clean, aes(x = datum, y = New_cases)) +
  ggtitle("Entwicklung Covid (PCR + Antigen)") +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Datum", y = "neue Fälle") +
  geom_vline(xintercept = as.Date("2021-02-14"), linetype="solid", 
             color = "orangered", size=1) +
  geom_smooth(method = "loess", formula = 'y ~ x') +
  # labs(caption="\U00A9 Thomas Ludwig") +
  facet_wrap(vars(Bezirkgsgemeinschaft), scales = "free") +
  theme(panel.spacing=unit(.02, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        strip.background = element_rect(color = "black", size = 0.5, fill = "lightgrey"))

plot_smooth

#############
# Calculate 7 day incidence per 100k inhabitants
#############

seven_day_incidence <- data_clean %>% 
  group_by(Bezirkgsgemeinschaft) %>% 
  mutate(roll_sum = roll_sum(New_cases, 7, align = "right", fill = NA)) %>% 
  ungroup() %>% 
  left_join(., population) %>% # join with population data
  filter(!is.na(roll_sum)) %>%  # exclude NA values
  mutate(seven_day_incidence = roll_sum/Einwohner*10^5)

#################
# Plot data
#################

#################
# Plot aggregated Data for South Tyrol
#################

# Note: Value not 100% accurate with official data.

# get max value for nicer plot
max_value_overall <- seven_day_incidence %>% 
  filter(Bezirkgsgemeinschaft == "0_Südtirol total") %>% 
  select(seven_day_incidence) %>% 
  filter(seven_day_incidence == max(seven_day_incidence)) %>% 
  pull()

seven_day <- ggplot(seven_day_incidence %>% 
                      filter(Bezirkgsgemeinschaft == "0_Südtirol total"), 
                    aes(x = datum, y = seven_day_incidence)) +
  geom_line(size = 1, colour = "steelblue") +
  theme_minimal() +
  labs(x = "Datum", y = "7-Tages Inzidenz/100k EW") +
  theme(legend.position="bottom") +
  annotate("rect", xmin = as.Date("2021-02-14"), # add shaded rectangle
           xmax = as.Date("2021-03-23"), 
           ymin = 0, ymax = max_value_overall, 
           alpha = .15, fill = "lightblue") +
  geom_hline(yintercept=50, linetype="dashed", color = "orangered", size = 1)

seven_day

#################
# Plot all data
#################

seven_day_all <- ggplot(seven_day_incidence, aes(x = datum, y = seven_day_incidence,
                                                 group=Bezirkgsgemeinschaft, 
                                                 color=Bezirkgsgemeinschaft)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Datum", y = "7-Tages Inzidenz/100k EW") +
  theme(legend.position="bottom") +
  annotate("rect", xmin = as.Date("2021-02-14"), # add shaded rectangle
           xmax = as.Date("2021-03-23"), 
           ymin = 0, ymax = max(seven_day_incidence$seven_day_incidence), 
           alpha = .15, fill = "lightblue") +
  geom_hline(yintercept=50, linetype="dashed", color = "orangered", size = 1)


seven_day_all

################################################################################

#############
# Estimation of effective Reproduction Number (R_eff) for South Tyrol
#############

# Note: We use a simplified parametric approach here, one could also use a more
# sophisticated approach.


# filter data of interest - here we estimate for South Tyrol
data_esti <- data_clean %>% 
  filter(Bezirkgsgemeinschaft == "0_Südtirol total")


# plot incidence data for South Tyrol in nice ggplot format
incidence_daily <- plot(as.incidence(data_esti$New_cases, dates = data_esti$datum)) +
  theme_minimal() +
  ggtitle("Daily Incidence Curve")

incidence_daily

# note: we use parametric serial interval here. therefore we need to specify
# mean and standard deviation of serial interval. Here we assume the seria interval
# to ge gamma distribuited, hence we need to specify the model parameters in explicit form
# We use some best practice estimates for Covid-19 here.

res_parametric_si <- estimate_R(data_esti$New_cases, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 4.8, 
                                  std_si = 2.3)))

# fix date vector for nicer plot
res_parametric_si$dates <- data_esti$datum


r_eff <- plot(res_parametric_si, "R") +
  theme_minimal() +
  ggtitle("Estimated R_eff South Tyrol") +
  scale_x_date(date_breaks = "2 week", date_labels = "%d/%m/%Y") +
  annotate("rect", xmin = as.Date("2021-02-14"), # add shaded rectangle
           xmax = as.Date("2021-03-23"), 
           ymin = 0, ymax = 2, alpha = .15, fill = "lightblue") +
  geom_line(size = 1) +
  # labs(caption="\U00A9 Thomas Ludwig") +
  theme(legend.position="bottom") # change legend position

r_eff


# some overall information about the estimation of R_eff for South Tyrol
# nicer plot not easily to implement --> need to decompose plot

plot(res_parametric_si)


# construct some nice summary output data frame
res_all <- cbind(data_esti$datum[8:length(data_esti$datum)], res_parametric_si$R)

################################################################################
################################################################################