################################################################################
# Covid Data Analysis South Tyrol
################################################################################

# packages
library(ggplot2)
library(dplyr)
library(readxl)
library(EpiEstim)
library(incidence)

################################################################################

# to do: calculate 7 day incidence per 100k inhabitants and compare curves!


# Information:
# Download daily incidence data from here:
# http://www.provinz.bz.it/sicherheit-zivilschutz/zivilschutz/aktuelle-daten-zum-coronavirus.asp

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
  filter(!is.na(New_cases))

#############
# Create Plot incl. loess smoothing
#############

ggplot(data_clean, aes(x = datum, y = New_cases)) +
  ggtitle("Entwicklung Covid (PCR + Antigen)") +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Datum", y = "neue Fälle") +
  geom_vline(xintercept = as.Date("2021-02-14"), linetype="solid", 
             color = "orangered", size=1) +
  geom_smooth(method = "loess", formula = 'y ~ x') +
  labs(caption="\U00A9 Thomas Ludwig") +
  facet_wrap(vars(Bezirkgsgemeinschaft), scales = "free") +
  theme(panel.spacing=unit(.02, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        strip.background = element_rect(color = "black", size = 0.5, fill = "lightgrey"))

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
plot(as.incidence(data_esti$New_cases, dates = data_esti$datum)) +
  theme_minimal()

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


plot(res_parametric_si, "R") +
  theme_minimal() +
  ggtitle("Estimated R_eff South Tyrol") +
  scale_x_date(date_breaks = "2 week", date_labels = "%d/%m/%Y") +
  annotate("rect", xmin = as.Date("2021-02-14"), # add shaded rectangle
           xmax = as.Date("2021-03-23"), 
           ymin = 0, ymax = 2, alpha = .15, fill = "lightblue") +
  geom_line(size = 1) +
  labs(caption="\U00A9 Thomas Ludwig") +
  theme(legend.position="bottom") # change legend position


plot(res_parametric_si)

# construct some nice summary output data frame
res_all <- cbind(data_esti$datum[8:length(data_esti$datum)], res_parametric_si$R)

################################################################################