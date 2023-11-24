#This is the code for a difference in difference experimental design about the effects of the Babri Masjid Demolition in India on societal religiosity, as measured by naming conventions
# loading Packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(panelView))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(xtable))
suppressPackageStartupMessages(library(lmtest))

# set WD and read the CSV file
setwd("~/Desktop/BabriMasjid")
voter_data <- read.csv("voter_dta.csv")

## SETTING UP DATA

# creating a df with all possible birth years and all possible family IDs and merge with voter_data
# all possible birth years
birth_year <- data.frame(x=c(seq(1987, 1997)))
# all unique family IDs
family_id<-data.frame(unique(voter_data$family_id))
# renaming temporarily
colnames(birth_year)<-c('Year')
colnames(family_id)<-c('Id')
# newdf of all possible combos
x<-merge(family_id, birth_year)
dataset<-merge(x=voter_data %>% rename(Id='family_id', Year='birth_year'), y=x, by=c('Id', 'Year'), all=TRUE)

# dropping years outside of our time period
dataset <- dataset[dataset$'Year' >= 1987 & dataset$'Year' <= 1997,]

# changing nulls to NAs
dataset %>%
  mutate(voter_name = ifelse(voter_name== 'null', NA, voter_name))

# creating birth_occurred variable, which is a dummy variable depending on if a birth occurred or not (Yes=1, No=0)
dataset <- dataset %>%
  mutate(birth_occurred=if_else(!is.na(voter_name), 1, 0))

# filling in the variables which we want to keep across all observations and adding the family_religion_score
final_df <- dataset %>% 
  group_by(Id) %>%
  fill(father_value, religion, father_name, .direction='downup') %>% 
  mutate(family_religion_score = ifelse(Year==1987, father_value, NA)) %>%
  mutate(family_religion_score = ifelse(birth_occurred==1, voter_value, family_religion_score)) %>%
  fill(family_religion_score) 

# how many Muslim families are there?
num_muslim_families <- dataset %>%
  filter(religion == "Muslim") %>%
  distinct(Id) %>%
  nrow()

## LINE PLOT

# creating an average family religion score
final_df <- final_df %>% 
  group_by(religion, Year) %>%
  mutate(mean_religion_score = mean(family_religion_score, na.rm = TRUE))

ggplot(final_df, aes(x=Year, y=mean_religion_score, color=religion)) +
  geom_line() +
  labs(title="Change in Family Religion Score over Time", x="Year", y="Average Family Religion Score", color="Religion") +
  scale_color_manual(values=c("blue", "green")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     breaks = seq(1987, 1997, by = 1))

ggsave("Change in Family Religion Score over Time.pdf")

## PANEL VIEW PLOT

#limiting families in panel to families 250 to 300
panel_data <- final_df[final_df$'Id' >= 250 & final_df$'Id' <= 300, ]

#creating treatment condition, which is being Muslim post 1992
panel_data <- panel_data  %>%
  mutate(treatment = ifelse(Year > 1992 & religion == "Muslim", 1, 0))

# Create panel view 
panelview(family_religion_score ~ treatment,
          data = panel_data, index = c("Id","Year"),
          xlab = "Year", ylab = "Family Id",
          by.timing = TRUE, 
          legend.labs = c("Control (Muslim Pre 1992, Hindu 1987 to 1997)", "Treated (Muslim Post 1992)"), background = "white")
ggsave("Treatment Status.pdf")

## REGRESSIONS

#creating the treatment condition for the estimating equation
final_df <- final_df %>%
  mutate(treatment = ifelse(religion == "Muslim", 1, 0))
#creating the time period for the estimating equation
final_df <- final_df %>%
  mutate(after = ifelse(Year >= 1992, 1, 0))
#creating the interaction effect
final_df <- final_df %>%
  mutate(interaction = treatment * after)
#making the model
did_model <- lm(family_religion_score ~ treatment + after + interaction, data = final_df)
print(summary(did_model))

#printing the estimating equation
estimating_equation <- paste0("Y_i = ", 
                              round(coef(did_model)[1], 2), 
                              " + ", 
                              round(coef(did_model)[2], 2), "T_i", 
                              round(coef(did_model)[3], 2), "t_i + ",
                              round(coef(did_model)[4], 2), "(T_i*t_i)")
estimating_equation

## EVENT STUDIES
# making control and treatment vars
final_df <- final_df %>%
  mutate(event_study_treatment = ifelse(religion == "Muslim", 1, 0))
final_df <- final_df %>%
  mutate(event_study_control = ifelse(religion == "Hindu", 1, 0))

# creating an event dummy variable indicating whether the event occurred and if the group was treated (interaction term)
final_df$event <- ifelse(final_df$Year > 1992 & final_df$religion == "Muslim", 1, 0)

#defining the event window as 1992 onwards 
event_window <- c(1,5)
#defining the reference period as 5 years before the event
reference_period <- c(-5,-1) 

#subsetting the data to the event and reference periods
event_data <- final_df %>% filter(Year >= event_window[1] & Year <= event_window[2])
reference_data <- final_df %>% filter(Year >= reference_period[1] & Year <= reference_period[2])

#creating indicator variables for each year in the reference period
reference_years <- seq(reference_period[1], reference_period[2])
reference_indicators <- lapply(reference_years, function(x) ifelse(final_df$Year == x, 1, 0))
names(reference_indicators) <- paste0("year_", reference_years)

# running the event study regression for the treatment group (Muslim families post 1992)
treatment_regression <- lm(family_religion_score ~ event_study_treatment, data = final_df)

# running the event study regression for the control group (Hindu families post 1992)
control_regression <- lm(family_religion_score ~ event_study_control, data = final_df)

# extracting coefficient estimates for both event and reference indicators
treatment_estimates <- coef(summary(treatment_regression))
control_estimates <- coef(summary(control_regression))

# combining the treatment and control estimates
estimates <- cbind(treatment_estimates, control_estimates)
colnames(estimates) <- c(paste0("treatment_", reference_years), paste0("control_", reference_years))