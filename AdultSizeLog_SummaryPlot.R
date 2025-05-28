# Load required packages and set working directory --------------------------------------------------------------
rm(list=ls())

required_pckg = c("tidyverse", "sjmisc")
lapply(required_pckg, library, character.only=TRUE)

setwd("~/Desktop/R Working Directory/Databases/")

# Create function for weight/svl plots --------------------------------------------------------------
plot_size = function(y_var, s, y_title){ #x_var is the x variable, y_var is the y variable, s is the suspected sex
  y_var = sym(y_var)

  ggplot(data = weight.data %>% filter(suspected.sex == s,
                                         is.na(!!y_var) == FALSE), #filter out NA values so geom_line is continuous
         aes(y=!!y_var, x = date, colour = animal.id)) + 
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_x_date(name = "Date", limits = c(min(weight.data$date), max(weight.data$date)), date_breaks = "2 months", date_labels="%b-%Y") + 
    scale_color_discrete(name = "Animal ID") +
    #if weight is being plotted then use one scale. if svl is being plotted, use a different scale
    if(!!y_var == "weight.g"){scale_y_continuous(limits = c(40, 200), breaks = seq(40,200, by = 20), name = y_title)}else{
      scale_y_continuous(limits = c(60, 140), breaks = seq(60,140, by = 20), name = y_title)
    }
}

# Read databases --------------------------------------------------------------
weight.data <- read.csv("WoLab_AnimalCareDatabase - RM Transport.csv", header = TRUE) # colClasses = c("Date", "factor", "factor", "factor", "character", "numeric", "numeric", "character", "character"))
sex.data <- read.csv("WoLab_AnimalCareDatabase - Animal Log.csv", header = TRUE, na.strings = "NA") #, colClasses = c("Date", "factor", "factor", "factor", "factor", "factor", "factor", "character", "character", "Date", "character","character"))

# Join suspected sex from sex.data to weight.data --------------------------------------------------------------
weight.data <- weight.data %>% 
  #add suspected sex column based on animal.id
  left_join(sex.data %>% 
              group_by(animal.id) %>%
              select(suspected.sex, disposition.date),
              join_by(animal.id)) %>%
  mutate_at(vars(date), as.Date) %>% #change column classes
  mutate_at(vars(animal.id), factor) %>% #change column classes
  filter(date %nin% c("2023-10-06", "2023-11-16", "2023-12-05", "2023-12-26"),
         is.na(disposition.date) == TRUE)
         #filter out deceased individuals and rows for which the individual ID is incorrect

# Create Panel Plot that shows time series for weight and body length separately for Male, Female, and Unknown ---------------------------------------------------
ggarrange(
  ggarrange(plot_size("weight.g", "Female", "Female body weight (g)"), plot_size("svl.mm", "Female", "Female svl (mm)"),
            ncol = 2, nrow = 1, common.legend = TRUE, legend = "right"),
  ggarrange(plot_size("weight.g", "Male", "Male body weight (g)"), plot_size("svl.mm", "Male", "Male svl (mm)"),
            ncol = 2, nrow = 1, common.legend = TRUE, legend = "right"),
  ggarrange(plot_size("weight.g", "Unknown", "Unknown sex body weight (g)"), plot_size("svl.mm", "Unknown", "Unknown sex svl (mm)"), ncol = 2, nrow = 1, common.legend = TRUE, legend = "right"),
  common.legend = FALSE,
  ncol = 1,
  nrow = 3
)