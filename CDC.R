# Library
library(vroom)
library(tidyverse)
library(ggplot2)
library(reshape2) # to flip the data frame

dat <- vroom(file = "CDC_example_data.txt", delim= "\t", show_col_types = F)

# Function 1: to extract data for each CDC level
extract_data <- function(data, cdc_level) {
  data %>% filter(`Sample Type` == cdc_level) %>%  select(c('GUANIDINOACETIC', 'GLY', 'ALA', 'VAL', 'ISOLEU_LEU_OHPRO', 'ORN', 
                                                     'MET', 'PHE', 'GLYCYL_PROLINE', 'ARG', 'CITRULLINE', 'TYR',
                                                     "FREE CARN" , "C2 CARN", "C3 CARN", "C4 CARN", "C5:1 CARN",
                                                     "C5 CARN", "OH C4 CARN", "C6 CARN", "OH C5 CARN", "C8 CARN",
                                                     "C3 DC CARN", "C10 CARN", "C5 DC CARN", "C14:1 CARN", "C14 CARN",
                                                     "C16 CARN", "OHC16 CARN", "C18 CARN"))
}

# Function 2: to draw plots
draw_plot <- function(data, cdc_name) {
  # Convert columns to rows
  data_melted <- melt(data, variable.name = "Variable", value.name = "Value")
  
  # Drawing boxplots
  
  ggplot(data_melted, aes(x = Variable, y = Value)) +
    geom_boxplot(alpha= 1, color = "darkgreen", fill = "white",
                 outlier.alpha= 1, outlier.color = "red", outlier.shape = 16, outlier.size = 1.5) +
    facet_wrap(~ Variable, scales = "free") +
    labs(title = paste("Boxplots for", cdc_name), x= "Analytes", y = "Value") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      strip.background = element_rect(fill = "lightgray"),
      strip.text = element_text(face = "bold"))
  
}

########## Analysis from here ##############

# For CDC-A
CDC_A <- extract_data(data = dat,
                      cdc_level = "A")
draw_plot(data = CDC_A,
          cdc_name = "CDC_A")
# 9x12 pdf

# For CDC-B
CDC_B <- extract_data(data = dat,
                      cdc_level = "B")

draw_plot(data = CDC_B,
          cdc_name = "CDC_B")


# For CDC-C
CDC_C <- extract_data(data = dat,
                      cdc_level = "C")
draw_plot(data= CDC_C,
          cdc_name = "CDC_C")


# For CDC-D
CDC_D <- extract_data(data = dat,
                      cdc_level = "D")
draw_plot(data = CDC_D,
          cdc_name = "CDC-D")




