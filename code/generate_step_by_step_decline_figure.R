#------------------------------------------------------------------------------#
## Project :  Bottleneck
## Purpose :  Methods figure
##  Date   :  06/24/2020
## Author  :  Gordon Blasco
#------------------------------------------------------------------------------#

#### Libraries and Data ####
#------------------------------------------------------------------------------#
library(tidyverse)
library(here)
library(ggpubr)
# library(stargazer)
library(patchwork)
library(paletteer)
# library(colorspace) # For darken()

food_balance <- read_csv(file.path("","home","shares",
                                   "clean-seafood","data",
                                   "bottlenecks_data", "FAO",
                                   "food_balances",
                                   "food_balance_tidy.csv"))

# Load in decline functions
source(here("src", "decline_functions.R"))

# Choose time series to plot 
food_supply <- food_balance %>%
  filter(element_code == 645) %>%
  select(-element_code) %>%
  filter(area == "Australia" & item == "Mutton & Goat Meat") %>% 
  group_split(area, item_code)

#### Run decline and incline finders ####
#------------------------------------------------------------------------------#

# Prep the data with the rolling average and rolling slope
prepped <- food_supply %>%
  # Rolling average smoother
  map(., rolling_average,
      window        = 5,
      time_col      = year,
      value_col     = quantity,
      new_col       = meaned_quant) %>%
  # Rolling slope extraction
  map(., rolling_slope,
      window        = 5,
      time_col      = year,
      value_col     = meaned_quant,
      new_col       = slope) %>%
  # Remove problematic timeseries
  discard(., ~is.character(.)) %>%
  # Classifiy slopes
  map(., classify_slope,
      threshold     = 1000000,
      value_column  = meaned_quant,
      slope_column  = slope,
      new_col       = trend_char) 


# Run the event finder portion 

  # Find declines
decline_finder <- prepped %>% 
  
  map(., event_finder,
      trend_col          = trend_char,
      trend_type         = "decline",
      year_col           = year,
      forgiveness_window = 4,
      min_duration       = 10) #%>%
  
  # Run significance test
decline_sig_test <- decline_finder %>% 
  map(., significance_test, 
      event_col          = event_id,
      slope_filter       = -0.2,
      tau_filter         = -0.6) %>%
  
  # Remove timeseries with no declines after the significance test
  discard(~ all(is.na(.$event_id)))

#   # Find inclines
# incline_finder <- prepped %>% 
#   
#   map(., event_finder,
#       trend_col          = trend_char,
#       trend_type         = "incline",
#       year_col           = year,
#       forgiveness_window = 4,
#       min_duration       = 10) #%>%
# 
#   # Run significance test
# incline_sig_test <- incline_finder %>% 
#   map(., significance_test, 
#       event_col          = event_id,
#       slope_filter       = 0.2,
#       tau_filter         = 0.6) %>%
#   
#   # Remove timeseries with no declines after the significance test
#   discard(~ all(is.na(.$event_id)))
# 


#### Prep the results for plotting ####
#------------------------------------------------------------------------------#
 
  # Declines
declines_df <- decline_sig_test %>% 
  map_df(., bind_rows) %>% 
  mutate(event_fixed = if_else(!is.na(event_id), 
                               paste("Event", event_id), 
                               "No Event"),
         trends = if_else(trend_char == "decreasing", "Decreasing", "Increasing"))

decline_events_info <- declines_df %>% 
  distinct(event_fixed, .keep_all = TRUE) %>% 
  filter(event_fixed != "No Event") %>% 
  select(event_fixed, mk_tau, mk_p, mk_S, sens_slope, sens_slope_z, sens_slope_p) %>% 
  pivot_longer(-event_fixed) %>% 
  pivot_wider(names_from = event_fixed)

#stargazer(events_info, type='html', out="events.htm")
#DT::datatable(decline_events_info)

  # Inclines
# inclines_df <- incline_sig_test %>% 
#   map_df(., bind_rows) %>% 
#   mutate(event_fixed = if_else(!is.na(event_id), 
#                                paste("Event", event_id), 
#                                "No Event"),
#          trends = if_else(trend_char == "decreasing", "Decreasing", "Increasing"))
# 
# incline_events_info <- inclines_df %>% 
#   distinct(event_fixed, .keep_all = TRUE) %>% 
#   filter(event_fixed != "No Event") %>% 
#   select(event_fixed, mk_tau, mk_p, mk_S, sens_slope, sens_slope_z, sens_slope_p) %>% 
#   pivot_longer(-event_fixed) %>% 
#   pivot_wider(names_from = event_fixed)
# 
# #stargazer(events_info, type='html', out="events.htm")
# DT::datatable(incline_events_info)

  # Combine incline and decline event data

# events_df <- declines_df %>% 
#   select(area_code:trend_char, 
#          trends, 
#          decline_event_fixed = event_fixed) %>% 
#   left_join(inclines_df %>% 
#               select(area_code:trend_char, 
#                      trends, 
#                      incline_event_fixed = event_fixed) %>% 
#               mutate(incline_event_fixed = if_else(incline_event_fixed == "Event 2", "Event 3", incline_event_fixed))) %>% # Both the declines and inclines have an "Event 2" so rename
#   mutate(event_fixed = if_else(decline_event_fixed == "No Event" & incline_event_fixed != "No Event", incline_event_fixed, decline_event_fixed)) # Combine incline and decline events into one column


events_df <- declines_df %>% 
  select(area_code:trend_char, 
         trends, 
         decline_event_fixed = event_fixed) %>% 
  rename(event_fixed = decline_event_fixed) # Combine incline and decline events into one column


#### Make the plots ####
#------------------------------------------------------------------------------#

  # Define colors and point sizes

size_line <- 0.75
size_point <- 2.25

pal <- paletteer::paletteer_d("beyonce::X6")
# pal <- paletteer::paletteer_d("beyonce::X18")
# pal <- paletteer::paletteer_d("beyonce::X60")

scales::show_col(pal)

# Plot 1 - Raw time series
(part1 <- ggplot((prepped %>% map_df(., bind_rows)) , aes(x = year, y = quantity)) +
    geom_point(size = size_point) +
    geom_line(size = size_line) +
    labs(x = NULL, 
         y = "") +
    scale_x_continuous(breaks = seq(1960, 2010, by = 10)) +
    cowplot::theme_half_open() +
    theme(axis.text.x = element_blank())
)

#   # Plot 2 - Smoothed time series
# (part2 <- ggplot((prepped %>% map_df(., bind_rows)) , aes(x = year, y = meaned_quant)) +
#     geom_point(size = size_point) +
#     geom_line(size = size_line) +
#   labs(x = NULL, 
#        y = NULL) +
#     scale_x_continuous(breaks = seq(1960, 2010, by = 10)) +
#     #theme_bw(base_size = 14) +
#     cowplot::theme_half_open() +
#     theme(axis.text.x = element_blank())
#   )

  # Plot 2 - Smoothed time series & slope extraction
(part2 <- ggplot(events_df , aes(x = year, y = meaned_quant)) +
    geom_line(size = size_line) +
    geom_point(aes(color = trends), size = size_point) +
    scale_x_continuous(breaks = seq(1960, 2010, by = 10)) +
    scale_color_manual(values = c("Increasing" = pal[5],
                                  "Decreasing" = pal[2])) +
    labs(x = NULL, 
         y = bquote(Food~supply~(kg~capita^-1~yr^-1))) +
    cowplot::theme_half_open() +
    theme(axis.text.x = element_blank())
)

  # Plot 3 - Event finder 
(part3 <- events_df %>% 
    mutate(event_fixed = if_else(event_fixed == "No Event", "No Event", "Decline Event")) %>% 
    ggplot(aes(x = year, y = meaned_quant)) +
    geom_line(size = size_line) + 
    geom_point(aes(color = factor(event_fixed)), size = size_point) +
    scale_x_continuous(breaks = seq(1960, 2010, by = 8)) +
    scale_color_manual(values = c("Decline Event" = pal[3],
                                  "No Event" = "black")) +
    labs(x = "Year",
         y = "",
         color = NULL) +
    cowplot::theme_half_open()
  )



# Patch the plots together, set common scales and themes, and add tags
(final_plot <- part1/part2/part3 &
    scale_y_continuous(limits = c(0, 50)) &
    theme(legend.title = element_blank(), # Remove legend titles
          legend.position = c(0.8,0.9), # Position legend inside plot
          plot.tag.position = c(0.1,1)) & # Move plot tags to left of y axis title
    plot_annotation(tag_levels = "a")
)

# ggarrange(part1, part2, part3,
#           nrow = 3,
#           align = "hv",
#           labels = "auto",label.x = 0.1)


# Save
ggsave(filename = here::here("output", "figs", "declines_ms", "fig_event_finder_methods.png"), plot = final_plot, width = 7, height = 6, dpi = 600)

###################
# PLaying around with a conceptual figure

library(ggbump)
library(ggtext)

# Function describing generalized S-curve
sigmoid <- function(x, s) {
  s / (1 + exp(-x))
}

# Build dataset of theoretical curves
data <- tibble(
  x = seq(-5, 5, 0.01),
  incline = sigmoid(x, s = 1),
  decline = rev(sigmoid(x, s = 1)))

data <- data %>% 
  mutate(decline_lag = lag(decline, 300, default = max(decline)),
         incline_lag = lag(incline, 300, default = min(decline)))

data <- data %>% 
  pivot_longer(cols = starts_with(c("incline", "decline")), names_to = "curve", values_to = "value")

pal

# Decline lags
(decline_lags <- data %>% 
  filter(curve %in% c("incline", "decline_lag")) %>% 
  ggplot(aes(x = x, y = value, color = curve)) +
  geom_line(size = size_line) +
  annotate("richtext", x = -5, y = 0.94, label = "<b span style='color:#226060FF'>Existing food</span b>", size = 5, hjust = 0, fill = NA, label.color = NA) +
  annotate("richtext", x = -5, y = 0.06, label = "<b span style='color:#BE7245FF'>Novel food</span b>", size = 5, hjust = 0, fill = NA, label.color = NA) +
  scale_color_manual(values = c(pal[2], pal[5])) +
  labs(x = paste("Time", "\U2192"),
       y = paste("Consumption per capita", "\U2192"),
       color = NULL)  +
  cowplot::theme_half_open() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
)

# Decline leads 
(decline_leads <- data %>% 
  filter(curve %in% c("decline", "incline_lag")) %>% 
  ggplot(aes(x = x, y = value, color = curve)) +
  geom_line(size = size_line) +
  labs(x = paste("Time", "\U2192"),
       y = paste("Consumption per capita", "\U2192"),
       color = NULL) +
  cowplot::theme_half_open() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
)

ggpubr::ggarrange(decline_lags, decline_leads, nrow = 2, labels = "AUTO")

