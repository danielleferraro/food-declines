# Sets file paths for commonly used directories on the brewfish/aquaculture Github repo as well as the NCEAS Aurora server

# # Aurora server data directories
dir_server <- file.path("","home","shares","clean-seafood") # server folder
dir_data_aquaculture <- file.path("","home","shares","clean-seafood","data","aquaculture_data") # where Track 1 clean/final data are saved (on server)
dir_data_bottlenecks <- file.path("","home","shares","clean-seafood","data", "bottlenecks_data") # where Track 2 clean/final data are saved (on server)
dir_raw_data <- file.path("","home","shares","clean-seafood","raw_data") # where raw data is saved (on server)
dir_iucn <- file.path("","home","shares","aquaculture","ARF","iucn") # where iucn data is saved (on server)
# # R Project directories
dir_scripts <- here::here("scripts") # where scripts are kept (in project)
dir_explore <- here::here("explore") # where explore files are kept (in project)

# # Once defined, the safest way to call a something within
# # one of the below directories in your code is as follows:
# # e.g. file.path(dir_raw_data, "name_of_subdirectory", "name_of_file.csv"

