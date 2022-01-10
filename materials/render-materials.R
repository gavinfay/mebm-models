# load packages ----------------------------------------------------------------

library(fs)
library(tidyverse)
library(rmarkdown)
#library(xaringan)

# non-xaringan -----------------------------------------------------------------

rmds <- dir_info(recurse = 3, glob = "materials/*.Rmd") %>% 
  filter(!str_detect(path, "slides")) %>%
  pull(path)
walk(rmds, render)

# # xaringan ---------------------------------------------------------------------
# 
# xaringans <- dir_info(recurse = 3, glob = "course-materials/*.Rmd") %>% 
#   filter(str_detect(path, "slides")) %>%
#   filter(!str_detect(path, "setup")) %>%
#   #filter(!str_detect(path, "u3-")) %>%
#   filter(str_detect(path, "u4-")) %>%
#   #filter(!str_detect(path, "u5-")) %>%
#   #filter(!str_detect(path, "u5-")) %>%
#   # filter(!str_detect(path, "u2-d11")) %>%
#   # filter(!str_detect(path, "u2-d14")) %>%
#   # filter(!str_detect(path, "u2-d15")) %>%
#   # filter(!str_detect(path, "u2-d16")) %>%
#   # filter(!str_detect(path, "u2-d17")) %>%
#   # filter(!str_detect(path, "u2-d18")) %>%
#   # filter(!str_detect(path, "u2-d19")) %>%
#   # filter(!str_detect(path, "u2-d2")) %>%
#   #filter(str_detect(path, "u2-d13")) %>%
#   pull(path)
# walk(xaringans, render)
# 
# 
# xaringans <- dir_info(recurse = 3, glob = "course-materials/*.Rmd") %>% 
#   filter(str_detect(path, "fdd")) %>%
#   pull(path)
# map(9:length(xaringans),~walk(xaringans[[.x]], render))
