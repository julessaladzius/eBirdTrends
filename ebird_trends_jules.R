# Load libraries (install them if necessary)
library(dplyr) # you can install the dplyr package using install.packages("dplyr")
library(ggplot2)
library(rnaturalearth)
library(sf)
library(terra)
library(ebirdst)
library(tidyverse)
setwd("C:/Users/SaladziusJ/OneDrive - EC-EC/Ontario-Trend-Comparison")



#List of birds from bbs (called list_bbs1)
bbs_data <- read.csv("data/BBS_trend_estimates/Ontario_trends2002_2022.csv")
list_bbs <- unique(bbs_data[,31])
list_bbs1 <- data.frame(list_bbs)

#list of birds from ebird
list_ebird <- trends_runs <- ebirdst_runs |> 
  filter(has_trends) |> 
  select(common_name)

#which one are in both (198/215)
list_final <- subset(list_bbs1, list_bbs %in% list_ebird$common_name )

# species not in the list_final because not found in list_ebirds
not_present <- subset(list_bbs1, !(list_bbs %in% list_ebird$common_name))


                        
#setting in correct format the list of birds
list_f <- c(list_final[1:5,])

#n being total number of birds on bird_list, run that after General section
bird_list <- list_f

#--------------s-t-a-r-t---------------------------


#General
{
n=5
results <- data.frame(matrix(ncol = 5, nrow = 4*n))
colnames(results) <- c('BCRNAME', 'abd_ppy_median','abd_ppy_lower', 'abd_ppy_upper','bird_id')
study_region <- read_sf("data/BCR_shapefiles/BCR_Terrestrial_master.shp")
study_region$BCR_name <- paste0(study_region$BCR," - ",study_region$BCRNAME)
study_region <- subset(study_region, PROVINCE_S == "ONTARIO" & WATER=='3')
study_region <- st_transform(study_region, crs = 4326)
bird_list <- c("Black-capped Chickadee","American Robin","American Goldfinch")
i=0
}

for (i in 0:(n-1)) {


my_species <- bird_list[i+1]
ebirdst_download_trends(my_species)

{
trends_bkcchi    <-  load_trends(my_species, fold_estimates = TRUE)
trends_bkcchi_sf <-  st_as_sf(trends_bkcchi, 
                              coords = c("longitude", "latitude"), 
                              crs = 4326)

#add corresponding region column
trends_bkcchi_sf <- st_join(trends_bkcchi_sf, study_region, left = FALSE)

# abundance-weighted average trend by region and fold.(long part)
trends_region_folds <- trends_bkcchi_sf |>
  st_drop_geometry() |>
  group_by(BCRNAME, fold) |>
  summarize(abd_ppy = sum(abd * abd_ppy) / sum(abd),
            .groups = "drop")

# summarize across folds for each region. La conclusion qu'on veut save
block <- trends_region_folds |> 
  group_by(BCRNAME) |>
  summarise(abd_ppy_median = median(abd_ppy, na.rm = TRUE),
            abd_ppy_lower = quantile(abd_ppy, 0.10, na.rm = TRUE),
            abd_ppy_upper = quantile(abd_ppy, 0.90, na.rm = TRUE),
            .groups = "drop") |> 
  arrange(abd_ppy_median)

block$bird_id <- my_species

results[(4*i+1):(4*i+4),] <- block[1:4,]

}

}

#----------------E-N-D--------------------------------------





#VISUALISATION, NOT IF TABLE IS TOO TOO BIG, 
trends_region_sf <- left_join(study_region, results, by = "BCRNAME")
ggplot(trends_region_sf) +
  geom_sf(aes(fill = abd_ppy_median)) +
  scale_fill_distiller(palette = "Reds", 
                       limits = c(NA, 0), 
                       na.value = "grey80") +
  guides(fill = guide_colorbar(title.position = "top", barwidth = 15)) +
  labs(title = "Breeding Trends of Selected Species in Ontario (2012-2022)",
       fill = "Relative Abundance Trend [% Change / Year]") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~ bird_id)



#old code, useless.
table <- matrix(ncol = 5, nrow = 0)
colnames(table) <- c('BCRNAME', 'abd_ppy_median','abd_ppy_lower', 'abd_ppy_upper','bird_id')

table <- rbind(table,block)
