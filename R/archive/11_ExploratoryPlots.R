library(ggstatsplot)
library(ggpubr)

# Load data ---------------------------------------------------------------

mydata_distDirStats <- read.csv(file.path(wd$out, "mydata_distDirStats.csv"))
mydata_transformed <- readRDS(file.path(wd$bin, "mydata_transformed.rds")) %>% 
  dplyr::filter(species == "SEBA") %>% 
  dplyr::mutate(Sex = case_when(Sex %in% c("Male", "Female") ~ Sex, TRUE ~ "NA")) 


# Summarize samples analyzed ----------------------------------------------

mydata_transformed %>% 
  dplyr::group_by(State, Sex) %>% 
  dplyr::summarise(n=n())



# Plot histogram of d2H vals ----------------------------------------------
mydata_transformed %>% 
  ggplot() +
  geom_histogram(aes(d2H), binwidth = 5, color = "grey30", fill = "grey50") +
  theme_pubclean() +
  scale_x_continuous(breaks =seq(-100,0,by=5)) +
  scale_y_continuous(expand = c(0,0.01)) +
  ggplot2::xlab(expression(paste(delta^2 ~ H[fur], " (", "\u2030", ", VSMOW)") )) +
  ggplot2::ylab(expression("Number of samples" ))


library(ggstatsplot)
ggbetweenstats(data =mydata_transformed, y=d2H, x=Sex)
ggbetweenstats(data =mydata_transformed, y=d2H, x=State)  

# Compare d2H values ------------------------------------------------------

# Between species
(p1 <- ggbetweenstats(data = mydata_distDirStats, x = species, y = d2H))
ggsave(p1, file.path(wd$figs, "d2H_bySpecies.png"))
# Between species and sex (when known)
(p2 <- mydata_distDirStats %>% 
  dplyr::filter(!is.na(Sex), Sex != "", Sex != "inconclusive") %>% 
  dplyr::mutate(species_sex = paste(species, Sex, sep = "_")) %>% 
  ggbetweenstats(data = ., x = species_sex, y = d2H))
ggsave(p2, file.path(wd$figs, "d2H_bySpecies.png"))


# Compare distances traveled ----------------------------------------------
(p3 <- mydata_distDirStats %>% 
  ggplot() +
  aes(minDist_km, group = species, color = species) +
  geom_density() +
  theme_pubclean() +
  geom_rug())
ggsave(p3, filename = file.path(wd$figs, "distancesTraveledBySpecies.png"))
