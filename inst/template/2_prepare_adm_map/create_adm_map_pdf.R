# ========================================================================================
# Project:  ssid
# Subject:  Create adm maps
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================


# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here::here("working_paper/scripts/1_model_setup/set_model_parameters.r"))


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

adm <- readRDS(file.path(param$model_path, glue("adm/adm_{param$iso3c}.rds")))


# ========================================================================================
# PROCESS -------------------------------------------------------------------------------
# ========================================================================================

# Create adm1 map
adm1 <- adm %>%
  group_by(adm1_code, adm1_name) %>%
  summarize(geometry = st_union(geometry),
            .groups = "drop")

# Create adm2 map
adm2 <- adm %>%
  group_by(adm2_code, adm2_name) %>%
  summarize(geometry = st_union(geometry),
            .groups = "drop")

library(RColorBrewer)

# ========================================================================================
# CREATE ADM1 MAP ------------------------------------------------------------------------
# ========================================================================================

# Labels at the centre of adm
adm1_name <- st_centroid(adm1)
adm1_name <- cbind(adm1, st_coordinates(st_centroid(adm1$geometry)))

adm1_name <- adm1_name %>%
  mutate(poly_area = as.numeric(st_area(.)/10000)) %>%
  group_by(adm1_name) %>%
  filter(poly_area == max(poly_area, na.rm = T)) # Select largest area to filter our labels on small islands

# Increase number of colours in palette
colourcount = length(unique(adm1_name$adm1_name))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot
adm1_plot <- ggplot() +
  geom_sf(data = adm1, colour = "grey30", aes(fill = adm1_name)) +
  scale_fill_manual(values = getPalette(colourcount)) +
  geom_text(data= adm1_name, aes(x = X, y = Y, label = adm1_name),
            check_overlap = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = "transparent"), plot.title = element_text(hjust = 0.5)) +
  #theme_void(base_size = 14) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(fill = "", x = "", y = "",
       title = paste(countrycode(param$iso3c, "iso3c", "country.name"), "ADM1", sep = " ")) +
  guides(fill = "none")


# ========================================================================================
# CREATE ADM2 MAP ------------------------------------------------------------------------
# ========================================================================================

# Labels at the centre of adm
adm2_name <- st_centroid(adm2)
adm2_name <- cbind(adm2, st_coordinates(st_centroid(adm2$geometry)))

adm2_name <- adm2_name %>%
  mutate(poly_area = as.numeric(st_area(.)/10000)) %>%
  group_by(adm2_name) %>%
  filter(poly_area == max(poly_area, na.rm = T)) # Select largest area to filter our labels on small islands


# Increase number of colours in palette
colourcount = length(unique(adm2$adm2_name))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot
adm2_plot <- ggplot() +
  geom_sf(data = adm2, colour = "grey30", aes(fill = adm2_name)) +
  scale_fill_manual(values = getPalette(colourcount)) +
  geom_text(data= adm2_name, aes(x = X, y = Y, label = adm2_name),
            check_overlap = FALSE, size = 1.5) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = "transparent"), plot.title = element_text(hjust = 0.5)) +
  #theme_void(base_size = 14) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(fill = "", x = "", y = "",
       title = paste(countrycode(param$iso3c, "iso3c", "country.name"), "ADM2", sep = " ")) +
  guides(fill = "none")


# ========================================================================================
# SAVE------------------------------------------------------------------------------------
# ========================================================================================

temp_path <- file.path(param$model_path, "/adm")
dir.create(temp_path, recursive = T, showWarnings = F)

pdf(file = file.path(temp_path, glue("adm_maps_{param$iso3c}.pdf")), width = 8.27, height = 11.69)
print(adm1_plot)
print(adm2_plot)
dev.off()

