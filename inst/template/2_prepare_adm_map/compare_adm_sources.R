# ========================================================================================
# Project:  sidd
# Subject:  Script to compare adm map and adm info in survey
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SET MODEL PARAMETERS -------------------------------------------------------------------
# ========================================================================================

source(here::here("working_paper/scripts/model_setup/set_model_parameters.r"))


# ========================================================================================
# NOTES ---------------------------------------------------------------------------------
# ========================================================================================

# Note that sometimes adm_code only contains a nested code (e.g. district # 10 in region # 1,
# and district # 10 in region # 3), which means that they are not unique. In this case, different
# levels of adm_code need to be combined.
# Also note that the same names of districts, etc might occur within different regions and
# region names often have different spelling between sources. Therefore always try to join
# fields using unique adm_code.


############## CHECK MAP SEND by SYED IF IT MATCHES WITH HIES2016


### ADD CODE TO COMPARE WITH IPUMS!!
# adm 1
check_adm1 <- full_join(
  adm %>%
    st_drop_geometry() %>%
    dplyr::select(adm1_name) %>%
    mutate(source1 = "map") %>%
    unique(),
  ipums %>%
    dplyr::select(adm1_name) %>%
    mutate(source2 = "ipums") %>%
    unique()
)


# ========================================================================================
# ADM LIST FOR SURVEY --------------------------------------------------------------------
# ========================================================================================

# adm information
adm <- read_sav(file.path(param$db_path,
                          glue("household_surveys/{param$iso3c}/hies_2016/data/hh_sec_a.sav")))


# add BGD iso3c country code (050) and leading zeros so lenght of id is always 3
adm_list_survey <- adm %>%
  transmute(
    adm1_name = id_01_name,
    adm1_code = stringr::str_pad(id_01_code, 3, side = "left", pad = 0),
    adm1_code = glue("050{adm1_code}"),
    adm2_name = id_02_name,
    adm2_code = stringr::str_pad(id_02_code, 3, side = "left", pad = 0),
    adm2_code = glue("{adm1_code}{adm2_code}"),
    adm3_name = id_03_name,
    adm3_code = stringr::str_pad(id_03_code, 3, side = "left", pad = 0),
    adm3_code = glue("{adm2_code}{adm3_code}"),
    adm4_name = id_04_name,
    adm4_code = id_04_code,
    adm5_name = id_05_name,
    adm5_code = id_05_code) %>%
  dplyr::select(adm1_code, adm1_name, adm2_code, adm2_name, adm3_code, adm3_name) %>%
  unique()

n_distinct(adm_list_survey$adm1_code)
n_distinct(adm_list_survey$adm1_name)

n_distinct(adm_list_survey$adm2_code)
n_distinct(adm_list_survey$adm2_name)

n_distinct(adm_list_survey$adm3_code)
n_distinct(adm_list_survey$adm3_name) # Note that there must be duplicate adm names! So link on code!


# ========================================================================================
# ADM LIST FOR MAP 1 ---------------------------------------------------------------------
# ========================================================================================

# IPUMS adm2 world map
adm1_wld <- st_read(file.path(param$db_path, "ipums/shapefiles/world_geolev1_2019/world_geolev1_2019.shp"))
adm2_wld <- st_read(file.path(param$db_path, "ipums/shapefiles/world_geolev2_2019/world_geolev2_2019.shp"))

# Process
adm1_source1 <- adm1_wld %>%
  filter(CNTRY_CODE == paste0("0", countrycode(param$iso3c, "iso3c", "un"))) %>%
  transmute(adm0_code = param$iso3c,
            adm1_name = toupper(ADMIN_NAME),
            adm1_code = GEOLEVEL1) %>%
  st_make_valid()

adm2_source1 <- adm2_wld %>%
  filter(CNTRY_CODE == paste0("0", countrycode(param$iso3c, "iso3c", "un"))) %>%
  transmute(adm0_code = param$iso3c,
            adm2_name = toupper(ADMIN_NAME),
            adm2_code = GEOLEVEL2) %>%
  st_make_valid() # added to solve potential problems with 'Ring Self-intersection'

# Link adm1 code and name
adm2_source1 <- adm2_source1 %>%
  mutate(adm1_code = str_sub(adm2_code, 1, nchar(adm2_code)-3)) %>%
  left_join(st_drop_geometry(adm1_source1)) %>%
  dplyr::select(adm0_code, adm1_name, adm1_code, adm2_name, adm2_code)

adm_list_map1 <- adm2_source1 %>%
  st_drop_geometry()
rm(adm1_source1, adm2_source1)

n_distinct(adm_list_map1$adm1_code)
n_distinct(adm_list_map1$adm1_name)

n_distinct(adm_list_map1$adm2_code)
n_distinct(adm_list_map1$adm2_name)


# ========================================================================================
# ADM LIST FOR MAP 2 ---------------------------------------------------------------------
# ========================================================================================

# Unknown map
adm_source2 <- st_read(file.path(param$db_path, glue("adm/{param$iso3c}/bgd_3cod_Q1/bgd_3cod_Q1.shp")))

# Process
adm_list_map2 <- adm_source2 %>%
  st_drop_geometry() %>%
  transmute(adm0_code = param$iso3c, adm1_name = toupper(admin1Name), adm1_code = admin1Pcod,
            adm2_name = toupper(admin2Name), adm2_code = admin2Pcod, adm3_name = toupper(admin3Name),
            adm3_code = admin3Pcod)

n_distinct(adm_list_map2$adm1_code)
n_distinct(adm_list_map2$adm1_name)

n_distinct(adm_list_map2$adm2_code)
n_distinct(adm_list_map2$adm2_name)

n_distinct(adm_list_map2$adm3_code)
n_distinct(adm_list_map2$adm3_name)


# ========================================================================================
# ADM LIST FOR MAP 3 ---------------------------------------------------------------------
# ========================================================================================

# Map from https://data.humdata.org/dataset/cod-ab-bgd
adm_source3 <- st_read(file.path(param$db_path, glue("adm/{param$iso3c}/bgd_adm_bbs_20201113_SHP/bgd_admbnda_adm3_bbs_20201113.shp")))

# Process
adm_list_map3 <- adm_source3 %>%
  st_drop_geometry() %>%
  transmute(adm0_code = param$iso3c, adm1_name = toupper(ADM1_EN), adm1_code = ADM1_PCODE,
            adm2_name = toupper(ADM2_EN), adm2_code = ADM2_PCODE, adm3_name = toupper(ADM3_EN),
            adm3_code = ADM3_PCODE)

n_distinct(adm_list_map2$adm1_code)
n_distinct(adm_list_map2$adm1_name)

n_distinct(adm_list_map2$adm2_code)
n_distinct(adm_list_map2$adm2_name)

n_distinct(adm_list_map2$adm3_code)
n_distinct(adm_list_map2$adm3_name)


# ========================================================================================
# ADM LIST FOR MAP 4 ---------------------------------------------------------------------
# ========================================================================================

# Map supplied by Syed Islam
# this map seems rather messy - we do not use it
adm_source4 <- st_read(file.path(param$db_path, glue("adm/{param$iso3c}/Bangladesh_Admin_Boundary-20220217T082840Z-001/BD_Upazila_WGS84.shp")))

# Process
adm_list_map4 <- adm_source4 %>%
  st_drop_geometry()


# ========================================================================================
# ADM LIST FOR MAP 5 ---------------------------------------------------------------------
# ========================================================================================

# IPUMS BGD 2011 map
# The map with the adms as defined in the population census of 2011
adm1_source5 <- st_read(file.path(param$db_path, glue("adm/{param$iso3c}/geo1_bd2011/geo1_bd2011.shp")))
adm2_source5 <- st_read(file.path(param$db_path, glue("adm/{param$iso3c}/geo2_bd2011/geo2_bd2011.shp")))

# Process
adm1_list_map5 <- adm1_source5 %>%
  st_drop_geometry() %>%
  transmute(adm1_code = IPUM2011, adm1_name = toupper(ADMIN_NAME))

adm_list_map5 <- adm2_source5 %>%
  st_drop_geometry() %>%
  transmute(adm1_code = PARENT, adm2_name = toupper(ADMIN_NAME), adm2_code = ZILA2011) %>%
  left_join(adm1_list_map5)



# ========================================================================================
# COMPARE --------------------------------------------------------------------------------
# ========================================================================================

# SURVEY AND MAP 1 -----------------------------------------------------------------------
# adm 1
check1_adm1 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm1_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map1 %>%
    dplyr::select(adm1_name) %>%
    mutate(source2 = "map1") %>%
    unique()
)

# adm 2
check1_adm2 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm2_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map1 %>%
    dplyr::select(adm2_name) %>%
    mutate(source2 = "map1") %>%
    unique()
)

# SURVEY AND MAP 2 -----------------------------------------------------------------------

# adm 1
check2_adm1 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm1_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map2 %>%
    dplyr::select(adm1_name) %>%
    mutate(source2 = "map2") %>%
    unique()
)

# adm 2
# Apply a few fixes in the names
adm_list_survey <- adm_list_survey %>%
  mutate(adm2_name = case_when(
    adm2_name == "BRAHMANBARIA" ~ "BRAHAMANBARIA",
    adm2_name == "KISHOREGONJ" ~ "KISHOREGANJ",
    adm2_name == "CHAPAI NABABGANJ" ~ "NAWABGANJ",
    TRUE ~ adm2_name)
  )

check2_adm2 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm2_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map2 %>%
    dplyr::select(adm2_name) %>%
    mutate(source2 = "map2") %>%
    unique()
)

# adm 3
# We match on adm2_name and adm3_name as adm3_names contain duplicates
# Seems map2 contains additional districts not included in the survey
check2_adm3 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm2_name, adm3_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map2 %>%
    dplyr::select(adm2_name, adm3_name) %>%
    mutate(source2 = "map2") %>%
    unique()
) %>%
  filter(is.na(source1) | is.na(source2))


# SURVEY AND MAP 3 -----------------------------------------------------------------------

# This map is the same as map2, we prefer this one has we know its source

# adm 1
check3_adm1 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm1_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map3 %>%
    dplyr::select(adm1_name) %>%
    mutate(source2 = "map3") %>%
    unique()
)

# adm 2
# Apply a few fixes in the names
adm_list_survey <- adm_list_survey %>%
  mutate(adm2_name = case_when(
    adm2_name == "BRAHMANBARIA" ~ "BRAHAMANBARIA",
    adm2_name == "KISHOREGONJ" ~ "KISHOREGANJ",
    adm2_name == "CHAPAI NABABGANJ" ~ "NAWABGANJ",
    TRUE ~ adm2_name)
  )

check3_adm2 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm2_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map3 %>%
    dplyr::select(adm2_name) %>%
    mutate(source2 = "map3") %>%
    unique()
)


# adm 3
# We match on adm2_name and adm3_name as adm3_names contain duplicates
# Seems map3 contains additional districts not included in the survey
check3_adm3 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm2_name, adm3_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map3 %>%
    dplyr::select(adm2_name, adm3_name) %>%
    mutate(source2 = "map3") %>%
    unique()
) %>%
  filter(is.na(source1) | is.na(source2))


# SURVEY AND MAP 5 -----------------------------------------------------------------------

# adm 1
check5_adm1 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm1_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map5 %>%
    dplyr::select(adm1_name) %>%
    mutate(source2 = "map5") %>%
    unique()
)

# adm 2
# Apply a few fixes in the names
adm_list_survey <- adm_list_survey %>%
  mutate(adm2_name = case_when(
    adm2_name == "BRAHMANBARIA" ~ "BRAHAMANBARIA",
    adm2_name == "KISHOREGONJ" ~ "KISHOREGANJ",
    adm2_name == "CHAPAI NABABGANJ" ~ "NAWABGANJ",
    TRUE ~ adm2_name)
  )

adm_list_map5 <- adm_list_map5 %>%
  mutate(adm2_name = case_when(
    adm2_name == "BRAHMANBARIA" ~ "BRAHAMANBARIA",
    adm2_name == "JHENAIDAHA" ~ "JHENAIDAH",
    adm2_name == "MAULVI BAZAR" ~ "MAULVIBAZAR",
    adm2_name == "CHAPAI NABABGANJ" ~ "NAWABGANJ",
    adm2_name == "NETROKONA" ~ "NETRAKONA",
    TRUE ~ adm2_name)
  )

check5_adm2 <- full_join(
  adm_list_survey %>%
    dplyr::select(adm2_name) %>%
    mutate(source1 = "survey") %>%
    unique(),
  adm_list_map5 %>%
    dplyr::select(adm2_name) %>%
    mutate(source2 = "map5") %>%
    unique()
)


