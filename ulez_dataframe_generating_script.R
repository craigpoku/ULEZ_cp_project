#Script used to generate observed vs modelled pollutants based on set
#case study. Parameters chosen to account for bias-variance trade off.
#Note, overfitting has also been tested.

#Processing code----------------------------------------------

london_kcl_meta = importMeta(source = "kcl") %>%
  drop_na()

london_urb_sites <- filter(
  kcl_noaa_nearest,
  site_type %in% c("Urban Background", "Roadside"))

directory_met_data = "D:/cpdav/UK_met_data/noaa_UK_met_data_"

met_london_df_all = map2_dfr(.x = london_urb_sites$code, 
                    .y = london_urb_sites$met_code,
                    .f = ~read_met_sites_london(site_code = .x, metcode = .y, 
                                                "2016-01-01", "2020-12-31"))

#preparing data to be used in random forest algorithm

met_aq_london_urban_background_no2=met_aq_prepared_rm(met_london_df_all, 
                                               "no2", "Urban Background")

met_aq_london_roadside_no2=met_aq_prepared_rm(met_london_df_all, 
                                                   "no2", "Roadside")

met_aq_london_roadside_pm25=met_aq_prepared_rm(met_london_df_all, 
                                                    "pm25", "Roadside")

London_code_no2_urban_background = unique(as.character(met_aq_london_urban_background_no2$code))
London_code_no2_roadside = unique(as.character(met_aq_london_roadside_no2$code))
London_code_pm25_roadside = unique(as.character(met_aq_london_roadside_pm25$code))

#sites that were included in the ULEZ - note, shape file unavailable was done manually

ULEZ_no2_urban_background_sites = c("BL0", "CT3", "KC1", "WM0")
ULEZ_no2_urban_roadside_sites = c("CD9", "CT4", "CT6", "NB1") 
ULEZ_pm25_urban_roadside_sites = c("CD9") 


#Note, ULEZ was initially implemented 8th April 2019, ran code a month prior to implementation


normalised_urban_background_no2_london = map(.x = London_code_no2_urban_background,
                                            .f = ~normalised_concentration_and_BAU_observed_combined_df(df = met_aq_london_urban_background_no2,
                                                            site = .x, 300, "2016-01-01", "2019-12-31", 0.85, 300))

normalised_roadside_no2_london = map(.x = London_code_no2_roadside,
                                             .f = ~normalised_concentration_and_BAU_observed_combined_df(df = met_aq_london_roadside_no2,
                                                            site = .x, 300, "2016-01-01", "2019-12-31", 0.85, 300))

normalised_roadside_pm25_london = map(.x = London_code_pm25_roadside,
                                     .f = ~normalised_concentration_and_BAU_observed_combined_df(df = met_aq_london_roadside_pm25,
                                                                         site = .x, 300, "2016-01-01", "2019-12-31", 0.85, 300))



normalised_urban_background_no2_ULEZ_reformat = 
  reformat_random_forest_df_output_statistics_added(normalised_urban_background_no2_london,
                                       London_code_no2_urban_background, ULEZ_no2_urban_background_sites,
                                       normal=TRUE, site_subset=TRUE)

normalised_urban_background_no2_all_sites_reformat = 
  reformat_random_forest_df_output_statistics_added(normalised_urban_background_no2_london,
                                        London_code_no2_urban_background, ULEZ_no2_urban_background_sites,
                                        normal=TRUE, site_subset=FALSE)

normalised_roadside_no2_ULEZ_reformat = 
  reformat_random_forest_df_output_statistics_added(normalised_roadside_no2_london,
                                        London_code_no2_roadside, ULEZ_no2_urban_roadside_sites,
                                        normal=TRUE, site_subset=TRUE)

normalised_roadside_no2_all_sites_reformat = 
  reformat_random_forest_df_output_statistics_added(normalised_roadside_no2_london,
                                        London_code_no2_roadside, ULEZ_no2_urban_roadside_sites,
                                        normal=TRUE, site_subset=FALSE)

normalised_roadside_pm25_ULEZ_reformat = 
  reformat_random_forest_df_output_statistics_added(normalised_roadside_pm25_london,
                                        London_code_pm25_roadside, ULEZ_pm25_urban_roadside_sites,
                                        normal=TRUE, site_subset=TRUE)

normalised_roadside_pm25_all_sites_reformat = 
  reformat_random_forest_df_output_statistics_added(normalised_roadside_pm25_london,
                                        London_code_pm25_roadside, ULEZ_pm25_urban_roadside_sites,
                                        normal=TRUE, site_subset=FALSE)






