
#Change point algorithm has been applied to a multivariant time series in this script. For this
#Example, the time series includes compliance data from TFL, normalised PM2.5 and normalised NO2


#reads in compliance data based on FOI source: 
#https://tfl.gov.uk/corporate/transparency/freedom-of-information/foi-request-detail?referenceId=FOI-2045-1920

ULEZ_compliance_df = read.csv("~/Github/live_change_point_development/ULEZVehiclesCharges_PCs.csv") %>%
  rename(date = "Date.of.Travel",
         total_vehicle_number = "Unique.VRMs.Captured.in.Charging.Hours",
         compliant_number = "Unique.VRMs.Compliant.with.ULEZ",
         non_compliant_number = "Unique.VRMs.Non.Compliant.with.ULEZ") %>%
  select(c(date, total_vehicle_number, compliant_number, non_compliant_number)) %>%
  filter(total_vehicle_number!="Not Yet Calculated") %>%
  mutate(date = substring(date, 5),
         date = as.Date(date, format = "%d/%m/%Y"),
         total_vehicle_number = as.numeric(gsub(",","",total_vehicle_number)),
         compliant_number = as.numeric(gsub(",","",compliant_number)),
         non_compliant_number = as.numeric(gsub(",","",non_compliant_number)),
         compliant_percentage = (compliant_number/total_vehicle_number)*100.,
         d7_rollavg_compliant_percentage = roll_mean(compliant_percentage, n = 7, align = "right", 
                                                     fill = NA))%>%
  drop_na()


#prepared functions to be applied in the CP programme. Can this section be streamlined?

cp_urban_background_no2_df_all = normalised_urban_background_no2_all_sites_reformat %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30")) %>%
  select(date, normal_mean)%>%
  mutate(df_header = "Urban background NO2") %>%
  rename(value = normal_mean)

cp_roadside_no2_df_all = normalised_roadside_no2_all_sites_reformat %>%
  filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-07-30")) %>%
  select(date, normal_mean)%>%
  mutate(df_header = "Roadside NO2") %>%
  rename(value = normal_mean)

cp_roadside_pm25_df_ULEZ = normalised_roadside_pm25_ULEZ_reformat %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-07-30")) %>%
  select(date, normal_mean)%>%
  mutate(df_header = "Roadside PM2.5") %>%
  rename(value = normal_mean)

cp_roadside_no2_df_ULEZ = normalised_roadside_no2_ULEZ_reformat %>%
  filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-07-30")) %>%
  select(date, normal_mean)%>%
  mutate(df_header = "Roadside NO2") %>%
  rename(value = normal_mean)

compliance_statistics_cp_df = ULEZ_compliance_df  %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30")) %>%
  select(date, total_vehicle_number) %>%
  mutate(df_header = "Traffic count") %>%
  rename(value = total_vehicle_number)

#Creates multivariant TS df

ULEZ_total_cp_df = rbind(compliance_statistics_cp_df)

ULEZ_total_cp_code = unique(as.character(ULEZ_total_cp_df$df_header))

#applies CP to dataframe, in this case ULEZ

sd_v = 1
day_window = c(4,5,6,7)

ULEZ_example_detected_cps = map2_dfr(.x = day_window, .y = ULEZ_total_cp_code,
                  .f = ~multi_var_ts_gradient_cp_detection_new(df = ULEZ_total_cp_df,
                                                           .x, .y, sd_value = sd_v, date = TRUE))

filter_ULEZ_df = ULEZ_total_cp_df %>%
  filter(date >= as.Date("2019-04-01") & date <= as.Date("2019-07-30"))
mean_cp_algorithm = cpt.mean(filter_ULEZ_df$value)
mean_output = data.frame(x1 = filter_ULEZ_df$date[1], 
                         x2 = filter_ULEZ_df$date[53],
                         x3 = filter_ULEZ_df$date[121], 
                         y1 = 71.9, 
                         y2 = 67.7)

test = change_point_model_statistics(ULEZ_example_detected_cps, 25, FALSE)


ULEZ_example_coinciding_CPs = coinciding_cp_generator(ULEZ_example_detected_cps)

theme_set(theme_gray(base_size = 20))

ULEZ_summarise_stats = filter(ULEZ_example_detected_cps, variables == "3. 2nd derivative") %>% 
  group_by(window_length_level) %>% 
  summarise(mean_value = mean(value), lower = mean(value) - sd_v*sd(value), 
            upper = mean(value) + sd_v*sd(value)) %>%
  mutate(variables = "3. 2nd derivative")


ULEZ_example_detected_cps %>%
  filter(date >= as.Date("2019-04-01") & date <= as.Date("2019-06-30"),
         variables %in% c("1. Input dataset", "3. 2nd derivative", "diff")) %>%

  ggplot(aes(x = date, y = value)) +
  geom_line(aes(colour=variables), lwd = 1.5)+
  geom_vline(data = filter(ULEZ_example_detected_cps_window_sen,
                           cp==TRUE, date >= as.POSIXct("2019-04-01"),
                           variables == "1. Input dataset"),
             aes(xintercept = date), size  = 1, colour = "blue")+
  labs(x= "Date", y = "Various Units", colour = "Variables")+

  geom_hline(data = filter(ULEZ_summarise_stats, variables == "3. 2nd derivative"),
             aes(yintercept = mean_value), size  = 1, colour = "red")+
  geom_hline(data = filter(ULEZ_summarise_stats, variables == "3. 2nd derivative"),
             aes(yintercept = lower), size  = 0.8, colour = "red",
             linetype = 2)+
  geom_hline(data = filter(ULEZ_summarise_stats, variables == "3. 2nd derivative"),
             aes(yintercept = upper), size  = 0.8, colour = "red",
             linetype = 2) +
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y")


#Just mean scheme

ULEZ_example_detected_cps %>%
  filter(date >= as.Date("2019-04-01") & date <= as.Date("2019-07-30"), 
         variables == "Input dataset") %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line(aes(colour=df_label), lwd = 2)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y1), colour = "black", data = mean_output, lwd = 1.2)+
  geom_segment(aes(x = x2, y = y2, xend = x3, yend = y2), colour = "black", data = mean_output, lwd = 1.2)+
  annotate("rect", xmin = as.POSIXct("2019-04-08"), 
           xmax = as.POSIXct("2019-07-30"), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  labs(x= "Date", colour = "ULEZ data sources")+ylab(quickText("Normalised NO2 (ug/m3)"))+
  facet_grid(df_label~., scales = "free_y")+
  ggtitle("Applied CPD example - ULEZ")

#With both mean scheme and new scheme

ULEZ_example_detected_cps %>%
  filter(date >= as.Date("2019-04-01") & date <= as.Date("2019-07-30"), 
         variables == "Input dataset") %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line(aes(colour=df_label), lwd = 2)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y1), colour = "black", data = mean_output, lwd = 1.2)+
  geom_segment(aes(x = x2, y = y2, xend = x3, yend = y2), colour = "black", data = mean_output, lwd = 1.2)+
  geom_vline(data = filter(ULEZ_example_detected_cps,
                           cp==TRUE & date >= as.POSIXct("2019-04-01") & variables %in% c("Input dataset")),
             aes(xintercept = date), colour = "blue", size = 1.2)+
  geom_vline(data = filter(ULEZ_example_detected_cps,
                           cp==TRUE & date >= as.POSIXct("2019-04-01") & variables %in% c("Input dataset")
                           & date %in% ULEZ_example_coinciding_CPs),
             aes(xintercept = date), colour = "red", size = 1.2)+
  annotate("rect", xmin = as.POSIXct("2019-04-08"), 
           xmax = as.POSIXct("2019-07-30"), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  labs(x= "Date", colour = "ULEZ data sources")+ylab(quickText("Normalised NO2 (ug/m3)"))+
  facet_grid(df_label~., scales = "free_y")+
  ggtitle("Applied CPD example - ULEZ")


test %>%
  filter(date >= as.Date("2019-04-01") & date <= as.Date("2019-07-30")) %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line(aes(colour=variables), lwd = 2)+
  labs(x= "Date", colour = "ULEZ data sources")+ylab(quickText("Normalised pollutants (ug/m3)"))+
  ggtitle("Applied CPD example - ULEZ")


filter(ULEZ_example_detected_cps, variables == "2nd derivative")%>% group_by(window_length_level) %>% 
  summarise(mean(value), mean(value) - sd_v*sd(value), mean(value) + sd_v*sd(value))
