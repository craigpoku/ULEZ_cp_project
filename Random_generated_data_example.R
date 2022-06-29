#example script that was used to test CPD debugging

x = c(0, 300, 600, 900, 1200, 1500)
f_x = c(-5, 8, 1, 2, 3, 6)
g_x = c(-5, 8, 1, 2, 3, 4.5)



plot(x, f_x, type = "l")
plot(x, g_x, type = "l")


fun_1_df = linear_function_generator(x, f_x, tail(x, n=1), 0.01) %>%
  mutate(df_header = "test")
fun_2_df = linear_function_generator(x, g_x, tail(x, n=1), 0.01)

plot(fun_1_df$value, type = "l")

win_length_vector = c(25)

v_val = 1.960
sd_v  = 1

fun_1_cp_detection = map_dfr(.x = win_length_vector,
                             .f = ~multi_var_ts_gradient_cp_detection_new(df = fun_1_df,
                                                           .x, "test", z_value = v_val, date = FALSE))
fun_1_cp_detection_example = map_dfr(.x = 60,
                             .f = ~window_length_constrain(df = fun_1_df,
                                                           .x, cp_factor = 1e2,
                                                           epsilon = 1e-9, date = FALSE))

random_example_summarise_stats = filter(fun_1_cp_detection, variables == "3. 2nd derivative") %>% 
  group_by(window_length_level) %>% 
  summarise(mean_value = mean(value, na.rm  = TRUE),
            sd_value = sd(value, na.rm  = TRUE),
            median_value = median(value, na.rm  = TRUE),
            lower = mean(value) - sd_v*sd(value), 
            upper = mean(value) + sd_v*sd(value),
            lower_CI = mean(value) - (v_val*sd(value))/sqrt(n()),
            upper_CI = mean(value) + (v_val*sd(value))/sqrt(n()),
            n = n()) %>%
  mutate(variables = "3. 2nd derivative")

theme_set(theme_gray(base_size = 18))





fun_1_cp_detection %>%
  filter(variables %in% c("1. Input dataset", "2. Rolling gradient",
                          "3. 2nd derivative")) %>%
  ggplot(aes(x = index, y = value)) +
  geom_line(aes(colour=variables), lwd = 1.5)+
  labs(x= "x", y = "Various Units", colour = "Variables")+
  geom_vline(data = filter(fun_1_cp_detection,
                           cp_marker==TRUE, variables == "1. Input dataset"),
             aes(xintercept = index), size  = 1, colour = "blue")+
  geom_hline(data = filter(random_example_summarise_stats, variables == "3. 2nd derivative"),
             aes(yintercept = mean_value), size  = 1, colour = "red")+
  geom_hline(data = filter(random_example_summarise_stats, variables == "3. 2nd derivative"),
             aes(yintercept = lower_CI), size  = 0.8, colour = "red",
             linetype = 2)+
  geom_hline(data = filter(random_example_summarise_stats, variables == "3. 2nd derivative"),
             aes(yintercept = upper_CI), size  = 0.8, colour = "red",
             linetype = 2) +
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y")

