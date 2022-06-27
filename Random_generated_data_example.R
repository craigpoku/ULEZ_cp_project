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

win_length_vector = c(10)

fun_1_cp_detection = map_dfr(.x = win_length_vector,
                             .f = ~multi_var_ts_gradient_cp_detection_new(df = fun_1_df,
                                                           .x, "test", sd_value = sd_v, date = FALSE))
fun_1_cp_detection_example = map_dfr(.x = 60,
                             .f = ~window_length_constrain(df = fun_1_df,
                                                           .x, cp_factor = 1e2,
                                                           epsilon = 1e-9, date = FALSE))

random_example_summarise_stats = filter(fun_1_cp_detection, variables == "3. 2nd derivative") %>% 
  group_by(window_length_level) %>% 
  summarise(mean_value = mean(value), lower = mean(value) - sd_v*sd(value), 
            upper = mean(value) + sd_v*sd(value)) %>%
  mutate(variables = "3. 2nd derivative")

theme_set(theme_gray(base_size = 18))





fun_1_cp_detection %>%
  filter(variables %in% c("1. Input dataset", "2. Rolling gradient",
                          "3. 2nd derivative", "diff")) %>%
  ggplot(aes(x = index, y = value)) +
  geom_line(aes(colour=variables), lwd = 1.5)+
  labs(x= "x", y = "Various Units", colour = "Variables")+
  geom_vline(data = filter(fun_1_cp_detection,
                           cp==TRUE, variables == "1. Input dataset"),
             aes(xintercept = index), size  = 1.2, colour = "blue")+
  geom_hline(data = filter(random_example_summarise_stats, variables == "3. 2nd derivative"),
             aes(yintercept = mean_value), size  = 1, colour = "red")+
  geom_hline(data = filter(random_example_summarise_stats, variables == "3. 2nd derivative"),
             aes(yintercept = lower), size  = 0.8, colour = "red",
             linetype = 2)+
  geom_hline(data = filter(random_example_summarise_stats, variables == "3. 2nd derivative"),
             aes(yintercept = upper), size  = 0.8, colour = "red",
             linetype = 2) +
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y")

  
  #example script that was used to test CPD debugging
  
  x = c(0, 300, 600, 900, 1200, 1500)
  f_x = c(-5, 8, 1, 2, 3, 6)
  g_x = c(-5, 8, 1, 2, 3, 4.5)
  
  
  
  plot(x, f_x, type = "l")
  plot(x, g_x, type = "l")
  
  
  fun_1_df = linear_function_generator(x, f_x, tail(x, n=1), 0.01)
  fun_2_df = linear_function_generator(x, g_x, tail(x, n=1), 0.01)
  
  plot(fun_2_df, type = "l")
  
  
  win_length_vector = c(10, 60, 250)
  
  fun_1_cp_detection = map_dfr(.x = win_length_vector,
                               .f = ~window_length_constrain(df = fun_1_df,
                                                             .x, cp_factor = 1e2,
                                                             epsilon = 1e-9, date = FALSE))
  fun_1_cp_detection_example = map_dfr(.x = 60,
                               .f = ~window_length_constrain(df = fun_1_df,
                                                             .x, cp_factor = 1e2,
                                                             epsilon = 1e-9, date = FALSE))
  
  
  theme_set(theme_gray(base_size = 18))
  
  fun_1_cp_detection_example %>%
    filter(variables == "Test dataset") %>%
    ggplot(aes(x = index, y = value)) +
    geom_line(colour = "red", lwd = 3)+
    labs(x= "x", y = "f(x)")+
    geom_vline(data = filter(fun_1_cp_detection_example,
                             cp==TRUE, variables == "Test dataset"),
               aes(xintercept = index), size  = 1.2, colour = "blue")+
    labs(x= "Date", y = "Various Units", colour = "Variables")+
    facet_grid(vars(variables), vars(window_length_level), scales = "free_y")
  
  
  
  
  fun_1_cp_detection %>%
    filter(variables != "r.squareds") %>%
    ggplot(aes(x = index, y = value)) +
    geom_line(aes(colour=variables), lwd = 1.5)+
    geom_vline(data = filter(fun_1_cp_detection,
                             cp==TRUE, variables == "Test dataset"),
               aes(xintercept = index), size  = 1, colour = "blue")+
    labs(x= "x", y = "Various Units", colour = "Variables")+
    facet_grid(vars(variables), vars(window_length_level), scales = "free_y")

