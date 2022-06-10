
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
theme_set(theme_gray(base_size = 18))

fun_1_cp_detection %>%
  filter(variables != "r.squareds") %>%
  ggplot(aes(x = index, y = value)) +
  geom_line(aes(colour=variables), lwd = 1.5)+
  geom_vline(data = filter(fun_1_cp_detection,
                           cp==TRUE, variables == "Test dataset"),
             aes(xintercept = index), size  = 1, colour = "blue")+
  labs(x= "Date", y = "Various Units", colour = "Variables")+
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y")
