
#example script that was used to test CPD debugging

x = c(0, 300, 600, 900, 1200, 1500)
f_x = c(-5, 8, 1, 2, 3, 6)
g_x = c(-5, 8, 1, 2, 3, 4.5)



plot(x, f_x, type = "l")
plot(x, g_x, type = "l")


fun_1_df = linear_function_generator(x, f_x, tail(x, n=1), 0.01)
fun_2_df = linear_function_generator(x, g_x, tail(x, n=1), 0.01)

plot(fun_2_df, type = "l")


win_length_vector = c(7)

fun_1_cp_detection = map_dfr(.x = win_length_vector,
                             .f = ~window_length_constrain(df = fun_1_df,
                                                           .x, cp_factor = 1e2,
                                                           epsilon = 1e-9, date = FALSE))

fun_2_cp_detection = map_dfr(.x = win_length_vector,
                             .f = ~window_length_constrain(df = fun_2_df,
                                                           .x, cp_factor = 1e2,
                                                           epsilon = 1e-9, date = FALSE))






