library(rstanarm)
if (Sys.getenv("CI") == "true") {
  print(packageVersion("dplyr"))
  install.packages("hrbrthemes", repos = "https://cinc.rud.is")
}
library(hrbrthemes)

df <- cuadrantes %>%
  group_by(crime) %>%
  mutate(total_count = sum(count)) %>%
  ungroup %>%
  filter(total_count /  length(unique(cuadrantes$date)) > 20) %>%
  group_by(date, crime) %>%
  summarise(n = sum(count)) %>%
  mutate(month = month(date)) %>%
  mutate(time = as.numeric(as.Date(date))) %>%
  mutate(logn = log1p(n / days_in_month(as.Date(date)) * 30)) 


duration <- days_in_month(as.Date(df$date)) / (365/12)

m1 <- stan_gamm4(n ~ s(time, by = crime) + offset(log(duration)), # + s(month, bs = "cc", k = 12), #,
                 family = poisson, 
                 random = ~(1 | crime), 
                 data = df, 
                 chains = 4,
                 iter = 2000,
                 adapt_delta = .99, 
                 cores = 4,
                 seed = 12345)
save(m1, file = "clean-data/m1_crimes.RData")

#load( "clean-data/m1_crimes.RData")
#plot_nonlinear(m1)
#pp_check(m1)
#pp_check(m1, plotfun = "ppc_ecdf_overlay")


dates <- seq(as.Date(min(df$date)), as.Date(max(df$date)), by = "month")
ndates <- length(dates)

trends <- do.call(rbind, lapply(as.character(unique(df$crime)), function(x) {
  crime_name <- x
  inc <- grep(crime_name, colnames(predict(m1$jam, type = "lpmatrix")))
  #X0 <- predict(m1$jam, type = 'lpmatrix')[, c(1, inc)]
  
  
  eps <- 1e-7
  newDFeps <- df 
  newDFeps$time <- df$time + eps
  newDFeps$duration <- log(1)
  X1 <- predict(m1$jam, newDFeps, type = 'lpmatrix')[, c(1, inc)]
  
  sims_o <- as.matrix(m1)[, c(1, inc)] %*% t(as.matrix(m1$x[which(df$crime == crime_name), c(1, inc)])) 
  sims_n <- as.matrix(m1)[, c(1, inc)] %*% t(X1[which(df$crime == crime_name),])
  
  #100 x 10 * ndates * 10
  d1 <- ((sims_n - sims_o) / eps) 
  dim(d1)
  d1[1:5, 1:5]
  sum(d1[, ndates] >= 0)
  qt <- quantile(d1[, ndates], c(.1, .90))
  med <- median(d1[, ndates])
  if (qt[1] < 0 & qt[2] < 0)
    return(data.frame(crime = crime_name, 
                      trend = "negative", 
                      fd = med))
  else if (qt[1] > 0 & qt[2] > 0)
    return(data.frame(crime = crime_name, 
                      trend = "positive", 
                      fd = med))
  else
    return(data.frame(crime = crime_name, 
                      trend = NA, 
                      fd = med))
})
)

sims <- do.call(rbind, lapply(as.character(unique(df$crime)), function(x) {
  crime_name <- x
  print(x)
  inc <- grep(crime_name, colnames(predict(m1$jam, type = "lpmatrix")))
  
  X0 <- as.matrix(m1$x)[which(df$crime == crime_name), c(1, inc)]
  sims <- as.matrix(m1)[, c(1, inc)] %*% t(X0) %>% as.data.frame()
  
  binc <- grep(paste0("b\\[\\(Intercept\\) crime:", 
                      str_replace_all(crime_name," ", "_"),
                      "\\]$"), 
               colnames(m1$x))
  b = as.matrix(m1)[, binc, drop = FALSE]
  sims <- apply(sims, 2, function(x) {x + b}) %>% as.data.frame()
  
  sims$sim <- 1:nrow(sims)
  sims <- gather(data.frame(sims), "time", "rate", -sim) %>%
    mutate(time = as.numeric(str_replace(time, "X", ""))) %>%
    arrange(sim, time)
  sims$date <- dates
  sims$crime <- crime_name
  sims$count <- exp(sims$rate)
  return(sims)
}))

df$date <- as.Date(df$date)
sims <- left_join(sims, trends, by = "crime")
sims <- left_join(sims, df[, c("crime", "date", "n")], by = c("crime", "date"))
sims <- sims %>%
  mutate(fd = as.numeric(fd)) %>%
  arrange(desc(fd)) %>%
  mutate(crime = factor(crime, levels = unique(crime)))



p <- ggplot(sims, aes(x = date, y = expm1(rate), group = sim)) +
  geom_line(alpha = 0.1, aes(color = trend), size = .05) +
  geom_point(aes(date, n),
             fill = "#f8766d",
             color = "black",
             shape = 21,
             size = 1.1) +
  scale_color_manual("tendencia\núltimo mes",
                     values = c("positive" = "#e41a1c", 
                                "negative" = "#1f78b4"), 
                     labels = c("al alza", "a la baja", "no significativa"),
                     breaks = c("positive", "negative", NA),
                     na.value = "#cab2d6") +
  expand_limits(y = 0) +
  xlab("fecha") +
  ylab("número de crímenes") +
  labs(title = "Tendencias de crímenes en CDMX y 1000 simulaciones del posterior de un modelo aditivo multinivel, por crimen",
       subtitle = "El color de cada crimen corresponde a la tendencia del último mes (primera derivada, intervalo de credibilidad del 90%).",
       caption = "Fuente: PGJ-CDMX Carpetas de Investigación") +
  theme_ft_rc(base_family = "Arial Narrow", strip_text_size = 10) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  facet_wrap(~crime, scale = "free_y", ncol = 4)

ggsave("graphs/trends.png", height = 14, width = 14, dpi = 100)


