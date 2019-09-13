library(rstanarm)
if (Sys.getenv("CI") == "true") {
  print(packageVersion("dplyr"))
  install.packages("hrbrthemes", repos = "https://cinc.rud.is")
}
library(hrbrthemes)

df <- cuadrantes %>%
  group_by(date, crime) %>%
  summarise(n = sum(count)) %>%
  mutate(month = month(date)) %>%
  mutate(time = as.numeric(as.Date(date)))

m1 <- stan_gamm4(n ~ s(time, by = crime) + s(month, bs = "cc", k = 12), #,
                 family = poisson, 
                 random = ~(1 | crime), 
                 data = df, 
                 chains = 2, 
                 iter = 1000,
                 adapt_delta = .99, 
                 cores = 2, 
                 seed = 12345)
save(m1, file = "clean-data/m1_crimes.RData")

#plot_nonlinear(m1)
#pp_check(m1)
#pp_check(m1, plotfun = "ppc_ecdf_overlay")

tryCatch(
  {
    
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
      qt <- quantile(d1[, ndates], c(.05, .95))
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
    
    sims <- left_join(sims, trends, by = "crime")
    sims <- sims %>%
      mutate(fd = as.numeric(fd)) %>%
      arrange(desc(fd)) %>%
      mutate(crime = factor(crime, levels = unique(crime)))
    
    
    p <- ggplot(sims, aes(x = as.Date(date), y = rate, group = sim)) +
      geom_line(alpha = 0.1, aes(color = trend), size = .05) +
      scale_color_manual("tendencia\núltimo mes",
                         values = c("positive" = "#e41a1c", 
                                    "negative" = "#1f78b4"), 
                         labels = c("positiva", "negativa", "no significativa"),
                         breaks = c("positive", "negative", NA),
                         na.value = "#cab2d6") +
      geom_point(data = df, aes(as.Date(date), n, group = crime), 
                 fill = "#f8766d", 
                 color = "black",
                 shape = 21,
                 size = 1.1) +
      theme_bw() +
      expand_limits(y = 0) +
      xlab("fecha") +
      ylab("número") +
      labs(title = "Número de crimenes y 1000 simulaciones del posterior\nde un modelo aditivo multinivel ajustado por estacionalidad, por estado",
           subtitle = "La tendencia del último mes corresponde al color de cada estado (primera derivada, intervalo de credibilidad del 90%).",
           caption = "Fuente: SNSP víctimas y proyecciones del CONAPO con datos del 2015") +
      theme_ft_rc(base_family = "Arial Narrow") +
      facet_wrap(~crime, scale = "free_y", ncol = 4) + 
      guides(color = guide_legend(override.aes = list(size = 2, alpha = 1)))
    
    ggsave("graphs/predicted.png", height = 14, width = 14, dpi = 100)
    
  },
  error = function(cond) {
    message("Ploting rstanarm model caused an error:")
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  warning = function(cond) {
    message("Plotting rstanarm model caused a warning:")
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  }
)