## @knitr plot-multipanel-summary

#   Making the Figure 1 graphic
figure1_func = function(method, color){
  a <- ggplot(data=big, aes(shape = source)) +
    geom_text(x=2, y=5.5, label= paste0(method), 
              color='black', size =4) +
    geom_point(alpha=.80, color='black',
               shape='+', size=1,
               aes_string(x = big$lagTFR, 
                          y = big[[paste0(method)]])) +
    geom_abline(intercept=0,slope=1, alpha=.25) +
    geom_abline(slope = 0.9, lty=2, lwd=0.25, col='black', alpha=.50) +
    geom_abline(slope = 1.1, lty=2, lwd=0.25, col='black', alpha=.50) +
    theme_bw() +
    theme(legend.position="none",
          plot.background = element_rect(fill = paste0(color)))+
    theme(text=element_text(face='bold', size=9)) +
    xlim(0.9,8) + ylim(0.9,8) +
    labs(x='TFR (Data)', y='TFR (Estimate)') 
  
  return(a)
}



figure2_func = function(method, color){
  dif<- (big[[paste0(method)]] - big$lagTFR)/ big$lagTFR*100
  a <- ggplot(data=big, aes(shape = source)) +
    geom_point(alpha=.80, color='black',
               shape='+', size=1,
               aes_string(x = "C", y = dif)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 10, lty=2, lwd=0.5)+
    geom_hline(yintercept = -10, lty=2, lwd=0.5)+
    theme_bw() +
    theme(legend.position="none",
      text=element_text(face='bold', size=9),
          plot.background = element_rect(fill = paste0(color))) +
    ylim(-30,30) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    labs( x='Population', y='% Error') 
  return(a)
}

figure3_func = function(method, color){
  dif<- (big[[paste0(method)]] - big$lagTFR)/ big$lagTFR*100
  a <- ggplot(data=big, aes(shape = source)) +
    geom_point(alpha=.80, color='black',
               shape='+', size=1,
               aes_string(x = "Year", y = dif)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 10, lty=2, lwd=0.5)+
    geom_hline(yintercept = -10, lty=2, lwd=0.5)+
    theme_bw() +
    theme(legend.position="none",
      text=element_text(face='bold', size=9),
          plot.background = element_rect(fill = paste0(color))) +
    xlim(1891,2015) + ylim(-30,30) +
    labs(x='Year', y='% Error') 
  return(a)
}

itfr_1 <- figure1_func("iTFR", "lightcoral")
xtfr_1 <- figure1_func("xTFR", "lightblue")
bayestfr_1 <- figure1_func("bTFR", "lightgreen")

itfr_2 <- figure2_func("iTFR", "lightcoral")
xtfr_2 <- figure2_func("xTFR", "lightblue")
bayestfr_2 <- figure2_func("bTFR", "lightgreen")

itfr_3 <- figure3_func("iTFR", "lightcoral")
xtfr_3 <- figure3_func("xTFR", "lightblue")
bayestfr_3 <- figure3_func("bTFR", "lightgreen")

f1j<- ggplot() +
  stat_density(data=big, aes(x=iTFR-lagTFR), geom='line', adjust=1.5,col='lightcoral', lwd=1) +
  stat_density(data=big,geom='line', aes(x=bTFR-lagTFR),adjust=1.5,  col='green', lwd=1) +
  stat_density(data=big, aes(x=xTFR-lagTFR),geom='line', adjust=1.5, col="lightblue", lwd=1)+
  geom_vline(xintercept=0) +
  theme_bw() +
  xlim(-0.5,0.5) + 
  theme(text=element_text(face='bold', size=9)) +
  labs(x='Algebraic Error',
       y='Density')

f1k<- ggplot() +
  stat_density(data=big, aes(x=abs((iTFR-lagTFR))), adjust=1.5,col='lightcoral', lwd=1, geom='line') +
  stat_density(data=big, aes(x=abs((bTFR-lagTFR))),adjust=1.5,   col='green', lwd=1, geom='line') +
  stat_density(data=big, aes(x=abs((xTFR-lagTFR))), adjust=1.5, col="lightblue", lwd=1, geom='line')+
  geom_vline(xintercept=0) +
  theme_bw() +
  xlim(0,+.50) + 
  theme(text=element_text(face='bold', size=9)) +
  labs(x='Absolute Algebraic Error',
       y='Density')

f1l<- ggplot() +
  stat_density(data=big, aes(x=abs((iTFR-lagTFR)/lagTFR)*100), adjust=1.5,col='lightcoral', lwd=1, geom='line') +
  stat_density(data=big, aes(x=abs((bTFR-lagTFR)/lagTFR)*100),adjust=1.5, col='green', lwd=1, geom='line') +
  stat_density(data=big, aes(x=abs((xTFR-lagTFR)/lagTFR)*100), adjust=1.5, col="lightblue", lwd=1, geom='line')+
  geom_vline(xintercept=0) +
  theme_bw() +
  xlim(0,25) +
  theme(text=element_text(face='bold', size=9)) +
  labs(x='Absolute Percent Error',
       y='Density')

plot_grid(itfr_1, xtfr_1, bayestfr_1,
          itfr_2, xtfr_2, bayestfr_2,
          itfr_3, xtfr_3, bayestfr_3,
          f1j, f1k, f1l,
          labels = "auto", ncol=3, label_y=0.3, label_x=0)