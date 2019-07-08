## @knitr plot-errors-by-q5

err_q5 = function(df,err, color){
  errs <- abs(df[[paste0(err)]] / df$lagTFR -1)
  a<- ggplot(data=df) +
    geom_point(alpha=.80, color='black',
               shape='+', size=1,
               aes(x = 1000*q5, y = abs(df[[err]] / df$lagTFR -1)*100)) +
    geom_smooth(aes(x = 1000*q5, y = errs*100), color = 'black') +
    theme_bw() +
    geom_hline(yintercept=10,lty=2, lwd=0.5, col='black') +
    geom_text(x=50, y=20, label= paste0(err), color='black', size =4) +
    coord_cartesian(ylim = c(0, 25)) +
    theme(text=element_text(face='bold', size=9),
          plot.background = element_rect(fill = paste0("light", color))) +
    labs(x = "q(5), per 1000",
         y = "APE")
  return(a)
}

a<- err_q5(big,"iTFR", "coral")
b<- err_q5(big,"iTFR+", "coral")
c<- err_q5(big,"xTFR", "blue")
d<- err_q5(big,"xTFR+", "blue")
e<- err_q5(big,"bTFR", "green")

plot_grid(a,b,c,d,e, labels = 'auto', ncol = 2)


