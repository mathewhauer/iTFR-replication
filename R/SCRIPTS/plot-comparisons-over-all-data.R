## @knitr plot-comparisons-over-all-data

fullanalysis = rbind(parts,whole) %>%
                  mutate(Method = 
                    factor(Method,
                      levels=c('iTFR','iTFR+','xTFR','xTFR+','bTFR','rele'),
                      labels=c('iTFR','iTFR+','xTFR','xTFR+','bTFR','Rele'),
                      ordered=TRUE
                        )) %>%
  filter(Method %in% c('iTFR','iTFR+','xTFR','xTFR+','bTFR'))

## arrange the vertical plotting order (20 is top, 1 is bottom)
ord = tribble(
  ~'i', ~'Data', ~'Method',
    20, 'All Data', 'iTFR',
    19, 'All Data', 'iTFR+',
    18, 'All Data', 'xTFR',
    17, 'All Data', 'xTFR+',
    16, 'All Data', 'bTFR',
    14, 'HMD/HFD', 'iTFR',
    13, 'HMD/HFD', 'iTFR+',
    12, 'HMD/HFD', 'xTFR',
    11, 'HMD/HFD', 'xTFR+',
    10, 'HMD/HFD', 'bTFR',
     8, 'DHS', 'iTFR',
     7, 'DHS', 'iTFR+',
     6, 'DHS', 'xTFR',
     5, 'DHS', 'xTFR+',
     4, 'DHS', 'bTFR',
     2, 'US counties','iTFR',
     1, 'US counties','xTFR'
)



fullanalysis = fullanalysis %>%
                left_join(ord, by=c('Data','Method'))

b<- ggplot(data = fullanalysis, 
       aes(x=A50,y=i, 
           color=Method,
           group=Method,
           label=paste0(Method,' (',n,')'))) +
  geom_point( size=2) +
  geom_text(x=fullanalysis$A90+0.05, y=fullanalysis$i,
            size=3, hjust=0, color='black') +
    geom_segment( x=fullanalysis$A10, xend=fullanalysis$A90, 
                y=fullanalysis$i, yend=fullanalysis$i,
                lwd=1) +
  geom_hline(yintercept=c(15,9,3)) +
  geom_vline(xintercept=0) +
  geom_vline(xintercept=-1) +
  scale_color_discrete(guide=FALSE) +
  theme(axis.line.y        = element_blank(),
        axis.text.y        = element_blank(),
        axis.ticks.y       = element_blank(),
        panel.grid.major.x = element_line(color='darkgrey')) +
  scale_x_continuous(breaks=seq(-0,1,0.25), limits=c(-0.2,1.1)) +
  labs(x='Absolute Error (Births/Woman)',
       y='') +
  geom_text(x= -.25, y=18,  label='ALL\nDATA\nCOMBINED', 
            color='black',hjust= 0, size=4) +
  geom_text(x=  -.25, y=12,  label='HMD/HFD', 
            color='black',hjust= 0, size=4) +
  geom_text(x=  -.25, y=6,   label='DHS', 
            color='black',hjust= 0, size=4) +
  geom_text(x=  -.25, y=1.5, label='US\nCOUNTIES', 
            color='black',hjust= 0, size=4) 


b