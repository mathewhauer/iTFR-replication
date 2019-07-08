## @knitr plot-nonhuman-estimates

wild50thabs <- quantile(abs(combined$iTFR / combined$TFR_table -1), 0.5)
wild90thabs <- quantile(abs(combined$iTFR / combined$TFR_table -1), 0.9)

#   Making the Primate graphic
ggplot(combined, aes(x = TFR_table, y = iTFR)) +
  geom_point(shape = 19, color = 'gray', size = 3) +
  geom_line(aes(x = TFR_table, y = 0.9 * TFR_table), lty = 2, lwd = 1, col = 'black') +
  geom_line(aes(x = TFR_table, y = 1.1 * TFR_table), lty = 2, lwd = 1, col = 'black') +
  geom_text_repel(aes(label = Species), hjust = 0, vjust = 0) +
  annotate("text", label = paste0("50th percentile APE: ", percent(round(wild50thabs, 4))), x = 4, y = 10) +
  annotate("text", label = paste0("90th percentile APE: ", percent(round(wild90thabs, 4))), x = 4, y = 9.5) +
  theme_bw() +
  geom_abline(slope = 1) +
  xlim(2.9, 11) +
  ylim(2.9, 11) +
  theme(text = element_text(face = 'bold')) +
  labs(x = 'TFR(Data)',
       y = 'iTFR')
