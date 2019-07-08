## @knitr table-comparisons-over-all-data


errortable_hfd <- tribble(
  ~"Method Family", ~n, ~"50%ile Absolute Error", ~"90%ile Absolute Error", ~"50%ile APE", ~"90%ile APE",
  "iTFR",  length(which(!is.na(big$iTFR))), itfrabs50, itfrabs90, itfrape50, itfrape90,
  "iTFR+", length(which(!is.na(big$'iTFR+'))), itfr_hatabs50, itfr_hatabs90, itfr_hatape50, itfr_hatape90,
  "XTFR", length(which(!is.na(big$xTFR))), xtfrabs50, xtfrabs90, xtfrape50, xtfrape90,
  "xTFR+",length(which(!is.na(big$'xTFR+'))), xtfr_hatabs50, xtfr_hatabs90, xtfr_hatape50, xtfr_hatape90,
  "bTFR", length(which(!is.na(big$bTFR))), bayestfrabs50, bayestfrabs90, bayestfrape50, bayestfrape90
) %>%
  mutate(Data = "HFD/HMD")

errortable_counties <-tribble(
  ~Method, ~n, ~"50%ile APE", ~"90%ile APE",
  "iTFR", countyeval$tot, countyeval$ilagTFR50,	 countyeval$ilagTFR90,
  "xTFR", countyeval$tot, countyeval$xlagTFR50,countyeval$xlagTFR90
)

errortable[1]