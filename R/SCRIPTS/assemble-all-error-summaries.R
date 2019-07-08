## @knitr assemble-all-error-summaries


# DHS and HFD/HMD error summaries
part1 = big %>%
  dplyr::select(-q5) %>%
  gather(key=Method, value=estimate,-Code,-Year,-lagTFR, -source, -C ) %>%
  mutate(err=estimate-lagTFR,
         ape= 100* abs(err)/lagTFR ) %>%
  filter( is.finite(ape)) %>% 
  dplyr::select(Data=source, Method, ape, err, lagTFR)

# census county error summaries
part2 = census_county %>%
  gather(key=Method, value=estimate,-CountyCode,-Year,-lagTFR ) %>%
  mutate(source='US counties',
         err=estimate-lagTFR,
         ape= 100* abs(err)/lagTFR ) %>%
  filter( is.finite(ape)) %>% 
  dplyr::select(Data=source, Method, ape, err, lagTFR)

parts = rbind(part1,part2) %>%
              group_by(Data,Method) %>%
              summarize(n = n(),
                        Q10 = quantile(ape,.10),
                        Q50 = quantile(ape,.50),
                        Q90 = quantile(ape,.90),
                        A10 = quantile(abs(err) ,.10),
                        A50 = quantile(abs(err), .50),
                        A90 = quantile(abs(err), .90))  %>%
              ungroup()

whole = rbind(part1,part2) %>%
          group_by(Method) %>%
          summarize(n=n(),
                    Q10 = quantile(ape,.10),
                    Q50 = quantile(ape,.50),
                    Q90 = quantile(ape,.90),
                    A10 = quantile(abs(err),.10),
                    A50 = quantile(abs(err),.50),
                    A90 = quantile(abs(err),.90))  %>%
          mutate(Data='All Data') %>%
          ungroup() %>% 
          dplyr::select(Data,Method,n,Q10:A90)


allerrortable_hfddhs <- big %>%
  dplyr::select(-C,-W,-q5) %>%
  gather(key=Method, value=estimate,-Code,-Year,-lagTFR, -source ) %>%
  mutate(err=estimate-lagTFR,
         ape= abs(err)/lagTFR) %>%
  filter( is.finite(ape)) %>% 
  dplyr::select(Data=source, Method, ape, err)

allerrortable_county <- census_county %>%
  gather(key=Method, value=estimate,-CountyCode,-Year,-lagTFR ) %>%
  mutate(source='US counties',
         err=estimate-lagTFR,
         ape= abs(err)/lagTFR) %>%
  filter( is.finite(ape)) %>% 
  dplyr::select(Data=source, Method, ape, err)

allerrortable <- rbind(allerrortable_county, allerrortable_hfddhs) %>%
  filter(!Method == "rele") %>%
  mutate(Method = 
           factor(Method,
                  levels=c('iTFR','iTFR+','xTFR','xTFR+','bTFR','rele'),
                  labels=c('iTFR','iTFR+','xTFR','xTFR+','bTFR','Rele'),
                  ordered=TRUE
           )) %>%
  group_by(Data, Method) %>%
  dplyr::summarise(n = n(),
                   `50%ile Absolute Error` = f_num(quantile(abs(err), 0.5),2, "0"),
                   `90%ile Absolute Error` = f_num(quantile(abs(err), 0.9), 2, "0"),
                   `50%ile APE` = f_prop2percent(quantile(ape, 0.5),1),
                   `90%ile APE` = f_prop2percent(quantile(ape, 0.9),1)) %>%
  ungroup() %>%
  rename("Method Family" = "Method") 
