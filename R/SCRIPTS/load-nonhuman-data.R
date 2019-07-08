## @knitr load-nonhuman-data

# Importing the Bronikowski primate data.
PRIM_DATA <- read_csv("../R/DATA-RAW/PRIMATES-Bronikowski.LifeTables.SciData2016.csv")

# Importing the lion data from Jones et al
need_lions = FALSE   # FALSE once downloaded successfully (2 Mar 2019)
if (need_lions) {
 download.file(url     ="https://media.nature.com/original/nature-assets/nature/journal/v505/n7482/source_data/nature12789-f1.xls", 
               destfile="../R/DATA-RAW/PRIMATES-nature12789-f1.xls", mode = "wb")
}

# Processing the Jones data
jones <- read_excel("../R/DATA-RAW/PRIMATES-nature12789-f1.xls", sheet = 37) %>%
  filter(!Lion == "x") 

names(jones)[c(1,3,6)] = c('Age','N.x.female','nFx')

jones = jones %>% 
  mutate(Species          = "African Lion", #setting the species
         Age              = as.numeric(Age),
         N.x.female       = as.numeric(N.x.female),
         N.x.male         = 0, #creating some columns to allow a clean rbind with the PRIM_DATA file
         fertint          = ifelse(nFx == 0, 0, 1), # making the fertility interval length
         child            = 0,
         nFx              = as.numeric(nFx),
         Breeding_females = 0,
         N.x.male         = 0,
         Offspring        = 0,
         fertstart        = min(Age[which(nFx>0)]),
         fertend          = max(Age[which(nFx>0)]),
         fertlength       = as.numeric(fertend) - as.numeric(fertstart)) %>%
  dplyr::select(Age, N.x.female, nFx, Species, N.x.male, fertint, child, Breeding_females, Offspring, fertstart, fertend, fertlength) #selecting the columns for a clean rbind with PRIM_DATA

# Data come from various sources and are listed in the main text and supplementary materials.
prim2 <- tribble(
  ~'Species', 	~'AGE', 	~'Maleenter', 	~'MaleDie', 	~'FemaleEnter', 	~'FemaleDie', 	~'nFx', 
  'Thomas Langur', 	0, 	50, 	24, 	61, 	26, 	0, 
  'Thomas Langur', 	1, 	0, 	0, 	34, 	9, 	0, 
  'Thomas Langur', 	2, 	0, 	0, 	21, 	2, 	0, 
  'Thomas Langur', 	3, 	0, 	0, 	15, 	3, 	0, 
  'Thomas Langur', 	4, 	0, 	0, 	13, 	1, 	0.26, 
  'Thomas Langur', 	5, 	0, 	0, 	276, 	15, 	0.32, 
  'Thomas Langur', 	6, 	0, 	0, 	0, 	0, 	0.65, 
  'Thomas Langur', 	7, 	0, 	0, 	0, 	0, 	0.49, 
  'Thomas Langur', 	8, 	0, 	0, 	0, 	0, 	0.48, 
  'Thomas Langur', 	9, 	0, 	0, 	0, 	0, 	0.41, 
  'Thomas Langur', 	10, 	0, 	0, 	0, 	0, 	0.48, 
  'Thomas Langur', 	11, 	0, 	0, 	0, 	0, 	0.58, 
  'Macaque', 	0, 	0, 	0, 	2368, 	0, 	0, 
  'Macaque', 	1, 	0, 	0, 	1919.9, 	0, 	0, 
  'Macaque', 	2, 	0, 	0, 	1569.4, 	0, 	0, 
  'Macaque', 	3, 	0, 	0, 	1319.1, 	0, 	0.0765673565309681, 
  'Macaque', 	4, 	0, 	0, 	1122.3, 	0, 	0.529270248596632, 
  'Macaque', 	5, 	0, 	0, 	978.4, 	0, 	0.603025347506132, 
  'Macaque', 	6, 	0, 	0, 	841.7, 	0, 	0.577402875133658, 
  'Macaque', 	7, 	0, 	0, 	689.6, 	0, 	0.587296983758701, 
  'Macaque', 	8, 	0, 	0, 	565, 	0, 	0.527433628318584, 
  'Macaque', 	9, 	0, 	0, 	463.9, 	0, 	0.48286268592369, 
  'Macaque', 	10, 	0, 	0, 	361.9, 	0, 	0.530533296490743, 
  'Macaque', 	11, 	0, 	0, 	288.6, 	0, 	0.488565488565489, 
  'Macaque', 	12, 	0, 	0, 	232.4, 	0, 	0.430292598967298, 
  'Macaque', 	13, 	0, 	0, 	184, 	0, 	0.385869565217391, 
  'Macaque', 	14, 	0, 	0, 	134.3, 	0, 	0.275502606105733, 
  'Macaque', 	15, 	0, 	0, 	99.3, 	0, 	0.392749244712991, 
  'Macaque', 	16, 	0, 	0, 	74.8, 	0, 	0.200534759358289, 
  'Macaque', 	17, 	0, 	0, 	52, 	0, 	0.230769230769231, 
  'Macaque', 	18, 	0, 	0, 	38, 	0, 	0.0789473684210526, 
  'Macaque', 	19, 	0, 	0, 	21.3, 	0, 	0.0938967136150235, 
  'Macaque', 	20, 	0, 	0, 	12.6, 	0, 	0.158730158730159, 
  'Macaque', 	21, 	0, 	0, 	8.1, 	0, 	0.123456790123457, 
  'Macaque', 	22, 	0, 	0, 	7.2, 	0, 	0.138888888888889, 
  'Macaque', 	23, 	0, 	0, 	4.8, 	0, 	0, 
  'Macaque', 	24, 	0, 	0, 	3.8, 	0, 	0, 
  'Macaque', 	25, 	0, 	0, 	2.2, 	0, 	0, 
  'Macaque', 	26, 	0, 	0, 	2, 	0, 	0, 
  'Macaque', 	27, 	0, 	0, 	1.8, 	0, 	0, 
  'Macaque', 	28, 	0, 	0, 	1, 	0, 	0, 
  'Macaque', 	29, 	0, 	0, 	0.2, 	0, 	0, 
  'Northern Fur Seal', 	0, 	0, 	0, 	0, 	2096, 	0, 
  'Northern Fur Seal', 	1, 	0, 	0, 	0, 	0, 	0, 
  'Northern Fur Seal', 	2, 	0, 	0, 	0, 	0, 	0, 
  'Northern Fur Seal', 	3, 	0, 	0, 	0, 	646, 	0.015, 
  'Northern Fur Seal', 	4, 	0, 	0, 	0, 	572, 	0.22, 
  'Northern Fur Seal', 	5, 	0, 	0, 	0, 	530, 	0.395, 
  'Northern Fur Seal', 	6, 	0, 	0, 	0, 	505, 	0.395, 
  'Northern Fur Seal', 	7, 	0, 	0, 	0, 	486, 	0.425, 
  'Northern Fur Seal', 	8, 	0, 	0, 	0, 	478, 	0.46, 
  'Northern Fur Seal', 	9, 	0, 	0, 	0, 	447, 	0.445, 
  'Northern Fur Seal', 	10, 	0, 	0, 	0, 	434, 	0.45, 
  'Northern Fur Seal', 	11, 	0, 	0, 	0, 	429, 	0.44, 
  'Northern Fur Seal', 	12, 	0, 	0, 	0, 	387, 	0.43, 
  'Northern Fur Seal', 	13, 	0, 	0, 	0, 	362, 	0.42, 
  'Northern Fur Seal', 	14, 	0, 	0, 	0, 	336, 	0.41, 
  'Northern Fur Seal', 	15, 	0, 	0, 	0, 	293, 	0.39, 
  'Northern Fur Seal', 	16, 	0, 	0, 	0, 	233, 	0.33, 
  'Northern Fur Seal', 	17, 	0, 	0, 	0, 	142, 	0.355, 
  'Northern Fur Seal', 	18, 	0, 	0, 	0, 	97, 	0.265, 
  'Northern Fur Seal', 	19, 	0, 	0, 	0, 	59, 	0.255
)

prim2tfr <- prim2 %>%
  group_by(Species) %>%
  summarise(TFR_table = sum(nFx))
prim2children <- prim2 %>%
  filter(AGE == 0) %>%
  mutate(child = Maleenter + MaleDie + FemaleEnter + FemaleDie) %>%
  dplyr::select(Species, child)
prim2women <- prim2 %>%
  group_by(Species) %>%
  mutate(fertint = ifelse(nFx == 0, 0, 1),
         fertstart = min(AGE[which(nFx>0)]),
         fertend = max(AGE[which(nFx>0)]),
         fertlength = as.numeric(fertend) - as.numeric(fertstart)) %>%
  summarise(Women = sum(as.numeric(FemaleEnter[which(AGE >= fertstart & AGE<=fertend)])) + sum(as.numeric(FemaleDie[which(AGE >= fertstart & AGE<=fertend)])),
            fertlength = unique(fertlength),
            fertstart = unique(fertstart),
            fertend = unique(fertend))
prim22 <- left_join(prim2children, prim2women) %>%
  left_join(., prim2tfr)

# Calculating the total number of women, the total number of children, and the width of the fertility interval for the lions
lion <- jones %>%
  group_by(Species) %>%
  summarise(Women = sum(as.numeric(N.x.female[which(Age>= fertstart & Age<=fertend)])),
            child = sum(as.numeric(N.x.female[Age ==0])),
            fertlength = unique(fertlength),
            fertstart = unique(fertstart),
            fertend = unique(fertend))

# subsetting the PRIM_DATA to exclude Sifaka. summing the total number of children, estimating nFx
d <- PRIM_DATA %>%
  group_by(Species) %>%
  mutate(child = N.x.female + N.x.male + N.x.unknown,
         nFx = Offspring / Breeding_females,
         nFx = ifelse(is.na(nFx),0, nFx),
         fertstart = min(Age[which(nFx>0)]),
         fertend = max(Age[which(nFx>0)]),
         fertlength = fertend - fertstart)

# Selecting the number of children from the Primate data
children <- d %>%
  filter(Age == 0) %>%
  dplyr::select(Species, child)

# Summing the number of child-bearing Women and the fertility interval by species for the primate data.
women <- d %>%
  group_by(Species) %>%
  summarize(Women = sum(N.x.female[which(Age>= fertstart & Age<=fertend)]),
            fertlength = unique(fertlength),
            fertstart = unique(fertstart),
            fertend = unique(fertend))

# rbinding the primate data and the lion data. Summing the nFx values to arrive at a TFR
TFR_table <- bind_rows(d, jones) %>%
  group_by(Species) %>%
  summarise(TFR_table = sum(as.numeric(nFx)))

# joining the child/women data from the primates. Then rbinding it with the lion data
cw <- inner_join(children, women) %>%
  bind_rows(., lion)

# Joining the child/woman data with the oTFR data. Calculating the iTFR
combined <- left_join(cw, TFR_table) %>%
  bind_rows(., prim22) %>%
  mutate(iTFR = (child/Women) * fertlength)