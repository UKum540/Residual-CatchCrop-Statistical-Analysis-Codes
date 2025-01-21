

# Foulum
## Analysis for catch crop nitrogen uptake

### Import data

d1 <- read.xlsx(laod data)


d1$id = paste(d1$treatment, d1$fertiliser.level, d1$block, sep = "_")

d1$year2 <- ifelse(d1$year == 2016, 1,
                      ifelse(d1$year == 2017, 2,
                      ifelse(d1$year == 2018, 3,
                      ifelse(d1$year == 2019, 4,
                             ifelse(d1$year == 2020, 5, ifelse(d1$year == 2021, 6, d1$year))))))
d1$fertiliser.level2 <- ifelse(d1$fertiliser.level == 0, "0 N",
                      ifelse(d1$fertiliser.level == 0.5, "0.5 N",
                      ifelse(d1$fertiliser.level == 1, "1 N",
                      ifelse(d1$fertiliser.level == 1.5, "1.5 N",d1$fertiliser.level
                            ))))
d1$system <-ifelse(d1$year2 == 5 & d1$system == "vw", "vw to cc", 
                   ifelse(d1$year2 == 6 & d1$system == "vw", "vw to cc", d1$system ))

d41 = d1[d1$year%in%c(2016, 2017, 2018, 2019),]
d44 = d1[d1$year%in%c(2020, 2021),]

## Analyses for four percolation periods of repeated catch crops


d41Fl = d41[d41$site =="Foulum",]
d41Fl$bs = paste(d41Fl$system, d41Fl$block, sep = "_")
d41Fl$fertiliser.level = as.factor(d41Fl$fertiliser.level)
d41Fl$treatment = as.factor(d41Fl$treatment)
d41Fl$block = as.factor(d41Fl$block)

### Statistical model for analysis


mod0_1 <- glmmTMB((catch.cropN) ~ as.factor(year2)*system*as.factor(fertiliser.level) + (1|block),
                   family = Gamma(link = "log"), data = d41Fl)


### Pairwise comparision

treatment_means2 <- emmeans(mod0_1, pairwise ~ system|fertiliser.level|year2, type = "unlink")
treatment_means2 <- cld(object = treatment_means2[[1]],
                         adjust = "holm",
                         Letters = LETTERS,
                         alpha = 0.05)

treatment_means3 <- emmeans(mod0_1, pairwise ~ fertiliser.level|system|year2, type = "unlink")
treatment_means3 <- cld(object = treatment_means3[[1]],
                         adjust = "holm",
                         Letters = letters,
                         alpha = 0.05)


## Analyses for last two percolation periods of non-repeated catch crops



d44Fl = d44[d44$site =="Foulum",]
d44Fl$fertiliser.level = as.factor(d44Fl$fertiliser.level)
d44Fl$treatment = as.factor(d44Fl$treatment)
d44Fl$block = as.factor(d44Fl$block)


### Statistical model for analysis


mod0_1 <- glmmTMB((catch.cropN) ~ as.factor(year2)*as.factor(fertiliser.level) + (1|block),
                   family = Gamma(link = "log"), data = d44Fl)


### Pairwise comparision


treatment_means2 <- emmeans(mod0_1, pairwise ~ fertiliser.level|year2, type = "unlink")
treatment_means2 <- cld(object = treatment_means2[[1]],
                         adjust = "holm",
                         Letters = letters,
                         alpha = 0.05)

