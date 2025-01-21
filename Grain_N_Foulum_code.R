
# Foulum
## Analysis for spring barley grain N


d1 <- read.xlsx(Load data)

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

d11 = d1[d1$year%in%c(2016, 2017, 2018, 2019, 2020),]
d14 = d1[d1$year%in%c(2021),]


### Analyses for five years of repeated catch crops


d11Fl = d11[d11$site =="Foulum",]
d11Fl$bs = paste(d11Fl$system, d11Fl$block, sep = "_")
d11Fl$fertiliser.level = as.factor(d11Fl$fertiliser.level)
d11Fl$treatment = as.factor(d11Fl$treatment)
d11Fl$block = as.factor(d11Fl$block)


### Statistical model for analysis


mod0_1 <- glmmTMB((grainN) ~ as.factor(year2)*system*as.factor(fertiliser.level) + (1|block),
                 family = gaussian(link = "log"),
                data = d11Fl)


### Pairwise comparision

treatment_means <- emmeans(mod0_1, pairwise ~ fertiliser.level|system|year2, type = "unlink")
treatment_means <- cld(object = treatment_means[[1]],
                         adjust = "holm",
                         Letters = letters,
                         alpha = 0.05)


treatment_means2 <- emmeans(mod0_1, pairwise ~ system|fertiliser.level|year2)
treatment_means22 <- cld(object = treatment_means2[[1]],
                         adjust = "holm",
                         Letters = LETTERS,
                         alpha = 0.05)


treatment_means3 <-regrid(treatment_means2, "response")
a1 = pairs(treatment_means3, reverse=TRUE, type="response")


## Analyses for last one non-repeated catch crop year

d14Fl = d14[d14$site =="Foulum",]
d14Fl$fertiliser.level = as.factor(d14Fl$fertiliser.level)
d14Fl$block = as.factor(d14Fl$block)


### Statistical model for analysis


mod0_1 <- glmmTMB(grainN ~ system*as.factor(fertiliser.level)+(1|block), data = d14Fl)


### Pairwise comparision

treatment_means <- emmeans(mod0_1, pairwise ~ fertiliser.level|system, type = "response")
treatment_means <- cld(object = treatment_means[[1]],
                         adjust = "holm",
                         Letters = letters,
                         alpha = 0.05)

treatment_means2 <- emmeans(mod0_1, pairwise ~ system|fertiliser.level, type = "response")
treatment_means22 <- cld(object = treatment_means2[[1]],
                         adjust = "holm",
                         Letters = LETTERS,
                         alpha = 0.05)

treatment_means3 <-regrid(treatment_means2, "response")
a1 = pairs(treatment_means3, reverse=TRUE, type="response")

