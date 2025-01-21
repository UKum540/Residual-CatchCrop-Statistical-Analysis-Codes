
# Foulum
## Analysis for catch crop nitrogen uptake

### Import data

d1 <- read.xlsx(load data)


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

d41 = d1[d1$year%in%c(2016, 2017, 2018, 2019),]
d44 = d1[d1$year%in%c(2020, 2021),]


## Analyses for four percolation periods of repeated catch crops


d41Fk = d41[d41$site =="Flakkebjerg",]
d41Fk$bs = paste(d41Fk$system, d41Fk$block, sep = "_")
d41Fk$fertiliser.level = as.factor(d41Fk$fertiliser.level)
d41Fk$treatment = as.factor(d41Fk$treatment)
d41Fk$block = as.factor(d41Fk$block)


### Statistical model for analysis


mod0_1 <- glmmTMB((catch.cropN) ~ as.factor(year2)*system*as.factor(fertiliser.level) + (1|block),
                   family = Gamma(link = "log"),
                  data = d41Fk)

### Pairwise comparison


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


d44Fk = d44[d44$site =="Flakkebjerg",]
d44Fk$fertiliser.level = as.factor(d44Fk$fertiliser.level)
d44Fk$treatment = as.factor(d44Fk$treatment)
d44Fk$block = as.factor(d44Fk$block)

### Statistical model for analysis


mod0_1 <- glmmTMB((catch.cropN) ~ as.factor(year2)*as.factor(fertiliser.level) + (1|block),
                   family = Gamma(link = "log"),
                   data = d44Fk)


### Pairwise comparision

treatment_means2 <- emmeans(mod0_1, pairwise ~ fertiliser.level|year2, type = "unlink")
treatment_means2 <- cld(object = treatment_means2[[1]],
                         adjust = "holm",
                         Letters = letters,
                         alpha = 0.05)
