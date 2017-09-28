library(survival) 
# https://rstudio-pubs-static.s3.amazonaws.com/5588_72eb65bfbe0a4cb7b655d2eee0751584.html
# http://rstudio-pubs-static.s3.amazonaws.com/5896_8f0fed2ccbbd42489276e554a05af87e.html
library(dplyr); library(ggplot2)

kellydat <- read.table("kellydat.txt", header = TRUE, stringsAsFactors = FALSE)

# Variable key: 
# nctdel  = min of neurologist time to assessment & CT scan from arrival at ER
# fail    = 1 if got neurologist/CT scan & 0 otherwise
# male    = 1 if male, 0=female
# black   = 1 if black, 0=not black 
# hisp    = 1 if hispanic, 0=not hispanic
# sn1     = 0/1 indicator 1 main symptom
# sn2     = 0/1 indicator 2 main symptoms
# sn3     = 0/1 indicator 3 main symptoms 
# all4    = 0/1 indicator all 4 main symptoms

#symptom var doesn't tell us WHICH symptoms :(
#no NA's! :)
#no one has sn1 - all4 recorded more than once. :)

#the response is right skewed but we don't need it to be gaussian do we?

pfail <- summary(aov(log(nctdel+1) ~ fail, data = kellydat))[[1]][["Pr(>F)"]][1]
pmale <- summary(aov((nctdel+1) ~ male, data = kellydat))[[1]][["Pr(>F)"]][1]
pblack <- summary(aov(log(nctdel+1) ~ black, data = kellydat))[[1]][["Pr(>F)"]][1]
phisp <- summary(aov(log(nctdel+1) ~ hisp, data = kellydat))[[1]][["Pr(>F)"]][1]
p1 <- summary(aov(log(nctdel+1) ~ sn1, data = kellydat))[[1]][["Pr(>F)"]][1]
p2 <- summary(aov(log(nctdel+1) ~ sn2, data = kellydat))[[1]][["Pr(>F)"]][1]
p3 <- summary(aov(log(nctdel+1) ~ sn3, data = kellydat))[[1]][["Pr(>F)"]][1]
p4 <- summary(aov(log(nctdel+1) ~ all4, data = kellydat))[[1]][["Pr(>F)"]][1]

ggplot(kellydat,
       aes(x=fail, group=as.factor(fail), y=log(nctdel+1))) + 
  geom_boxplot() + 
  labs(title = "nctdel by fail",
       caption = paste("p-value:",p1))

ggplot(kellydat,
       aes(x=male, group=as.factor(fail), y=log(nctdel+1))) + 
  geom_boxplot() + 
  labs(title = "nctdel by fail",
       caption = paste("p-value:",pmale))



boxplot(log(nctdel+1) ~ male, data = kellydat)  #males wait slightly longer
boxplot(log(nctdel+1) ~ black, data = kellydat) #black ppl wait slightly longer

kelly.surv <- Surv(time = kellydat$nctdel, event = kellydat$fail)
kelly.cox <- coxph()