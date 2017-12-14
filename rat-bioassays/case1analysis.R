library(ggplot2); library(dplyr); library(reshape2)

#QUESTION: IS THE BIOASSAY USEFUL???
#USEFULNESS IS DETERMINED BY CONSISTENT RESULTS ACROSS LABORATORIES
#GOALS OF THE STUDY: FIND THE BEST PROTOCOL? IS WET VS BLOTTED WEIGHT BETTER RESPONSE?

#### exploration and cleaning ####
dat <- read.table("alllabs.txt", header = TRUE, stringsAsFactors = FALSE, na.strings = ".")
dat <- dat[!(is.na(dat$blot) | is.na(dat$body)),]
weights <- dat[,c("body", "wet", "blot")]
weights_long <- melt(weights, id = "body")


ggplot(data = weights_long, 
       mapping = aes(x=body, y=value, color=variable)
      ) + geom_point()
# the difference between wet and blot for equal body weight follows a strange pattern at around body = 200, wet spikes but blot does not
# also for some reason there is a large gap in body weights from 68.6 to 148.0
# appears to be no direct relationship between body wt and uteris wt

#investigating that strange gap in body weights
grpweights <- dat[,c("body", "blot","lab", "proto", "group")]

ggplot(data = grpweights, 
       mapping = aes(x=body, y=blot, color=proto)
) + geom_point()
#protocol A & B are little rats, while C & D are big rats

labweights <- dat[,c("body", "wet", "blot", "lab")]
labweights_long <- melt(labweights, id="lab")
ggplot(data = labweights_long,
       mapping = aes(x=lab, y=log(value), color = variable)
       ) + geom_boxplot()
#it appears that all the the weights vary by lab

protoweights <- dat[,c("body", "wet", "blot", "proto")]
protoweights_long <- melt(protoweights, id="proto")
ggplot(data = protoweights_long,
       mapping = aes(x=proto, y=log(value), color = variable)
) + geom_boxplot()

#more protocol investifations
boxplot(log(dat$blot[dat$proto %in% c("C","D")]) ~ (dat$dose1[dat$proto %in% c("C","D")]), main="adults")




#each group has a unique dosage combination. not all possible combinations are explored.
dosage_groups <-  dat %>% 
                  group_by(group) %>% 
                  summarize(dose1 = unique(dose1), 
                            dose2 = unique(dose2),
                            A = sum(proto == "A"),
                            B = sum(proto == "B"),
                            C = sum(proto == "C"),
                            D = sum(proto == "D")
                  ) %>% as.data.frame()
#since you're "exposed to antagonist at higher levels of agonist" then we assume 2 is the antagonist and 1 is the agonist
#also assume group#2 is the vehicle control

#### outline several approaches ####

#super naive model 1
# log(uterus weight) ~ Norm(mu, sigma2)

doseweights <- dat %>% mutate(doseg)

ggplot(data = dat ,
       mapping=aes(x=as.factor(group), y=log(blot))) + geom_boxplot()


