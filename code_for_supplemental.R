#code for figures and statistical tests for shark mucus paper

#packages:
library(lme4)
library(tidyverse)
library(ggbiplot)

area<-read.csv('shark.area.7.17.csv',fileEncoding='latin1',check.names=F)
area$ID <- as.character(area$ID)
str(area)
colnames(area)[1] <- "Region"

area.dors<-read.csv('post.area.dorsal.csv',fileEncoding='latin1',check.names=F)
area.dors$ID <- as.character(area.dors$ID)
str(area.dors)
colnames(area.dors)[1] <- "Region"

density<-read.csv('shark.density.csv',fileEncoding='latin1',check.names=F)
density$ID <- as.character(density$ID)
str(density)
colnames(density)[1] <- "Region"

mm<-read.csv('shark.mm.7.17.csv',fileEncoding='latin1',check.names=F)
mm$ID <- as.character(mm$ID)
str(mm)
colnames(mm)[1] <- "Region"


#roughness t-tests
library (lme4)
mod.region <-lmer(Sq ~ Region + (1|ID), data=mm)
summary(mod.region)
lsmeans(mod.region, pairwise~Region, adjust="tukey")
mm.dfintip<-mm[which(mm$Region=="dfin.tip"),] 
mod.dfintip<-lmer(Sq ~ Condition + (1|ID), data=mm.dfintip)
summary(mod.dfintip)
lsmeans(mod.dfintip, pairwise~Condition, adjust="tukey")
mm.dfinlat<-mm[which(mm$Region=="dfin.lat"),] 
mod.dfinlat<-lmer(Sq ~ Condition + (1|ID), data=mm.dfinlat)
summary(mod.dfinlat)
lsmeans(mod.dfinlat, pairwise~Condition, adjust="tukey")
mm.lat<-mm[which(mm$Region=="lat"),]
mod.lat<-lmer(Sq ~ Condition + (1|ID), data=mm.lat)
summary(mod.lat)
lsmeans(mod.lat, pairwise~Condition, adjust="tukey")
mm.nose<-mm[which(mm$Region=="nose"),]
mod.nose<-lmer(Sq ~ Condition + (1|ID), data=mm.nose)
summary(mod.nose)
lsmeans(mod.nose, pairwise~Condition, adjust="tukey")
mm.post<-mm[which(mm$Region=="post"),]
mod.post<-lmer(Sq ~ Condition + (1|ID), data=mm.post)
summary(mod.post)
lsmeans(mod.post, pairwise~Condition, adjust="tukey")
mm.taillat<-mm[which(mm$Region=="tail.lat"),]
mod.taillat<-lmer(Sq ~ Condition + (1|ID), data=mm.taillat)
summary(mod.taillat)
lsmeans(mod.taillat, pairwise~Condition, adjust="tukey")
mm.tailte<-mm[which(mm$Region=="tail.te"),]
mod.tailte<-lmer(Sq ~ Condition + (1|ID), data=mm.tailte)
summary(mod.tailte)
lsmeans(mod.tailte, pairwise~Condition, adjust="tukey")
mm.head<-mm[which(mm$Region=="tophead"),]
mod.head<-lmer(Sq ~ Condition + (1|ID), data=mm.head)
summary(mod.head)
lsmeans(mod.head, pairwise~Condition, adjust="tukey")

#skew tests
ssk.dfintip<-mm[which(mm$Region=="dfin.tip"),]
ssk.mod.dfintip<-lmer(Ssk ~ Condition + (1|ID), data=ssk.dfintip) 
summary(ssk.mod.dfintip)
lsmeans(ssk.mod.dfintip, pairwise~Condition, adjust="tukey")
ssk.dfinlat<-mm[which(mm$Region=="dfin.lat"),]
ssk.mod.dfinlat<-lmer(Ssk ~ Condition + (1|ID), data=ssk.dfinlat)
summary(ssk.mod.dfinlat)
lsmeans(ssk.mod.dfinlat, pairwise~Condition, adjust="tukey")
ssk.lat<-mm[which(mm$Region=="lat"),]
ssk.mod.lat<-lmer(Ssk ~ Condition + (1|ID), data=ssk.lat)
summary(ssk.mod.lat)
lsmeans(ssk.mod.lat, pairwise~Condition, adjust="tukey")
ssk.nose<-mm[which(mm$Region=="nose"),]
ssk.mod.nose<-lmer(Ssk ~ Condition + (1|ID), data=ssk.nose)
summary(ssk.mod.nose)
lsmeans(ssk.mod.nose, pairwise~Condition, adjust="tukey")
ssk.post<-mm[which(mm$Region=="post"),]
ssk.mod.post<-lmer(Ssk ~ Condition + (1|ID), data=ssk.post)
summary(ssk.mod.post)
lsmeans(ssk.mod.post, pairwise~Condition, adjust="tukey")
ssk.taillat<-mm[which(mm$Region=="tail.lat"),]
ssk.mod.taillat<-lmer(Ssk ~ Condition + (1|ID), data=ssk.taillat)
summary(ssk.mod.taillat)
lsmeans(ssk.mod.taillat, pairwise~Condition, adjust="tukey")
ssk.tailte<-mm[which(mm$Region=="tail.te"),]
ssk.mod.tailte<-lmer(Ssk ~ Condition + (1|ID), data=ssk.tailte)
summary(ssk.mod.tailte)
lsmeans(ssk.mod.tailte, pairwise~Condition, adjust="tukey")
ssk.head<-mm[which(mm$Region=="tophead"),]
ssk.mod.head<-lmer(Ssk ~ Condition + (1|ID), data=ssk.head)
summary(ssk.mod.head)
lsmeans(ssk.mod.head, pairwise~Condition, adjust="tukey")

#kurtosis tests
sku.dfintip<-mm[which(mm$Region=="dfin.tip"),]
sku.mod.dfintip<-lmer(Sku ~ Condition + (1|ID), data=sku.dfintip) 
summary(sku.mod.dfintip)
lsmeans(sku.mod.dfintip, pairwise~Condition, adjust="tukey")
sku.dfinlat<-mm[which(mm$Region=="dfin.lat"),]
sku.mod.dfinlat<-lmer(Sku ~ Condition + (1|ID), data=sku.dfinlat)
summary(sku.mod.dfinlat)
lsmeans(sku.mod.dfinlat, pairwise~Condition, adjust="tukey")
sku.lat<-mm[which(mm$Region=="lat"),]
sku.mod.lat<-lmer(Sku ~ Condition + (1|ID), data=sku.lat)
summary(sku.mod.lat)
lsmeans(sku.mod.lat, pairwise~Condition, adjust="tukey")
sku.nose<-mm[which(mm$Region=="nose"),]
sku.mod.nose<-lmer(Sku ~ Condition + (1|ID), data=sku.nose)
summary(sku.mod.nose)
lsmeans(sku.mod.nose, pairwise~Condition, adjust="tukey")
sku.post<-mm[which(mm$Region=="post"),]
sku.mod.post<-lmer(Sku ~ Condition + (1|ID), data=sku.post)
summary(sku.mod.post)
lsmeans(sku.mod.post, pairwise~Condition, adjust="tukey")
sku.taillat<-mm[which(mm$Region=="tail.lat"),]
sku.mod.taillat<-lmer(Sku ~ Condition + (1|ID), data=sku.taillat)
summary(sku.mod.taillat)
lsmeans(sku.mod.taillat, pairwise~Condition, adjust="tukey")
sku.tailte<-mm[which(mm$Region=="tail.te"),]
sku.mod.tailte<-lmer(Sku ~ Condition + (1|ID), data=sku.tailte)
summary(sku.mod.tailte)
lsmeans(sku.mod.tailte, pairwise~Condition, adjust="tukey")
sku.head<-mm[which(mm$Region=="tophead"),]
sku.mod.head<-lmer(Sku ~ Condition + (1|ID), data=sku.head)
summary(sku.mod.head)
lsmeans(sku.mod.head, pairwise~Condition, adjust="tukey")

#area ratio tests
sdr.dfintip<-mm[which(mm$Region=="dfin.tip"),]
sdr.mod.dfintip<-lmer(Sdr ~ Condition + (1|ID), data=sdr.dfintip) 
summary(sdr.mod.dfintip)
lsmeans(sdr.mod.dfintip, pairwise~Condition, adjust="tukey")
sdr.dfinlat<-mm[which(mm$Region=="dfin.lat"),]
sdr.mod.dfinlat<-lmer(Sdr ~ Condition + (1|ID), data=sdr.dfinlat)
summary(sdr.mod.dfinlat)
lsmeans(sdr.mod.dfinlat, pairwise~Condition, adjust="tukey")
sdr.lat<-mm[which(mm$Region=="lat"),]
sdr.mod.lat<-lmer(Sdr ~ Condition + (1|ID), data=sdr.lat)
summary(sdr.mod.lat)
lsmeans(sdr.mod.lat, pairwise~Condition, adjust="tukey")
sdr.nose<-mm[which(mm$Region=="nose"),]
sdr.mod.nose<-lmer(Sdr ~ Condition + (1|ID), data=sdr.nose)
summary(sdr.mod.nose)
lsmeans(sdr.mod.nose, pairwise~Condition, adjust="tukey")
sdr.post<-mm[which(mm$Region=="post"),]
sdr.mod.post<-lmer(Sdr ~ Condition + (1|ID), data=sdr.post)
summary(sdr.mod.post)
lsmeans(sdr.mod.post, pairwise~Condition, adjust="tukey")
sdr.taillat<-mm[which(mm$Region=="tail.lat"),]
sdr.mod.taillat<-lmer(Sdr ~ Condition + (1|ID), data=sdr.taillat)
summary(sdr.mod.taillat)
lsmeans(sdr.mod.taillat, pairwise~Condition, adjust="tukey")
sdr.tailte<-mm[which(mm$Region=="tail.te"),]
sdr.mod.tailte<-lmer(Sdr ~ Condition + (1|ID), data=sdr.tailte)
summary(sdr.mod.tailte)
lsmeans(sdr.mod.tailte, pairwise~Condition, adjust="tukey")
sdr.head<-mm[which(mm$Region=="tophead"),]
sdr.mod.head<-lmer(Sdr ~ Condition + (1|ID), data=sdr.head)
summary(sdr.mod.head)
lsmeans(sdr.mod.head, pairwise~Condition, adjust="tukey")

#exposed denticle area tests
area.dfintip<-area[which(area$Region=="dfin.tip"),]
area.mod.dfintip<-lmer(Exposed.Area ~ Condition + (1|ID), data=area.dfintip) 
summary(area.mod.dfintip)
lsmeans(area.mod.dfintip, pairwise~Condition, adjust="tukey")
area.dfinlat<-area[which(area$Region=="dfin.lat"),]
area.mod.dfinlat<-lmer(Exposed.Area ~ Condition + (1|ID), data=area.dfinlat)# new p-value 0.00025 (still sig?), after resampling all- 0.06237--> old p-value? 
summary(area.mod.dfinlat)#wrong data in spreadsheet?--> resample i think
lsmeans(area.mod.dfinlat, pairwise~Condition, adjust="tukey")
area.lat<-area[which(area$Region=="lat"),]
area.mod.lat<-lmer(Exposed.Area ~ Condition + (1|ID), data=area.lat)
summary(area.mod.lat)
lsmeans(area.mod.lat, pairwise~Condition, adjust="tukey")
area.nose<-area[which(area$Region=="nose"),]
area.mod.nose<-lmer(Exposed.Area ~ Condition + (1|ID), data=area.nose)
summary(area.mod.nose)
lsmeans(area.mod.nose, pairwise~Condition, adjust="tukey")
area.dors.post<-area.dors[which(area.dors$Region=="Post. Body"),]
area.dors.mod.post<-lmer(Exposed.Area ~ Condition + (1|ID), data=area.dors.post) #right code for post area (11/19)
summary(area.dors.mod.post)
lsmeans(area.dors.mod.post, pairwise~Condition, adjust="tukey")
area.head<-area[which(area$Region=="tophead"),]
area.mod.head<-lmer(Exposed.Area ~ Condition + (1|ID), data=area.head)
summary(area.mod.head)
lsmeans(area.mod.head, pairwise~Condition, adjust="tukey")
area.tailte<-area[which(area$Region=="tail.te"),]
area.mod.tailte<-lmer(Exposed.Area ~ Condition + (1|ID), data=area.tailte)
summary(area.mod.tailte)
lsmeans(area.mod.tailte, pairwise~Condition, adjust="tukey")
area.taillat<-area[which(area$Region=="tail.lat"),]
area.mod.taillat<-lmer(Exposed.Area ~ Condition + (1|ID), data=area.taillat)
summary(area.mod.taillat)
lsmeans(area.mod.taillat, pairwise~Condition, adjust="tukey")

#exposed denticle density tests
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Dorsal Fin Center", var.equal = TRUE)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Dorsal Fin Center", var.equal = FALSE)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Dorsal Fin Tip", var.equal = TRUE)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Dorsal Fin Tip", var.equal = F)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Mid. Body", var.equal = TRUE)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Mid. Body", var.equal = F)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Nose Tip", var.equal = TRUE)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Nose Tip", var.equal = F)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Post. Body", var.equal = TRUE)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Post. Body", var.equal = F)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Tail Center", var.equal = TRUE)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Tail Center", var.equal = F)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Tail Trail. Edge", var.equal = TRUE)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Tail Trail. Edge", var.equal = F)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Head", var.equal = TRUE)
t.test(density$Exposed.Density ~ density$Condition, subset = density$Region == "Head", var.equal = F)

#code for plots
library (tidyverse)
mm<-read.csv('shark.mm.7.17.csv',fileEncoding='latin1',check.names=F)#new spreadsheet w/ some variables removed
mm$ID <- as.character(mm$ID)#id as chr and not numeric
str(mm)
colnames(mm)[1] <- "Region" #way to rename columns and fix if any issues
mm$Region_Condition <- paste(mm$Region, mm$Condition, sep = "_")#combine location and condition 
mm$ID_Condition_Region <- paste(mm$ID, mm$Condition, mm$Region, sep = "_")#combine all 3
mm$ID_Region <- paste(mm$ID, mm$Region, sep = "_") #combines id and location (in case any ind is weird)
mm$ID_Condition <- paste(mm$ID, mm$Condition, sep = "_")#combines id and condition (in case any ind is weird)

labels2 <- c('Alive Nose Tip','Dead Nose Tip', 'Alive Head Top', 'Dead Head Top','Alive Dorsal Fin Center', 'Dead Dorsal Fin Center', 'Alive Dorsal Fin Tip', 'Dead Dorsal Fin Tip', 'Alive Mid. Body', 'Dead Mid. Body', 'Alive Post. Body', 'Dead Post. Body', 'Alive Tail Center', 'Dead Tail Center','Alive Tail Trail. Edge', 'Dead Tail Trail. Edge')
#sq boxplot
sqplot2 <- ggplot(mm, aes(x=Region_Condition, y=Sq, fill=Condition)) + 
  geom_boxplot(lwd= 1.2, color = "black", alpha = 1, position = position_dodge(1), fatten = 0.8, coef=NULL)+ theme_classic() + theme(legend.position = "top")+ 
  geom_jitter(size = 2.1, color = "black", alpha = 1, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75))  +  scale_fill_manual(values=c("steelblue1", "steelblue4")) + labs(y="Roughness (µm)",x="Body Region") +theme(axis.text =element_text (size = 20, face = "bold")) +theme(axis.title = element_text (size = 26, face = "bold")) + theme(legend.text =element_text (size = 18, face = "bold")) +theme(legend.title = element_text (size = 20, face = "bold")) + geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5), linetype="solid", color="black", linewidth=0.5, alpha=1) + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1) + theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")))
sqplot3 <-sqplot2 + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1))+ theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))
plot(sqplot3)
#skew boxplot
skuplot2 <- ggplot(mm, aes(x=Region_Condition, y=Sku, fill=Condition)) + 
  geom_boxplot(lwd= 1.2, color = "black", alpha = 1, position = position_dodge(1), fatten = 0.8, coef=NULL)+ theme_classic() + theme(legend.position = "top")+ 
  geom_jitter(size = 2.1, color = "black", alpha = 1, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75))  +  scale_fill_manual(values=c("steelblue1", "steelblue4")) + labs(y="Sku",x="Body Region") +theme(axis.text =element_text (size = 20, face = "bold")) +theme(axis.title = element_text (size = 26, face = "bold")) + theme(legend.text =element_text (size = 18, face = "bold")) +theme(legend.title = element_text (size = 20, face = "bold")) + geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5), linetype="solid", color="black", linewidth=0.5, alpha=1) + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1) + theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")))
skuplot3 <-skuplot2 + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1))+ theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))
plot(skuplot3)
#kurtosis boxplot
sskplot2 <- ggplot(mm, aes(x=Region_Condition, y=Ssk, fill=Condition)) + 
  geom_boxplot(lwd= 1.2, color = "black", alpha = 1, position = position_dodge(1), fatten = 0.8, coef=NULL)+ theme_classic() + theme(legend.position = "top")+ 
  geom_jitter(size = 2.1, color = "black", alpha = 1, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75))  +  scale_fill_manual(values=c("steelblue1", "steelblue4")) + labs(y="Ssk",x="Body Region") +theme(axis.text =element_text (size = 20, face = "bold")) +theme(axis.title = element_text (size = 26, face = "bold")) + theme(legend.text =element_text (size = 18, face = "bold")) +theme(legend.title = element_text (size = 20, face = "bold")) + geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5), linetype="solid", color="black", linewidth=0.5, alpha=1) + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1) + theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")))
sskplot3 <-sskplot2 + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1))+ theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))
plot(sskplot3)
#area ratio boxplot
sdrplot2 <- ggplot(mm, aes(x=Region_Condition, y=Sdr, fill=Condition)) + 
  geom_boxplot(lwd= 1.2, color = "black", alpha = 1, position = position_dodge(1), fatten = 0.8, coef=NULL)+ theme_classic() + theme(legend.position = "top")+ 
  geom_jitter(size = 2.1, color = "black", alpha = 1, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75))  +  scale_fill_manual(values=c("steelblue1", "steelblue4")) + labs(y="Sdr (%)",x="Body Region") +theme(axis.text =element_text (size = 20, face = "bold")) +theme(axis.title = element_text (size = 26, face = "bold")) + theme(legend.text =element_text (size = 18, face = "bold")) +theme(legend.title = element_text (size = 20, face = "bold")) + geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5), linetype="solid", color="black", linewidth=0.5, alpha=1) + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1) + theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")))
sdrplot3 <-sdrplot2 + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1))+ theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))
plot(sdrplot3)
#area boxplot
areaplot2 <- ggplot(area, aes(x=Region_Condition, y=Exposed.Area, fill=Condition)) + 
  geom_boxplot(lwd= 1.2, color = "black", alpha = 1, position = position_dodge(1), fatten = 0.8, coef=NULL)+ theme_classic() + theme(legend.position = "top")+ 
  geom_jitter(size = 2.1, color = "black", alpha = 1, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75))  +  scale_fill_manual(values=c("steelblue1", "steelblue4")) + labs(y="Exposed Area (mm2)",x="Body Region") +theme(axis.text =element_text (size = 20, face = "bold")) +theme(axis.title = element_text (size = 26, face = "bold")) + theme(legend.text =element_text (size = 18, face = "bold")) +theme(legend.title = element_text (size = 20, face = "bold")) + geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5), linetype="solid", color="black", linewidth=0.5, alpha=1) + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1) + theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")))
areaplot3 <-areaplot2 + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1))+ theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))
plot(areaplot3)
#density boxplot
density$Region_Condition <- factor(density$Region_Condition, levels = c('Nose Tip_Alive', 'Nose Tip_Dead', 'Head_Alive', 'Head_Dead', 'Dorsal Fin Center_Alive', 'Dorsal Fin Center_Dead', 'Dorsal Fin Tip_Alive', 'Dorsal Fin Tip_Dead', 'Mid. Body_Alive', 'Mid. Body_Dead', 'Post. Body_Alive', 'Post. Body_Dead', 'Tail Center_Alive', 'Tail Center_Dead', 'Tail Trail. Edge_Alive', 'Tail Trail. Edge_Dead'))
densityplot2 <- ggplot(density, aes(x=Region_Condition, y=Exposed.Density, fill=Condition)) + 
  geom_boxplot(lwd= 1.2, color = "black", alpha = 1, position = position_dodge(1), fatten = 0.8, coef=NULL)+ theme_classic() + theme(legend.position = "top")+ 
  geom_jitter(size = 2.1, color = "black", alpha = 1, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75))  +  scale_fill_manual(values=c("steelblue1", "steelblue4")) + labs(y="Exposed Density (# of denticles/mm2)",x="Body Region") +theme(axis.text =element_text (size = 20, face = "bold")) +theme(axis.title = element_text (size = 26, face = "bold")) + theme(legend.text =element_text (size = 18, face = "bold")) +theme(legend.title = element_text (size = 20, face = "bold")) + geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5), linetype="solid", color="black", linewidth=0.5, alpha=1) + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1) + theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")))
densityplot3 <-densityplot2 + scale_x_discrete(labels = function(labels) str_wrap(labels2, width = 1))+ theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))
plot(densityplot3)

#pca plot and code
library(ggbiplot)
pca_all <- read.csv("pca.all.csv")
shark.pca.all <- prcomp (~ Sq + Ssk + Sku + Sdr + Exposed.Area + Exposed.Density,
                         data=pca_all,
                         scale = TRUE,
                         center = TRUE)
g <- ggbiplot(shark.pca.all, obs.scale = 1, var.scale = 1, varname.size = 6, varname.adjust = 2,
              groups=pca_all$Region) +
  geom_point(aes(colour = pca_all$Region, shape = pca_all$Condition), size = 7) + ylim(-3,3) +
  
  scale_color_manual(values= c("#56B4E9", "#416BD4", "#E69F00","#771483","#009E73","#7E5841","#E34242","#841111")) +
  
  theme_classic() +theme(legend.direction = 'vertical',
                         legend.position.inside = c(0,3), legend.background = element_rect(fill = "white", color = "black"), legend.text= element_text (size = 12), legend.title =element_text (size = 12)) +
  labs(color ="Region", shape = "Condition") 
print(g)

