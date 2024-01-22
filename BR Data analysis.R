### Boiling River ###

#Set working dir
setwd("C:/Users/rxf568/OneDrive - University of Miami/Peru - Boiling River/Data analysis")

#load libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(readr)
library(vegan)
library(reshape2)
library(stringr)
library(ggplot2)
library(gridExtra)
library(sjPlot)
library(Hmisc)
library(betapart)
library(plotly)
library(gstat)
library(cluster)
library(otuSummary)

### Input plot characteristics data
plot_char <- read.csv("Plot_chars.csv")
plot_char <- plot_char[,-c(1)]
plot_char <- dplyr::rename(plot_char, air.temp = tms_clim15_Tmax)

# Input soil data and match with plot_char
soil <- read_excel("Soil_data_new.xlsx")
m <- match(plot_char$Plot,soil$`Near plots`)
plot_char$Ca_available <- soil$Ca_available[m]
plot_char$pH <- soil$pH[m]
plot_char$Zn <- soil$Zn[m]
plot_char$N_total <- soil$N_total[m]
plot_char$Mn <- soil$Mn[m]
plot_char$Mg_available <- soil$Mg_available[m]
plot_char$Fe <- soil$Fe[m]
plot_char$Cu <- soil$Cu[m]
plot_char$K_available <- soil$K_available[m]
plot_char$B_soluble <- soil$B_soluble[m]
plot_char$P_available <- soil$P_available[m]

# Change N_total from % to g/kg
plot_char$N_total <- (plot_char$N_total/100)*1000

# make a correlation matrix for soil variables
cor_mat <- rcorr(as.matrix(plot_char[, c(11:21)]))

# make a separate dataframe for plots with soil nutrients
plot_char_soil <- subset(plot_char, !is.na(plot_char$Ca_available))

### Input composition data ###
comp <- read_excel("BR_Plot_Data.xlsx", 
                      sheet = "Sheet1")
bot_det <- read_excel("DETERMINACIONES BOTANICAS - RIO HIRVIENTE.xlsx")

#Collate Roy's determinations and the plot data
comp$Coll.no <- parse_number(comp$Coll)

m <- match(comp$Coll.no, bot_det$Coll.no)

comp$famdet <- bot_det$Familia[m]
comp$gendet <- bot_det$Genero[m]
comp$spdet <- bot_det$Especie[m]
rm(m)
rm(bot_det)

comp$Family <- ifelse(is.na(comp$famdet), comp$Family, comp$famdet )
comp$Genus <- ifelse(is.na(comp$gendet), comp$Genus, comp$gendet )
comp$Sp <- ifelse(is.na(comp$spdet), comp$Sp, comp$spdet )

comp$famdet <- NULL
comp$gendet <- NULL
comp$spdet <- NULL

#Make a new column with the binomials
comp$Binomial <- paste(comp$Genus, comp$Sp)

#Change case of the family column
comp$Family <- str_to_title(comp$Family)

#fix some species names 
comp$Binomial[comp$Binomial == "Himatanthus sucuuba"] <- "Himatanthus articulatus" #(H. sucuuba is a synonym of H. articulatus)
comp$Binomial[comp$Binomial == "Warscewiczia coccinea"] <- "Warszewiczia coccinea"
comp$Binomial[comp$Binomial == "Senna sylvestris"] <- "Senna silvestris"
comp$Binomial[comp$Binomial == "Protium stevensonii"] <- "Tetragastris panamensis" #T. panamensis is a synonym of P. stevensonii but BIEN data does not reflect this. Use T. panamensis for now and change to P. stevensonii at the end.
comp$Binomial[comp$Binomial == "Cupania strobiculata"] <- "Cupania scrobiculata"
comp$Binomial[comp$Binomial == "Ochroma pyramidalis"] <- "Ochroma pyramidale"
comp$Binomial[comp$Binomial == "Trichilia poepiggii"] <- "Trichilia poeppigii"

### Find some summary statistics about the community ###
unique(comp$Binomial) #198 spp

BR_spp <- count(comp,Binomial)
BR_spp <- BR_spp[order(BR_spp$n,decreasing=TRUE),]
#view(BR_spp)

### Diversity metrics ###
### Create a species matrix to use with diversity metrics
plot_mat <- acast(comp, Plot ~ Binomial) 

## Species richness as a function of temperature
#Summarize number of species per plot
plot_spp <- comp %>%
  group_by(Plot) %>%
  summarise(count = n_distinct(Binomial))

plot_char <- cbind(plot_char, plot_spp$count)
plot_char <- plot_char %>%
  dplyr::rename("no_spp" = "plot_spp$count")

# Shannon diversity at each plot
plot_div <- as.data.frame(diversity(plot_mat, index = "shannon", MARGIN = 1))
plot_div <- rownames_to_column(plot_div, var = "Plot")
plot_div$Shannon <- plot_div$`diversity(plot_mat, index = "shannon", MARGIN = 1)`
plot_div$`diversity(plot_mat, index = "shannon", MARGIN = 1)` <- NULL

# Simpson's index
plot_div$Simpson <- diversity(plot_mat, index = "simpson", MARGIN = 1)

# Inverse Simpson's
plot_div$Inv.simp <- diversity(plot_mat, index = "invsimpson", MARGIN = 1)

# Combine div metrics with other plot data
plot_char <- merge(plot_char, plot_div, by = "Plot")

### Make some models to see if there is a 
### relationship with diversity and air temp

# Look at QQ plot of response variables
qqnorm(plot_char$Shannon)
qqline(plot_char$Shannon)

qqnorm(plot_char$Simpson)
qqline(plot_char$Simpson)

qqnorm(plot_char$Inv.simp)
qqline(plot_char$Inv.simp)
# QQ plots look pretty good


### Semivariogram models for sp richness and each index to rule out spatial autocorrelation among plot diversity

#start with species richness
var <- variogram(object = no_spp~1, data = plot_char, locations = ~lat+lon)
plot(var)
##nugget 3.8
##sill 6.5
##range 0.001

fit_var <- fit.variogram(object = var, model = vgm(psill = 6.5, nugget = 3.8, range = 0.001, model = "Gau"))
sp_rich_var <- base::plot(var, model = fit_var, main = "Species richness")
sp_rich_var

#Shannon
var <- variogram(object = Shannon~1, data = plot_char, locations = ~lat+lon)
plot(var)
##nugget 0.09
##sill 0.14
##range 0.001

fit_var <- fit.variogram(object = var, model = vgm(psill = 0.14, nugget = 0.09, range = 0.001, model = "Gau"))
sh_var <- base::plot(var, model = fit_var, main = "Shannon")
sh_var

#Simpson
var <- variogram(object = Simpson~1, data = plot_char, locations = ~lat+lon)
plot(var)
##nugget 0.0045
##sill 0.0065
##range 0.001

fit_var <- fit.variogram(object = var, model = vgm(psill = 0.0065, nugget = 0.0045, range = 0.001, model = "Gau"))
Simp_var <- base::plot(var, model = fit_var, main = "Simpson")
Simp_var

#Inv Simpson
var <- variogram(object = Inv.simp~1, data = plot_char, locations = ~lat+lon)
plot(var)
##nugget 3.9
##sill 6.1
##range 0.001

fit_var <- fit.variogram(object = var, model = vgm(psill = 6.1, nugget = 3.9, range = 0.001, model = "Gau"))
invsi_var <- base::plot(var, model = fit_var, main = "Inverse Simpson")
invsi_var

#Combine the four semivariograms for a supplemental figure
sp_rich_var
sh_var
Simp_var
invsi_var

### Run linear regressions to test how diversity is influenced by air temperature

#First run some models with only nutrients to rule out nutrient effects
summary(lm(no_spp ~ Ca_available + K_available + P_available + Zn + Fe, data = plot_char))
summary(lm(Shannon ~ Ca_available + K_available + P_available + Zn + Fe, data = plot_char))
summary(lm(Simpson ~ Ca_available + K_available + P_available + Zn + Fe, data = plot_char))
summary(lm(Inv.simp ~ Ca_available + K_available + P_available + Zn + Fe, data = plot_char))
#No nutrient is significant. These results are in table S2.

# Run the regressions using MTWM and MAT
lm.sp.rich <- lm(no_spp ~ air.temp, data = plot_char) #MTWM
summary(lm.sp.rich)
tab_model(lm.sp.rich)
lm.sp.rich.mat <- lm(no_spp ~ tms_clim15_MAT, data = plot_char)#MAT
summary(lm.sp.rich.mat)

lm.Shannon <- lm(Shannon ~ air.temp, data = plot_char) #MTWM
summary(lm.Shannon)
tab_model(lm.Shannon)
lm.Shannon.mat <- lm(Shannon ~ tms_clim15_MAT, data = plot_char)#MAT
summary(lm.Shannon.mat)

lm.Simp <- lm(Simpson ~ air.temp, data = plot_char) #MTWM
summary(lm.Simp)
tab_model(lm.Simp)
lm.Simp.mat <- lm(Simpson ~ tms_clim15_MAT, data = plot_char)#MAT
summary(lm.Simp.mat)

lm.Inv.simp <- lm(Inv.simp ~ air.temp, data = plot_char) #MTWM
summary(lm.Inv.simp)
tab_model(lm.Inv.simp)
lm.Inv.simp.mat <- lm(Inv.simp ~ tms_clim15_MAT, data = plot_char) #MAT
summary(lm.Inv.simp.mat)

# plot Shannon index
shannon_plot <- ggplot(data = plot_char, aes(x = air.temp, y = Shannon)) +
  geom_point(size=4, shape=21, fill="gray25", color="black", alpha=.6) +
  geom_smooth(method = "glm", formula = y ~ x, color = "black") +
  scale_x_continuous(name="MTWM (°C)") +
  scale_y_continuous(name="Shannon index (H')") +
  #geom_text(x=40, y=12.5, label="p < 0.001", size = 5) +
  annotate("text", x=40, y=2.5, label= bquote("R"^2~" = 0.17, p < 0.001"), size = 14/.pt) +
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 14), 
        axis.title = element_text(size = 16),
        panel.border = element_rect(fill=NA),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))
shannon_plot

### do a nonlinear regression 
library(minpack.lm)
m <- lm(Shannon ~ poly(air.temp, 2) , data=plot_char)
summary(m) # almost the same. We include this result in the review responses but not the manuscript

shannon_poly <- ggplot(data = plot_char, aes(x = air.temp, y = Shannon)) +
  geom_point(size=4, shape=21, fill="gray25", color="black", alpha=.6) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), color="black") +
  #geom_smooth(method = "glm", formula = m, color = "black") +
  scale_x_continuous(name="MTWM (°C)") +
  scale_y_continuous(name="Shannon index (H')") +
  annotate("text", x=40, y=2.5, label= bquote("R"^2~" = 0.18, p < 0.001"), size = 14/.pt) +
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 14), 
        axis.title = element_text(size = 16),
        panel.border = element_rect(fill=NA),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))
shannon_poly

# Gather spp richness, density, and evenness with facet_wrap
density <- count(comp, Plot)
density <- density %>%
  dplyr::rename(density = n)
plot_char <- left_join(plot_char, density)
plot_char$evenness <- (plot_char$Shannon)/(log(plot_char$no_spp))

plot_div_new <- plot_char[,c(1,26,30,31)]
plot_div_new$air.temp <- plot_char$air.temp
plot_div_new <- gather(plot_div_new, key="Metric", value = "value", c("no_spp", "density", "evenness"))

# Run a couple lms on MTWM with evenness and density 
summary(lm(evenness ~ air.temp, plot_char)) # no effect
summary(lm(density ~ air.temp, plot_char)) # marginally significant effect

#add some text data and dummy data to annotate facets and change the scale on density facet
facet_text <- data.frame(label = c("R2=0.15, p=0.001", "R2=0.04, p=0.1", "R2=0.05, p=0.06"), 
                         Metric = c("no_spp", "evenness", "density"),
                         x = c(40, 40, 39),
                         y = c(13.5, 0.8, 17))

dummy <- data.frame(air.temp = min(plot_div_new$air.temp), value = 18, Metric = "density")

div_plot <- ggplot(data = plot_div_new, aes(x = air.temp, y = value, color = Metric)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x, color="black") +
  scale_x_continuous(name="MTWM (°C)") +
  facet_wrap(~fct_rev(Metric), ncol=1, scales = "free", strip.position = "right", labeller = as_labeller(c(no_spp="Species richness", density="# of individuals", evenness="Evenness"))) +
  scale_color_manual(values=c("orangered", "steelblue", "darkgreen")) +
  ylab(NULL) +
  theme_classic() +
  geom_text(data = facet_text, color="black",  mapping = aes(x = x, y = y, label= label)) +
  geom_blank(data = dummy) +
  theme(axis.text = element_text(color="black", size = 12), 
        axis.title = element_text(size = 14),
        panel.border = element_rect(fill=NA),
        legend.position = "none",
        strip.text = element_text(size=14),
        strip.placement = "outside",
        strip.background = element_blank())
div_plot

#combine plots
lay <- rbind(c(1,1,2),
             c(1,1,2))
div_fig <- grid.arrange(shannon_plot, div_plot, layout_matrix = lay)
#ggsave("diversity_plot.pdf",div_fig)

### Now make some plots of Shannon, richness, evenness, and density using only stems <10cm DBH ####
comp_small <- comp %>%
  filter(DBH<10)

comp_big <- comp %>%
  filter(DBH>10)

# Diversity metrics
# Create a species matrix to use with diversity metrics
plot_mat_small <- acast(comp_small, Plot ~ Binomial) 

## Species richness as a function of temperature
#Summarize number of species per plot
plot_spp_small <- comp_small %>%
  group_by(Plot) %>%
  summarise(count = n_distinct(Binomial))

plot_char_small <- cbind(plot_char, plot_spp_small$count)
# Get rid of some columns 
plot_char_small$no_spp <- NULL
plot_char_small$Shannon <- NULL
plot_char_small$density <- NULL
plot_char_small$Simpson <- NULL
plot_char_small$Inv.simp <- NULL
plot_char_small$evenness <- NULL

plot_char_small <- plot_char_small %>%
  dplyr::rename("no_spp" = "plot_spp_small$count")

# Shannon diversity at each plot
plot_div_small <- as.data.frame(diversity(plot_mat_small, index = "shannon", MARGIN = 1))
plot_div_small <- rownames_to_column(plot_div_small, var = "Plot")
plot_div_small$Shannon <- plot_div_small$`diversity(plot_mat_small, index = "shannon", MARGIN = 1)`
plot_div_small$`diversity(plot_mat_small, index = "shannon", MARGIN = 1)` <- NULL

#simpson and inverse simpson for each plot
plot_div_small$Simpson <- diversity(plot_mat_small, index = "simpson", MARGIN = 1)
plot_div_small$inv.simp <- diversity(plot_mat_small, index = "invsimpson", MARGIN = 1)

# Combine div metrics with other plot data
plot_char_small <- merge(plot_char_small, plot_div_small, by = "Plot")

# Calculate density and evenness
density_small <- count(comp_small, Plot)
density_small <- density_small %>%
  dplyr::rename(density = n)
plot_char_small <- left_join(plot_char_small, density_small)
plot_char_small$evenness <- (plot_char_small$Shannon)/(log(plot_char_small$no_spp))

plot_div_new_small <- plot_char_small[,c(1,26:31)]
plot_div_new_small$air.temp <- plot_char_small$air.temp
plot_div_new_small <- gather(plot_div_new_small, key="Metric", value = "value", c("no_spp", "density", "evenness"))
plot_div_new_small <- plot_div_new_small[-c(67,137,207),] #removes plot 67 with a Shannon value of 0 (two trees both C. argenteum)
plot_char_small <- plot_char_small[-c(67),]

#do the linear regressions for small stems using MTWM and MAT as main effects
summary(lm(Shannon ~ air.temp, data = plot_char_small)) #significant decrease
summary(lm(Shannon ~ tms_clim15_MAT, data = plot_char_small)) #significant decrease

summary(lm(no_spp ~ air.temp, data = plot_char_small)) #significant decrease
summary(lm(no_spp ~ tms_clim15_MAT, data = plot_char_small)) #significant decrease

summary(lm(Simpson ~ air.temp, data = plot_char_small)) #significant decrease
summary(lm(Simpson ~ tms_clim15_MAT, data = plot_char_small)) #significant decrease

summary(lm(inv.simp ~ air.temp, data = plot_char_small)) #significant decrease
summary(lm(inv.simp ~ tms_clim15_MAT, data = plot_char_small)) #significant decrease


summary(lm(density ~ air.temp, data = plot_char_small)) #no effect
summary(lm(evenness ~ air.temp, data = plot_char_small)) #no effect

# plot everything
shannon_plot_small <- ggplot(data = plot_char_small, aes(x = air.temp, y = Shannon)) +
  geom_point(size=4, shape=21, fill="gray25", color="black", alpha=.6) +
  geom_smooth(method = "glm", formula = y ~ x, color = "black") +
  scale_x_continuous(name="MTWM (°C)") +
  scale_y_continuous(name="Shannon index (H')") +
  annotate("text", x=40, y=2.3, label= bquote("R"^2~" = 0.10, p < 0.01"), size = 14/.pt) +
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 14), 
        axis.title = element_text(size = 16),
        panel.border = element_rect(fill=NA),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))
shannon_plot_small


#add some text data and dummy data to annotate facets and change the scale on density facet
facet_text_small <- data.frame(label = c("R2=0.08, p=0.02", "R2=0.02, p=0.27", "R2=0.03, p=0.18"), 
                         Metric = c("no_spp", "evenness", "density"),
                         x = c(39.5, 40, 39),
                         y = c(11.5, 0.8, 16.5))

dummy_small <- data.frame(air.temp = min(plot_div_new$air.temp), value = 18, Metric = "density")

div_plot_small <- ggplot(data = plot_div_new_small, aes(x = air.temp, y = value, color = Metric)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x, color="black") +
  scale_x_continuous(name="MTWM (°C)") +
  facet_wrap(~fct_rev(Metric), ncol=1, scales = "free", strip.position = "right", labeller = as_labeller(c(no_spp="Species richness", density="# of individuals", evenness="Evenness"))) +
  scale_color_manual(values=c("orangered", "steelblue", "darkgreen")) +
  ylab(NULL) +
  theme_classic() +
  geom_text(data = facet_text_small, color="black",  mapping = aes(x = x, y = y, label= label)) +
  geom_blank(data = dummy_small) +
  theme(axis.text = element_text(color="black", size = 12), 
        axis.title = element_text(size = 14),
        panel.border = element_rect(fill=NA),
        legend.position = "none",
        strip.text = element_text(size=14),
        strip.placement = "outside",
        strip.background = element_blank())
div_plot_small

#combine plots
lay <- rbind(c(1,1,2),
             c(1,1,2))
div_fig_small <- grid.arrange(shannon_plot_small, div_plot_small, layout_matrix = lay)
#ggsave("diversity_plot.pdf",div_fig)


#####

# Make some rarefaction curves

BA=comp[, c("DBH", "DBH2", "DBH3", "DBH4", "DBH5")] #calculate basal area per stem and then total basal area per tree
BA[is.na(BA)]=0
BA=3.14*(BA/2)^2
comp$BA=apply(BA,1, sum)
plotBA <- aggregate(comp$BA, by=list(Plot=comp$Plot), FUN="sum")
plot_char$BA <- plotBA[,c(2)]
rm(plotBA)

plot.o <- order(plot_char$air.temp) #order of plots by temperature

trees <- comp # choose which dataset to use (small trees are in the df comp_small)
m <- match(trees$Plot, plot_char$Plot[plot.o[1:25]]) #find trees in either coldest or hottest 25 plots
w.cold=which(!is.na(m))
m=match(trees$Plot, plot_char$Plot[plot.o[46:70]])
w.hot=which(!is.na(m))

n.hot=length(w.hot) #numbr of trees in "super plots"
n.cold=length(w.cold)
sp.hot=length(unique(trees$Binomial[w.hot])) #species richness in "super plots"
sp.cold=length(unique(trees$Binomial[w.cold]))
t=trees[w.hot,]
x=table(t$Plot,t$Binomial)
a=apply(x, 2, sum)
sp.hot10=rarefy(a, sample=1:sum(a), se=T) #rarefaction for hottest 25
v.h=vegdist(x, method="jaccard")
v.hot=mean(v.h) #mean jaccard dissimilarity between hottest 25 plots
t=trees[w.cold,]
x=table(t$Plot,t$Binomial)
a=apply(x, 2, sum)
sp.cold10=rarefy(apply(x, 2, sum), sample=1:sum(a), se=T) #rarefaction for coldest 25
v.c=vegdist(x, method="jaccard")
v.cold=mean(v.c) #mean jaccard dissimilarity between coldest 25 plots


#graph results
boxplot(plot_char$no_spp[plot.o[1:25]],plot_char$no_spp[plot.o[46:70]], col=c("blue", "red"), ylab="No. of Species per plot", xlab="cold plots vs. hot plots") #boxplot of species richness
t.test(plot_char$no_spp[plot.o[1:25]],plot_char$no_spp[plot.o[46:70]])

boxplot(plot_char$density[plot.o[1:25]],plot_char$density[plot.o[46:70]], col=c("blue", "red"), ylab="No. of Trees per plot", xlab="cold plots vs. hot plots") #boxplot of stem counts
t.test(plot_char$density[plot.o[1:25]],plot_char$density[plot.o[46:70]])

plot(sp.cold10[1,], type='l', lty=1, col="blue", lwd=3, xlab = "Sample Size (No. of trees)", ylab = "Rarefied No. of Species") #rarefaction plots
points(sp.cold10[1,]+sp.cold10[2,]*2, type='l', lty=3, col="blue")
points(sp.cold10[1,]-sp.cold10[2,]*2, type='l', lty=3, col="blue")

points(sp.hot10[1,], type='l', lty=1, col="red", lwd=3)
points(sp.hot10[1,]+sp.hot10[2,]*2, type='l', lty=3, col="red")
points(sp.hot10[1,]-sp.hot10[2,]*2, type='l', lty=3, col="red")
### Overall, there is lower diversity in hot plots than cool plots. ###


### Analyze turnover/nestedness ####
# Convert matrix to presence/absence
plot_mat_pa <- ifelse(plot_mat>0,1,0)

#calculate beta diversity, nestedness, and Sorensen dissimilarity
beta.div <- beta.multi(plot_mat_pa, index.family="sorensen")
beta.div
#most of the beta diversity is coming from turnover, not nestedness.

#calculate and plot each pairwise combo of bet-SOR and delta-MTWM
beta.div <- beta.pair(plot_mat_pa, index.family="sorensen")
beta.sor <- beta.div[["beta.sor"]] #keeps only b-SOR
delta.mtwm <- dist(plot_char$air.temp) #distance matrix of MTWM differences

#combine both distance matrices into a dataframe
beta.dif <- matrixConvert(beta.sor, colname=c("Var1","Var2","beta")) 
mtwm.dif <- matrixConvert(delta.mtwm, colname=c("Var1","Var2","value"))
beta.mtwm.dif <- cbind(beta.dif, mtwm.dif)
beta.mtwm.dif <- beta.mtwm.dif[,-c(4,5)]
beta.mtwm.dif <- beta.mtwm.dif[order(beta.mtwm.dif$Var1, beta.mtwm.dif$Var2),]

#calculate mean temp between each plot pair to color code the next figure
mean.mtwm.pairs <- plot_char %>%
    summarise(Plot = combn(Plot, 2, paste0, collapse = '-'), 
    mean.temp = combn(air.temp, 2, mean))

#combine dfs
beta.mtwm.dif$mean.temp <- mean.mtwm.pairs$mean.temp

ggplot(data = beta.mtwm.dif, aes(x = value, y = beta, fill=mean.temp)) +
  geom_jitter(data=subset(beta.mtwm.dif, beta=1), size=2, shape=21, color="black", alpha=.8, width = 0.01, height= 0.01) +
  geom_smooth(method = "lm", formula = y ~ x, color="black") +
  scale_fill_continuous(low = "yellow", high = "red") +
  labs(fill = "Mean MTWM (°C)") +
  scale_x_continuous(name="\u0394 MTWM (°C)") +
  scale_y_continuous(name="Beta-SOR") +
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 14), 
        axis.title = element_text(size = 16),
        legend.position = c(0.8,.25), 
        legend.background = element_rect(linetype="solid", color = "black", fill = "transparent"),
        panel.border = element_rect(fill=NA),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))  

## make two more panels with beta-SIM and beta-SNE against delta MTWM
beta.sim <- beta.div[["beta.sim"]]
beta.sne <- beta.div[["beta.sne"]]

sim.dif <- matrixConvert(beta.sim, colname=c("Var1","Var2","beta")) 
sim.dif <- cbind(sim.dif, mtwm.dif)
sim.dif$metric <- "beta.sim"

sne.dif <- matrixConvert(beta.sne, colname=c("Var1","Var2","beta")) 
sne.dif <- cbind(sne.dif, mtwm.dif)
sne.dif$metric <- "beta.sne"

sim.sne.dif <- rbind(sim.dif, sne.dif)
sim.sne.dif <- sim.sne.dif[,-c(4,5)]

ggplot(data = sim.sne.dif, aes(x = value, y = beta, color = metric)) +
  # geom_point(data=subset(beta.mtwm.dif, beta<1), size=2, shape=21, fill="gray25", color="black", alpha=.6) +
  geom_jitter(size=1, shape=21, fill="gray25", color="black", alpha=.6, width = 0.01, height= 0.01) +
  # geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "black") +
  scale_x_continuous(name="\u0394 MTWM (°C)") +
  scale_y_continuous(name="Beta") +
  facet_wrap(~ metric, ncol = 1) +
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 14), 
        axis.title = element_text(size = 16),
        panel.border = element_rect(fill=NA),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))  



### A couple random things to do before compositional analyses...
#add column in plot_char to summarize how many big trees (>10cm DBH) there are. 
big.tree <- comp_big %>%
  group_by(Plot) %>%
  summarise(count = n_distinct(Binomial))
  
plot_char <- left_join(plot_char, big.tree, by = "Plot")
plot_char <- plot_char %>%
  rename(big.tree = count)
plot_char$big.tree <- ifelse(is.na(plot_char$big.tree), 0 , plot_char$big.tree)
#write.csv(plot_char, file = "TableS5.csv")

# make a histogram of tree diameters
ggplot(data=comp, aes(x=DBH)) +
  geom_histogram(color="black", fill="white", binwidth=2) +
  scale_x_continuous(n.breaks = 10, name="Diameter at breast height (cm)") +
  scale_y_continuous(name="Count") +
  geom_vline(xintercept = mean(comp$DBH), linetype="dashed", color="blue2", size=1) +
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 14), 
        axis.title = element_text(size = 16),
        panel.border = element_rect(fill=NA),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))  


### Composition with NMDS ###
#Make a function to produce a scree plot for different dimensions of the NMDS
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS scree plot")
  abline(h=0.1, col = "gray", lty=2)
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

#run the function to determine the best number of dimensions
#NMDS.scree(plot_mat) #looks like 3 dimensions is best

#Do the NMDS and plot
set.seed(12)
NMDS <- metaMDS(plot_mat, distance = "bray", k=3) 
stressplot(NMDS) 

plot(NMDS)
orditorp(NMDS,display="species",col="red",air=0.01)

# Make data frame to use with ggplot
NMDSscores <- as.data.frame(scores(NMDS)$sites)
NMDSscores$air.temp <- plot_char$air.temp
NMDSscores <-rownames_to_column(NMDSscores)
NMDSscores <- NMDSscores %>%
  dplyr::rename(Plot = rowname)

ggplot(NMDSscores, aes(x = NMDS1, y = NMDS2, label = Plot)) +
  geom_point(size = 4, aes(color = air.temp)) +
  #geom_text() +
  theme_classic() +
  scale_color_continuous(low = "yellow", high = "red") +
  labs(color = "Air temperature (C)")

en <- envfit(NMDS, plot_char[, c(3:4,22)], permutations = 999)
head(en)
env.scores <- as.data.frame(scores(en, display = "vectors"))
env.scores <- cbind(env.scores, env.variables = rownames(env.scores))
env.scores[3,3] <- "Air temperature"

# radial shift function
rshift = function(r, theta, a=0.03, b=0.07) {
  r + a + b*abs(cos(theta))
}

# Calculate shift
env.scores = env.scores %>% 
  mutate(r = sqrt(NMDS1^2 + NMDS2^2),
         theta = atan2(NMDS2,NMDS1),
         rnew = rshift(r, theta),
         xnew = rnew*cos(theta),
         ynew = rnew*sin(theta))

env.scores[3,8] <- -0.015 # slightly changes the position of the Air temp label in the following plot


ggplot(NMDSscores, aes(x = NMDS1, y = NMDS2)) +
  geom_hline(yintercept=0, linetype="dashed", color = "gray25") +
  geom_vline(xintercept=0, linetype="dashed", color = "gray25") +
  geom_point(shape = 16, size = 5,  aes(color = air.temp)) +
  geom_segment(data = env.scores, aes(x = 0, xend = 1*NMDS1, y = 0, yend = 1*NMDS2), lineend = "round", arrow = arrow(length = unit(0.25, "cm")), color = "black", size = 1) +
  geom_text(data = env.scores, aes(x = xnew, y = ynew, label=env.variables), size = 4.5) +
  scale_color_continuous(low = "yellow", high = "red") +
  labs(color = "MTWM (°C)") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 14), 
        axis.title = element_text(size = 16),
        panel.border = element_rect(fill=NA), 
        legend.position = c(0.1,.825), 
        legend.background = element_rect(linetype="solid", color = "black", fill = "transparent"),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),) 

#ggsave("NMDS.pdf")  

#3d scatterplot
marker <- list(color = ~air.temp, colorscale = list(c(0,1), c("yellow","red")), 
               showscale = TRUE)
plot_ly(NMDSscores, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, marker = marker)
         
summary(lm(NMDS1~air.temp, data = NMDSscores))
plot(NMDSscores$air.temp,NMDSscores$NMDS1)

#### Do the NMDS again with only plots with soil nutrients. ####
# Add arrows with env variables. 
# For soil, include only Ca, K, P, Zn and Fe because these are the most 
# important properties that shape composition in the neotropics (John et al. 2007, Condit et al. 2013)

#another species matrix only with plots with soil nutrients
plot_mat_soil <- acast(comp[comp$Plot %in% plot_char_soil$Plot,], Plot ~ Binomial, value.var="Plot") 
plot_mat_soil <- plot_mat_soil[-c(22),]

NMDS <- metaMDS(plot_mat_soil, distance = "bray", k=2) 
NMDSscores <- as.data.frame(scores(NMDS)$sites)
NMDSscores$air.temp <- plot_char_soil[-c(22),]$air.temp
NMDSscores <-rownames_to_column(NMDSscores)
NMDSscores <- NMDSscores %>%
  dplyr::rename(Plot = rowname)

ggplot(NMDSscores, aes(x = NMDS1, y = NMDS2, label = Plot)) +
  geom_point(size = 4, aes(color = air.temp)) +
  #geom_text() +
  theme_classic() +
  scale_color_continuous(low = "yellow", high = "red") +
  labs(color = "Air temperature (C)")

en <- envfit(NMDS, plot_char_soil[-c(22), c(3:4,11,13,17:18,21:22)], permutations = 999)
head(en)
env.scores <- as.data.frame(scores(en, display = "vectors"))
env.scores <- cbind(env.scores, env.variables = rownames(env.scores))
env.scores[3,3] <- "Ca"
env.scores[5,3] <- "K"
env.scores[7,3] <- "P"
env.scores[8,3] <- "Air temperature"

# radial shift function
rshift = function(r, theta, a=0.03, b=0.07) {
  r + a + b*abs(cos(theta))
}

# Calculate shift
env.scores = env.scores %>% 
  mutate(r = sqrt(NMDS1^2 + NMDS2^2),
         theta = atan2(NMDS2,NMDS1),
         rnew = rshift(r, theta),
         xnew = rnew*cos(theta),
         ynew = rnew*sin(theta))

ggplot(NMDSscores, aes(x = NMDS1, y = NMDS2)) +
  geom_hline(yintercept=0, linetype="dashed", color = "gray25") +
  geom_vline(xintercept=0, linetype="dashed", color = "gray25") +
  geom_point(shape = 16, size = 5,  aes(color = air.temp)) +
  geom_segment(data = env.scores, aes(x = 0, xend = 1*NMDS1, y = 0, yend = 1*NMDS2), lineend = "round", arrow = arrow(length = unit(0.25, "cm")), color = "black", size = 1) +
  geom_text(data = env.scores, aes(x = xnew, y = ynew, label=env.variables), size = 4.5) +
  scale_color_continuous(low = "yellow", high = "red") +
  labs(color = "MTWM (°C)") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 14), 
        axis.title = element_text(size = 16),
        panel.border = element_rect(fill=NA), 
        legend.position = c(0.1,.825), 
        legend.background = element_rect(linetype="solid", color = "black", fill = "transparent"),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),) 


### Do a partial Mantel test to determine significance of environmental variables
#First make rownames of plot characteristics df
plot_mantel <- plot_char
rownames(plot_mantel) <- plot_mantel$Plot 
plot_mantel$Plot <- NULL

##extract community, soil (and temp), and geo distance to use in partial Mantel
#community dissimilarity
comm_dist <- vegdist(plot_mat, method="bray") 

#make air temp and geo matrices
temp_mat <- as.matrix(plot_mantel[,c(21)]) #21 is mtwm
temp_dist <- vegdist(temp_mat, method="bray")

dist_mat <- as.matrix(plot_mantel[,c(7,8)]) #these are slope, lat and lon
dist_dist <- vegdist(dist_mat, method="euclidean")

topo_mat <- as.matrix(plot_mantel[,c(2)]) #2 is slope
topo_dist <- vegdist(topo_mat, method="euclidean")

geo_mat <- as.matrix(plot_mantel[,c(2,7,8)]) #these are slope, lat and lon
geo_dist <- vegdist(geo_mat, method="euclidean")

### Mantel tests
#Mantel test to test temp effect on community composition
mantel(beta.sor, temp_dist, method="spearman", permutations=999)
#p = 0.001
#r = 0.32

#Mantel test to test geo distance on community composition
mantel(beta.sor, dist_dist, method="spearman", permutations=999)
#p = 0.001
#r = 0.37

#Mantel test to test topography on community composition
mantel(beta.sor, topo_dist, method="spearman", permutations=999)
#p = 0.005
#r = 0.09

#Partial Mantel test (controlling for geo distance and topography)
mantel.partial(beta.sor, temp_dist, geo_dist, method="spearman", permutations=999)
#p = 0.001 
#r = 0.31 

#Partial Mantel test to evaluate geo distance (controlling for temp)
mantel.partial(beta.sor, dist_dist, temp_dist, method="spearman", permutations=999)

#Do some Mantel tests again, but this time include soil nutrients for the 25 plots with soil data to see if air temp holds up. 
geo_mat2 <- as.matrix(plot_mantel[c(1,4,6,9,13:25,32:35,37,40,41,46),c(7,8)]) #these are lat, lon
geo_dist2 <- vegdist(geo_mat2, method="euclidean")

soil_mat2 <- as.matrix(plot_mantel[,c(2,10,12,15,17,20)]) #relevant soil nutrients
soil_mat2 <- na.omit(soil_mat2)
soil_dist2 <- vegdist(soil_mat2, method="euclidean")

beta.div2 <- beta.pair(plot_mat_pa[c(1,4,6,9,13:25,32:35,37,40,41,46),], index.family="sorensen")
beta.sor2 <- beta.div2[["beta.sor"]] #keeps only b-SOR

temp_mat2 <- as.matrix(plot_mantel[c(1,4,6,9,13:25,32:35,37,40,41,46),c(21)]) #21 is mtwm
temp_dist2 <- vegdist(temp_mat2, method="bray")

#simple
mantel(beta.sor2, temp_dist2, method="spearman", permutations=999)
mantel(beta.sor2, geo_dist2, method="spearman", permutations=999)
mantel(beta.sor2, soil_dist2, method="spearman", permutations=999)

#matrix with slope, lat, lon, and soil nutrients for the partial Mantel
geo_mat3 <- as.matrix(plot_mantel[c(1,4,6,9,13:25,32:35,37,40,41,46),c(2,7,8,10,12,15,17,20)])
geo_dist3 <- vegdist(geo_mat3, method="euclidean")

#partial
mantel.partial(beta.sor2, temp_dist2, geo_dist3, method="pearson", permutations=999)
#MTWM is still significant
rm(temp_mat2,temp_dist2,beta.div2,beta.sor2,geo_mat2, geo_dist2, geo_mat3, geo_dist3)


#### Analyze thermal maximum for each species and calculate community temperature scores.

# load cleaned occurrences
occ_clean <- read.csv("occ_clean_final.csv")
occ_clean <- occ_clean[order(occ_clean$species),]

## Calculate a thermal maximum for each species
splist <- unique(occ_clean$species)
MATmin = MATmax = MATmean = MATmedian = MATquart25 = MATquart75 = Tmax = Tmax_min = Tmax_median = Tmax_quart25 = Tmax_quart75 = Tmax_95 = Tmin = TNB = rep(NA, length(splist))

for(i in 1:length(splist)){
  sp <- splist[[i]]
  sp.sub <- subset(occ_clean, species == sp)
  
  # Temperature 
  maxq <- quantile(sp.sub$bio_5, .975, na.rm=T)
  minq <- quantile(sp.sub$bio_6, .025, na.rm=T)
  
  maxq2 <- quantile(sp.sub$bio_1, 0.975, na.rm=T)
  minq2 <- quantile(sp.sub$bio_1, 0.025, na.rm=T)
  
  minq3 <- quantile(sp.sub$bio_5, 0.025, na.rm=T)
  
  Tmax[i] <- mean(subset(sp.sub, bio_5 >= maxq)$bio_5, na.rm=T)
  Tmin[i] <- mean(subset(sp.sub, bio_6 <= minq)$bio_6, na.rm=T)
  MATmin[i] <- mean(subset(sp.sub, bio_1 <= minq2)$bio_1, na.rm=T)
  MATmax[i] <- mean(subset(sp.sub, bio_1 >= maxq2)$bio_1, na.rm=T)
  
  Tmax_min[i] <- mean(subset(sp.sub, bio_5 <= minq3)$bio_5, na.rm=T)
  Tmax_median[i] <- median(sp.sub$bio_5, na.rm=T)
  Tmax_quart25[i] <- quantile(sp.sub$bio_5, .25, na.rm=T)
  Tmax_quart75[i] <- quantile(sp.sub$bio_5, .75, na.rm=T)
  Tmax_95[i] <- quantile(sp.sub$bio_5, .95, na.rm=T)
  
  MATmean[i] <- mean(sp.sub$bio_1, na.rm=T)
  MATmedian[i] <- median(sp.sub$bio_1, na.rm=T)
  MATquart25[i] <- quantile(sp.sub$bio_1, 0.25, na.rm=T)
  MATquart75[i] <- quantile(sp.sub$bio_1, 0.75, na.rm=T)
  
  TNB[i] <- Tmax[i] - Tmin[i]
}

tnb_df <- data.frame(splist, MATmin, MATmax , MATmean , MATmedian , MATquart25 , MATquart75 , Tmax , Tmax_min , Tmax_median , Tmax_quart25 , Tmax_quart75 , Tmax_95 , Tmin , TNB)
colnames(tnb_df)[colnames(tnb_df) == "splist"] <- "species"

# Make supp table 
spp_plot_occs <- comp %>%
  dplyr::group_by(Family, Binomial) %>%
  dplyr::summarize(count = n_distinct(Plot))
  
m <- match(BR_spp$Binomial, tnb_df$species)

supp_tab_3 <- merge(BR_spp, spp_plot_occs, by = "Binomial")
supp_tab_3$TM <- tnb_df$Tmax_95[match(supp_tab_3$Binomial, tnb_df$species)]

#write.csv(supp_tab_3, "Table S3b.csv")

#calculate community temperature scores for each plot and compare with plot temp
m <- match(comp$Binomial, tnb_df$species)

comp$Tmax <- tnb_df$Tmax_median[m]
comp$Tmax <- ifelse(is.na(comp$Tmax), mean(tnb_df$Tmax_median), comp$Tmax)

comp$Tmax_95 <- tnb_df$Tmax_95[m]
comp$Tmax_95 <- ifelse(is.na(comp$Tmax_95), mean(tnb_df$Tmax_95), comp$Tmax_95)

comp$Tult <- tnb_df$Tmax[m]
comp$Tult <- ifelse(is.na(comp$Tult), mean(tnb_df$Tmax), comp$Tult)

comp$MAT <- tnb_df$MATmedian[m]
comp$MAT <- ifelse(is.na(comp$MAT), mean(tnb_df$MATmedian), comp$MAT)

rm(m)

#calculate basal area of each tree if weighting averages by basal area
comp$ba <- (pi * (comp$DBH/2)^2)

# summarize the previous thermal metrics for each plot
plots <- plot_char$Plot
Tmax <- rep(NA, length(plots))
Tmax_95 <- rep(NA, length(plots))
Tult <- rep(NA, length(plots))
MAT <- rep(NA, length(plots))
  
for (i in 1:length(plots)) {
  
  subset <- comp %>%
    filter(Plot == plots[i])
  
  Tmax[[i]] <- mean(subset$Tmax)
  Tmax_95[[i]] <- mean(subset$Tmax_95) #this will be our CTS for the paper
  Tult[[i]] <- mean(subset$Tult)
  MAT[[i]] <- mean(subset$MAT)
}

# put the thermal maxima lists into the plot dataframe
plot_char$CTS <- Tmax_95 #CTS for the paper 
plot_char$Tult <- Tult
plot_char$Tmax_median <- Tmax
plot_char$MAT <- MAT

# run a linear model to check if air temp is driving community temperature scores (CTS)
plot(plot_char$air.temp, plot_char$CTS)
m <- lm(CTS ~ air.temp, data=plot_char)
summary(m)
tab_model(m)

plot(plot_char$air.temp, plot_char$Tmax_median)
m2 <- lm(Tmax_median ~ air.temp, data=plot_char)
summary(m2)

plot(plot_char$air.temp, plot_char$Tult)
m3 <- lm(Tult ~ air.temp, data=plot_char)
summary(m3)

plot(plot_char$tms_clim15_MAT, plot_char$MAT)
m4 <- lm(MAT ~ tms_clim15_MAT, data=plot_char)
summary(m4)

#plot the results
cts_plot <- ggplot(plot_char, aes(x = air.temp, y = CTS)) +
  stat_smooth(method=lm, fullrange = TRUE, color="black") +
  geom_point(size=4, shape=21, fill="gray25", color="black", alpha=.6) +
  labs(x = "MTWM (°C)", y = "Community temperature score (°C)") +
  annotate("text", x=39.5, y=33.1, label= bquote("R"^2~" = 0.19, p < 0.001"), size = 14/.pt) +
  scale_x_continuous(limits = c(30,45)) +
  scale_y_continuous(limits = c(32,36)) +
  coord_cartesian(xlim=c(34,43.5),ylim=c(32.8,35.2)) +
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 14), 
        axis.title = element_text(size = 16),
        panel.border = element_rect(fill=NA),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))
cts_plot

#ggsave("thermophilization.pdf")

#make a plot with mean thermal tolerance and mean annual temp in the plots
ggplot(plot_char, aes(x = tms_clim15_MAT, y = MAT)) +
  stat_smooth(method=lm, fullrange = TRUE, color="black") +
  geom_point(size=4, shape=21, fill="gray25", color="black", alpha=.6) +
  labs(x = "MAT (°C)", y = "Community mean thermal optimum (°C)") +
  annotate("text", x=24.2, y=24.5, label= bquote("R"^2~" = 0.03, p = 0.16"), size = 14/.pt) +
  scale_x_continuous(limits = c(23.5,27)) +
  scale_y_continuous(limits = c(24.2,26.5)) +
  #coord_cartesian(xlim=c(34,43.5),ylim=c(32.8,35.2)) +
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 14), 
        axis.title = element_text(size = 16),
        panel.border = element_rect(fill=NA),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))

### Now calculate species thermal optima based on occurrences within plots at the Boiling River (mean temp)
# and compare to thermal optima based on regional occurrence records 

# first assign the plot temp to each individual in the comp dataframe
m <- match(comp$Plot, plot_char$Plot)
comp$plot.temp <- plot_char$air.temp[m]

# calculate mean temp for each species among BR plots
sp.br.topt <- rep(NA, length(splist))
sp.br.tmax <- rep(NA, length(splist))

for(i in 1:length(splist)){
  sp <- splist[[i]]
  sp.sub <- subset(comp, Binomial == sp)
  
  # Temperature 
  sp.br.topt[[i]] <- mean(sp.sub$plot.temp)
  sp.br.tmax[[i]] <- quantile(sp.sub$plot.temp, .95, na.rm=T)
}

#combine sp.br.topt with tnb_df
tnb_df$br.topt <- sp.br.topt
tnb_df$br.tmax <- sp.br.tmax

#plot and run a regression
plot(tnb_df$Tmax_95, tnb_df$br.tmax)
summary(lm(br.tmax ~ Tmax_95, data = tnb_df))

plot(tnb_df$Tmax_95, tnb_df$br.topt)
summary(lm(br.topt ~ Tmax_95, data = tnb_df))

ggplot(data=tnb_df, aes(x=Tmax_95, y=br.topt)) +
  stat_smooth(method=lm, fullrange = TRUE, color="black") +
  geom_point(size=4, shape=21, fill="gray25", color="black", alpha=.6) +
  labs(x = "Thermal maximum (°C)", y = "Thermal optimum at the Boiling River (°C)") +
  annotate("text", x=36, y=37, label= bquote("R"^2~" = 0.03, p < 0.05"), size = 12/.pt) +
  theme_classic()
  

