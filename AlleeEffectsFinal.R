####Constructing SPDs for Lima et al. 2023 ``Positive feedbacks in deep-time transitions of human populations"

library(tidyverse)
library(rcarbon)
library(ggplot2)

RawData <- read_csv("scrubbed_v5_0_nearestarchaeoglobe_Deleted.csv")

### LOOK AT THE DATA
# summarize the raw data according to archaeoglobe region, density, etc.
summary <- RawData %>%
  dplyr::filter(Age < 15000 ) %>% #remove any raw ages above 15,000 BP to avoid Pleistocene
  group_by(World_ID, World_RG, Archaeo_ID, Archaeo_RG, Land_Area) %>% # group by archaeoglobe region
  dplyr::summarize(n =n(),
                   DatesPerKM2 = n / Land_Area) %>% 
  dplyr::distinct() %>% 
  arrange(- DatesPerKM2) 


#write.csv(summary, "scrubbed_v5_0_nearestarchaeoglobe_summary.csv")

RawData %>%
  dplyr::filter(Age < 15000 ) %>% #remove any raw ages above 15,000 BP 
  group_by(World_ID, World_RG) %>% # group by archaeoglobe region
  dplyr::summarize(n =n())


#box plots by world regions of radiocarbon ages per land area
ggplot(data = summary,
       aes(y =log(DatesPerKM2), 
           x = World_RG))+
  geom_boxplot()+
  #scale_y_log10()+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),)+
  labs(x = "Region", y="LN Radiocarbon age density per sq. km")

#Box plot by world regions of number of radiocarbon ages
ggplot(data = summary,
       aes(y = log(n), 
           x = World_RG))+
  geom_boxplot()+
  #scale_y_log10()+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),)+
  labs(x = "Region", y="LN Number of radiocarbon ages")

##Plot raw age histograms by archaeoglobe World region
p.list1 = lapply(sort(unique(RawData$World_RG)), function(i) {
  RawHist<- ggplot(RawData[RawData$World_RG==i,], aes(x=(Age))) +
    geom_histogram(show.legend=FALSE) +
    theme_bw() +
    theme(axis.text = element_text(size = rel(1.75), colour = "black"), axis.title=element_text(size=22))+
    labs(x = "Years uncal BP", y="Frequency")+
    scale_x_reverse()+
    #geom_smooth(method="lm", color="magenta")+
    facet_wrap(~factor(World_RG))+
    theme(strip.text.x = element_text(size = 18, colour = "black"))
  #ggsave(paste0("RawHist/World/RawHist", i, ".pdf"), device = "pdf")
})
p.list1

####Create SPDs by World Regions=======================================================

### CALIBRATE THE DATA
# nest the dataset for calibration
by_region2 <- RawData %>%
  dplyr::filter(Age < 15000 ) %>% #remove any raw ages above 15,000 BP to avoid Pleistocene
  left_join(read_csv("calibCurves.csv"), by = "World_ID") %>% 
  group_by(World_ID, World_RG) %>% # group by archaeoglobe World region
  nest

###create SPD by world regions. Break into two different batches for computational ease (e.g.,1:5 and 6:17)
test <- by_region2[1:4,]

makeSPD <- function(data){
  cptcal <- calibrate(x = data$Age,  
                      errors = data$Error,
                      calCurves = data$calib, 
                      verbose = FALSE) 
  cptspd <- spd(x=cptcal, 
                timeRange=c(15000,0), 
                spdnormalised = FALSE,
                runm = 200,
                verbose = FALSE) 
  return(cptspd$grid)
}

###Test SPD

test$SPD <- map(test$data, makeSPD)

##Get test results as dataframe rather than a list

SPDResults2 <- test %>% 
  tidyr::unnest(cols = SPD) %>% 
  dplyr::select(-data)

#Save SPDS as csv fil if so desired
#write.csv(SPDResults2, "SPDs/SPDArchWorld1-5.csv")

###Visualize SPDs for arcgaeoglobe world regions prior to trimming
keep<-read.csv(file="SPDs/SPDArchWorld1-5.csv", header=T)

p.list = lapply(sort(unique(keep$World_RG)), function(i) {
  my_plot<- ggplot(keep[keep$World_RG==i,], aes((calBP), (PrDens))) +
    geom_line(show.legend=FALSE) +
    theme_bw() +
    theme(axis.text = element_text(size = rel(1.75), colour = "black"), axis.title=element_text(size=22))+
    labs(x = "Years cal BP", y="Summed Probability")+
    scale_x_reverse()+
    #geom_smooth(method="lm", color="magenta")+
    facet_wrap(~factor(World_RG))+
    theme(strip.text.x = element_text(size = 18, colour = "black"))
 # ggsave(paste0("SPDs/SPDWorld/my_plot", i, ".pdf"), device = "pdf")
})
p.list

#####Sums of SPDs into 30 year intervals

library(zoo)
library(tidyr)
#Load cleaned file
keep<-read.csv(file="SPDs/WorldSPDsTrimmed.csv", header=T)

keep2<-cbind(keep$World_RG, keep$PrDens, keep$calBP )
keep3<-as.data.frame((keep2))

data_wide <- spread(keep3, V1, V2)
data_wide

#write.csv(data_wide, "SPDs/worldTrimmed2.csv")

keep<-read.csv(file="SPDs/WorldTrimmed2.csv", header=T)

# sum and save new csvs.
sum30 <- rollapply(keep,30,(sum),by=30,by.column=TRUE,align='right')
write.table(sum30, file = "SPDs/sums/worldsum30.csv", sep = ",")

