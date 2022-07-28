##############Data analysis: Northern Atlantic elasmobranchs####
##############Author: Fernando Tuya; January-July 2022##############
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(ggtext)
library(fishualize)
library(ggstatsplot)
library(pscl)
library(MASS)

#Read data
data=read.table("./data/data.txt", sep="\t", header=TRUE)

#Obtain number of sightings per ecoregion
data %>%
  group_by(Ecoregion)%>%
  summarise(Total = sum(Abun))

#Obtain number of replicates per ecoregion 
data %>%
  group_by(Ecoregion)%>%
  summarise(Total = n())%>%
  arrange(desc(Total))%>%
  view()

#Obtain number of replicates per site 
data %>%
  group_by(Site)%>%
  summarise(Total = n())%>%
  arrange(desc(Total))%>%
  view()

#Obtain number of replicates per study
data %>%
  group_by(Source)%>%
  summarise(Total = n())%>%
  arrange(desc(Total))%>%
  View()

#Obtain total number of counts per method
data %>%
  group_by(Method)%>%
  count()

#Obtain total area of counts per ecoregion
data%>%
  group_by(Ecoregion)%>%
  summarise(area= sum(Dimensions))%>%
  arrange(desc(area))%>%
  View()

#Obtain number of replicates per dimension
data %>%
  group_by(Dimensions)%>%
  count()

#Plot total number of counts per ecoregion
data %>% group_by(Ecoregion)%>%
  summarise(Total = n())%>%
  ggbarplot(x = "Ecoregion", y = "Total", 
            fill = "Ecoregion",               
            color = "white",            
            palette = "jco",            
            sort.val = "desc",          
            sort.by.groups = FALSE,     
            x.text.angle = 90) + coord_flip() +
  xlab(" ") + ylab(" Total number of fish counts") +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") 
ggsave("Total_counts.tiff")

#Plot total area per ecoregion
data %>% group_by(Ecoregion)%>%
  summarise(Area = sum(Dimensions)/10000)%>%
  ggbarplot(x = "Ecoregion", y = "Area", 
            fill = "Ecoregion",               
            color = "white",            
            palette = "jco",            
            sort.val = "desc",          
            sort.by.groups = FALSE,     
            x.text.angle = 90) + coord_flip() +
  xlab(" ") + ylab("Total Area") +
  theme_bw(base_size = 20) +
  theme(legend.position = "none")
ggsave("Total_areas.tiff")

#Plot total abundances of elasmobranchs per ecoregion
data %>% group_by(Ecoregion)%>%
  summarise(Abund = 1000*(sum(Abun)/sum(Dimensions)))%>%
  ggbarplot(x = "Ecoregion", y = "Abund", 
            fill = "Ecoregion",               
            color = "white",            
            palette = "jco",           
            sort.val = "desc",          
            sort.by.groups = FALSE,     
            x.text.angle = 90) + coord_flip() +
  xlab(" ") + ylab("Total abundance") +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") 
ggsave("Total_abundance.tiff")

#Calculate the number of species per ecoregion
new_data=data %>% select(Ecoregion, Taxa) %>%
  drop_na(Taxa) %>%
  group_by(Ecoregion) %>%
  distinct() %>% 
  count()

#Plot the number of species per ecoregion
Ecoregion= data.frame(Ecoregion = c("North Sea", "Gulf of Guinea Islands", "Baltic Sea"), n=c(0, 0, 0))
fig4 = rbind(new_data, Ecoregion) %>%
  ggbarplot(x = "Ecoregion", y = "n", 
            fill = "Ecoregion",               
            color = "white",            
            palette = "jco",            
            sort.val = "desc",          
            sort.by.groups = FALSE,     
            x.text.angle = 90) + coord_flip() +
  xlab(" ") + ylab(" Total number of taxa") +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") 
ggsave("Total_taxa.tiff")

#Plot of the number of UVC dimensions across studies
data %>% 
  ggdensity(x = "Dimensions", 
            add = "mean", rug = TRUE,
            color = "Method", fill = "Method",
            palette = c("#00AFBB", "#E7B800")) +
xlab("Survey area") + ylab("Density") +
  xlim(0, 1300)
ggsave("Survey.tiff")

#Plot of number of UVC per depth across studies
data %>% 
  ggdensity(x = "Depth", 
            add = "mean", rug = TRUE,
            color = "Method", fill = "Method",
            palette = c("#00AFBB", "#E7B800")) +
xlab("Depth") + ylab("Density")
ggsave("Depth.tiff")

#Plot of number of UVC per habitat across studies
data$Habitat= recode_factor(data$Habitat, "Farm" = "Farms", 
                             "Farm " = "Farms")
data %>%
  group_by(Habitat) %>%
  summarise(cases = n()) %>%
  ggbarplot(y = "cases", x= "Habitat",
            add = "mean", rug = TRUE,
            color = "Habitat", fill = "Habitat") +
            theme_bw(base_size = 20) +
            xlab("Habitat") + ylab("Number of counts") +
            theme(legend.position = "none")
ggsave("Habitat.tiff")

#Rarefraction curves
rare=read.table("./data/rare.txt", sep="\t", header=TRUE)
ggplot(data=rare, aes(x=Site, y=S, group= Ecoregion)) + 
         geom_line(size = 2, aes(color=Ecoregion)) +
  theme_classic() + xlab("Number of sites") + ylab("Species richness") +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.1)
ggsave("rarefraction.tiff")

#Plot the total area of UVCs per site across ecoregions
newdata=data %>%
  group_by(Site, Ecoregion) %>%
  summarise(areamedia=sum(Dimensions), abuntot=sum(Abun)) %>%
  mutate(Area=log10(areamedia)) %>%
  ungroup %>%
  as_tibble()

newdata %>%
ggboxplot(x = "Ecoregion", y = "Area",
          color = "Ecoregion",
          add = "jitter", shape = "Ecoregion") +
  coord_flip()

newdata %>%
  ggstatsplot::ggbetweenstats(Ecoregion, Area,
                            plot.type = "box",type = "non-parametric", pairwise.comparisons = TRUE, pairwise.display = "significant",p.adjust.method = "bonferroni",
                            xlab = " ", ylab = "Total survey area per site (log)") + theme_bw(base_size = 22) +
  coord_flip() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  theme_bw(base_size = 10)
ggsave("site.tiff")




