rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(stars)


setwd('/Volumes/ext.drive/inat images/idigbio_morningside')

##idigbio
occ <- read.csv('occurrence.csv')
media <- read.csv('multimedia.csv')

joined <- left_join(occ,media, by = "coreid")


just_url <- joined%>%
  select(coreid, gbif.canonicalName, ac.accessURI, idigbio.eventDate, dwc.recordedBy)%>%
  filter(gbif.canonicalName == "")

sep <- separate(joined, gbif.canonicalName, into = c("genus", "species"), sep = " (?=[^ ]+$)")
sep[sep == ''] <- NA

species <- sep%>%
  select(genus, species)

counts <- species%>%
  group_by(genus, species)%>%
  summarize(counts = n())

genus_counts <- species%>%
  filter(!is.na(genus))%>%
  group_by(genus)%>%
  summarize(counts = n())

genus_counts <- species%>%
  group_by(family)%>%
  summarize(counts = n())

p <- ggplot(genus_counts, mapping = aes(x = genus, y = counts))+
  geom_bar(stat = 'identity')

p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##inat
setwd("/Volumes/ext.drive")

obs <- read.csv('observations-301341.csv')

just_url <- obs%>%
  select(id, scientific_name, image_url, observed_on, user_id)

sep <- separate(just_url, scientific_name, into = c("genus", "species"), sep = " (?=[^ ]+$)")

species <- sep%>%
  select(genus, species)

counts <- species%>%
  group_by(genus, species)%>%
  summarize(counts = n())

genus_counts <- species%>%
  filter(!is.na(genus))%>%
  group_by(genus)%>%
  summarize(counts = n())

p <- ggplot(genus_counts, mapping = aes(x = genus, y = counts))+
  geom_bar(stat = 'identity')

p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#group by family

#combine all inat families/combine all idigbio families
left_join(dm, natl, etc, by = family)

#from this data set, assign each data point a family name

####grouping families

setwd("/Volumes/ext.drive/inat images/spring2023/inat_idigbio_bias")

files <- list.files("/Volumes/ext.drive/inat images/spring2023/inat_idigbio_bias")
files
inat1 <- read.csv("observations-301341.csv")
inat2 <- read.csv("observations-301943.csv")
inat3 <- read.csv("observations-301947.csv")
inat4 <- read.csv("observations-301961.csv")

boul_idig <- read.csv("occurrence_Boulware.csv")
dev_idig <- read.csv("occurrence_devil.csv")
ali_idig <- read.csv("occurrence_Lake_Alice.csv")
natl_idig <- read.csv("occurrence_NATL.csv")

inat_rbind <- rbind(inat1, inat2, inat3, inat4)

inat_filtered <- inat_rbind%>%
  select(scientific_name)

inat_sep <- separate(inat_filtered, scientific_name, into = c("genus", "species"), sep = " (?=[^ ]+$)")

inat_counts <- inat_sep%>%
  group_by(genus)%>%
  summarize(counts = n())

write.csv(inat_counts, "/Volumes/ext.drive/inat images/spring2023/inat_genus_counts.csv")

inat_p <- ggplot(inat_counts, mapping = aes(x = genus, y = counts))+
  geom_bar(stat = 'identity')

inat_with_fam <- read.csv("/Volumes/ext.drive/inat images/spring2023/inat_genus_counts.csv")

inat_f_counts <- inat_with_fam%>%
  group_by(family)%>%
  summarize(counts = n())

write.csv(inat_f_counts, "/Volumes/ext.drive/inat images/spring2023/inat_family_counts.csv")

#idigbio combine

idig_rbind <- rbind(boul_idig, dev_idig, ali_idig, natl_idig)

idig_counts <- idig_rbind%>%
  group_by(dwc.family)%>%
  summarize(counts = n())

write.csv(idig_counts, "/Volumes/ext.drive/inat images/spring2023/idig_family_counts.csv")

##playing around with figures
rm(list = ls())
setwd("/Volumes/ext.drive/inat images/spring2023")

inat_fam <- read.csv("inat_family_counts.csv")
idig_fam <- read.csv("idig_family_counts.csv")

colnames(idig_fam) <- c("x", "family", "herb.counts")

colnames(inat_fam) <- c("x", "family", "inat.counts")

joined_fam <- full_join(inat_fam, idig_fam)
joined_fam[is.na(joined_fam)] <- 0

write.csv(joined_fam, "/Volumes/ext.drive/inat images/combined_plantFamilies.csv")

fam_plot <- ggplot() +
  geom_bar(joined_fam, mapping = aes(x = family, y = inat.counts), fill = "red", alpha = 0.7, stat = "identity") +
  geom_bar(joined_fam, mapping = aes(x = family, y = herb.counts), fill = "blue", alpha = 0.7, stat = "identity")

fam_plot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#maybe remove families with less than 5 observations?


#############pie chart?
# Load ggplot2
library(ggplot2)

# Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Basic piechart
ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric labels


#different sizes of dots
setwd('/Volumes/ext.drive/inat images/spring2023')
border <- read_sf('border_level6_polygon.shp')
xs_points <- read_sf("map.csv",
                     options = c("X_POSSIBLE_NAMES=lon",
                                 "Y_POSSIBLE_NAMES=lat"),
                     crs = 4326)


options(sf_max.plot=1)
plot(border)
plot(xs_points, add = TRUE)

cities <- read_sf('par_citylm_2021.shp')

gainesville <- cities%>%
  filter(NAME == "GAINESVILLE")

ggplot() + 
  geom_sf(data = border, aes()) +
  geom_sf(data = gainesville, aes())+
  geom_sf(data = xs_points, aes())+
  theme_minimal()

#from stack overflow, graduated points?
#https://stackoverflow.com/questions/58676661/geom-sf-mapping-points-shapes
inat_abund <- as.numeric(xs_points$total_inat)
idig_abund <- as.numeric(xs_points$total_idig)

ggplot() +
  geom_sf(data = gainesville, aes()) +
  geom_sf(data = xs_points,
    aes(size = inat_abund))+
  theme_minimal()

ggplot() +
  geom_sf(data = gainesville, aes()) +
  geom_sf(data = xs_points,
          aes(size = idig_abund))+
  theme_minimal()



#fixing sheets for bubble chart
#read in dm, natl, lake alice, boulware springs
setwd('/Volumes/ext.drive/inat images/spring2023/inat_idigbio_bias/')
files <- list.files("/Volumes/ext.drive/inat images/spring2023/inat_idigbio_bias")

idig_boul <- read.csv("occurrence_Boulware.csv")
sep1 <- separate(idig_boul, gbif.canonicalName, into = c("genus", "species"), sep = " (?=[^ ]+$)")
sep[sep == ''] <- NA

idig_LA <- read.csv("occurrence_Lake_Alice.csv")
sep2 <- separate(idig_LA, gbif.canonicalName, into = c("genus", "species"), sep = " (?=[^ ]+$)")
sep[sep == ''] <- NA

idig_dm <- read.csv("occurrence_devil.csv")
sep3 <- separate(idig_dm, gbif.canonicalName, into = c("genus", "species"), sep = " (?=[^ ]+$)")
sep[sep == ''] <- NA

idig_natl <- read.csv("occurrence_NATL.csv")
sep4 <- separate(idig_natl, gbif.canonicalName, into = c("genus", "species"), sep = " (?=[^ ]+$)")
sep[sep == ''] <- NA


inat_dm <- read.csv("observations-301341.csv")
sep5 <- separate(inat_dm, scientific_name, into = c("genus", "species"), sep = " (?=[^ ]+$)")
sep[sep == ''] <- NA

inat_LA <- read.csv("observations-301947.csv")
sep6 <- separate(inat_LA, scientific_name, into = c("genus", "species"), sep = " (?=[^ ]+$)")
sep[sep == ''] <- NA

inat_natl <- read.csv("observations-301943.csv")
sep7 <- separate(inat_natl, scientific_name, into = c("genus", "species"), sep = " (?=[^ ]+$)")
sep[sep == ''] <- NA

inat_boul <- read.csv("observations-301961.csv")
sep8 <- separate(inat_boul, scientific_name, into = c("genus", "species"), sep = " (?=[^ ]+$)")
sep[sep == ''] <- NA


#genus_counts for each park, give each a different name
#idigbio
boul_count <- sep1%>%
  group_by(dwc.family)%>%
  summarize(boul_name = n())

LA_count <- sep2%>%
  group_by(dwc.family)%>%
  summarize(la_name = n())

dm_count <- sep3%>%
  group_by(dwc.family)%>%
  summarize(dm_name = n())

natl_count <- sep4%>%
  group_by(dwc.family)%>%
  summarize(natl_name = n())

comb_idig1 <- full_join(boul_count, LA_count)
comb_idig2 <- full_join(dm_count, natl_count)
full_comb_idig <- full_join(comb_idig1, comb_idig2)


#inat
boul_count_inat <- sep8%>%
  group_by(genus)%>%
  summarize(boul_name = n())

LA_count_inat <- sep6%>%
  group_by(genus)%>%
  summarize(la_name = n())

dm_count_inat <- sep5%>%
  group_by(genus)%>%
  summarize(dm_name = n())

natl_count_inat <- sep7%>%
  group_by(genus)%>%
  summarize(natl_name = n())

inat1 <- full_join(boul_count_inat, LA_count_inat)
inat2 <- full_join(dm_count_inat, natl_count_inat)
full_comb_inat <- full_join(inat1, inat2)

#match to family 

families <- read.csv('/Volumes/ext.drive/inat images/spring2023/inat_genus_counts.csv')

inat_fam <- left_join(families, full_comb_inat)

write.csv(inat_fam, '/Volumes/ext.drive/inat images/spring2023/inat_families_bylocation.csv')
write.csv(full_comb_idig, '/Volumes/ext.drive/inat images/spring2023/idigbio_families_bylocation.csv')

##creating bar charts by location

byloc <- read.csv("idigbio_families_bylocation.csv")
loc <- read.csv("byparks.csv")
names(byloc) <- c("X","family","boul_name","la_name","dm_name","natl_name")

joined <- full_join(loc, byloc, by = "family")

boul <- joined%>%
  filter(boul_name >= 1)%>%
  
## sorting by phylum, class, and family
##ASSUMING THESE CATEGORIES ARE STORED IN THE DOWNLOADED DATAFRAMES
## so this would sort out bryophytes/liverworts by separating out phyla bryophyta and marchantiophyta
## then, sort out ferns via classes polypodiosida and filicospida
## then, flowering plants via liliospida and magnoliopsida
## then, grasses and sedges via order Poales
## so the groups would be: mosses/liverworts, flowering plants, ferns, graminoids
## sorted into separate dataframes? or just counting the rows and storing them in a separate dataframe?
  
#idigbio sorting

#inat sorting
library(dplyr)
inat_test <- read.csv("/Users/elizabethwhite/Downloads/observations-329987.csv")

moss_count <- inat_test%>%
  filter(taxon_phylum_name == "Bryophyta" | taxon_phylum_name == "Marchantiophyta")%>%
  summarize(counts = n())

class_count1 <- inat_test%>%
  filter(taxon_class_name == "Liliopsida" | taxon_class_name == "Magnoliopsida")%>%
  summarize(counts = n())

fern_count <- inat_test%>%
  filter(taxon_class_name == "Polypodiopsida")%>%
  summarize(counts = n())
  
grass_count <- inat_test%>%
  filter(taxon_order_name == "Poales")%>%
  summarize(counts = n())

#combining above data frames
df <- rbind(moss_count, class_count1, fern_count)

rownames(df) <- c("bryophyta", "flowering_plants", "ferns", "graminoids")

###CHANGE THESE TO APPROPRIATE LOCATION NAME
colnames(df) <- c("morningside")

write.csv(df, "/Volumes/ext.drive/inat images/morningside_counts.csv")

#or, if all the data is loaded in at the same time, you could repeat the above code for all locations and then combine, then write the csv



  

