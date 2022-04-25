y1 <- read.csv("geb13296-sup-0011-carnidiet.csv", sep = ",")
str(y1)

library (dplyr)
library (vcd)

y2 <- y1 %>%
  select(familyCarni, scientificNameCarni , commonNameCarni, 
         foodType, orderPrey, familyPrey, genusPrey, scientificNamePrey, commonNamePrey,
         percentage, country ) %>% 
  count(familyCarni, scientificNameCarni , commonNameCarni, 
        foodType, orderPrey, familyPrey, genusPrey, scientificNamePrey, commonNamePrey,
        percentage, country, sort=T)
y2
class(y2)
y2 %>%
  filter(familyCarni == "Felidae") %>%
  select(commonNameCarni,  orderPrey) %>% 
  count(commonNameCarni,  orderPrey, sort=T)

y3<-y2%>%
  filter(commonNameCarni %in% c("Geoffroy's cat","Jaguar","Jaguarundi","Margay","Ocelot","Oncilla","Puma"))%>%
  select(commonNameCarni,  orderPrey) %>% 
  count(commonNameCarni, orderPrey, sort=T)

library(reshape2)
y4=dcast(y3, orderPrey ~ commonNameCarni , value.var = "n")
class(y4)

library(janitor)
y4<-y4[-c(5,13,14), ]

library(tidyr)
y4<-mutate_all(y4, ~replace_na(.,0))
web<-as.matrix(y4, row.names=1)
class(y4)

colnames(y4)<-NULL
colnames(y4)<-c("","Geoffroy's cat","Jaguar","Jaguarundi","Margay","Ocelot","Oncilla","Puma")
y4 <- noquote(y4)

web<-as.matrix(y4, row.names=1)
print(web, row.names = F)
write.csv(web, "geb13296-sup-0011-carnidiet2.csv", row.names=FALSE)
web<-as.matrix(read.csv("geb13296-sup-0011-carnidiet2.csv", row.names=1))

##########################

library(bipartite)

plotweb(web, labsize=1.3, text.rot=90, col.high="#6699CC", 
        col.low="#999933", col.interaction="#999999")

visweb(web, type="diagonal") 

png(file = "felids_diet.png", width = 800, height = 700)

res <- computeModules(web)
plotModuleWeb(res)

dev.off()

##########################

library(magick)
library(magrittr) 

# Call back the plot
plot <- image_read("felids_diet.png")
plot2<-image_annotate(plot, "CarniDIET 1.0: A database of terrestrial carnivorous mammal diets", 
                      color = "blue", size = 25,
                      location = "10+50", gravity = "north")
plot3<-image_annotate(plot2, "Visualization by @fblpalmeira
                      Data: Middleton et al 2021 (doi.org/10.1111/geb.13296)
                      Image credit: Microbiotheria (Zimices), Other spp (Public domain) @PhyloPic", 
                      color = "gray", size = 12, 
                      location = "10+50", gravity = "southeast")
# And bring in a logo
chiroptera <- image_read("http://www.phylopic.org/assets/images/submissions/18bfd2fc-f184-4c3a-b511-796aafcc70f6.512.png") 
out1<-image_composite(plot3,image_scale(chiroptera,"x30"), offset = "+135+90")

didelphis <- image_read("http://www.phylopic.org/assets/images/submissions/96ed3343-20e6-4bea-90e3-06dd157d2447.512.png") 
out2<-image_composite(out1,image_scale(didelphis,"x20"), offset = "+135+135")

bradypus <- image_read("http://www.phylopic.org/assets/images/submissions/dbc172df-a4c7-4d0d-9388-c337d01aca52.512.png") 
out3<-image_composite(out2,image_scale(bradypus,"x40"), offset = "+135+160")

cebidae <- image_read("http://www.phylopic.org/assets/images/submissions/6264cf2b-4f34-4b3f-ba65-cc5565db2de5.512.png") 
out4<-image_composite(out3,image_scale(cebidae,"x40"), offset = "+135+210")

rodentia <- image_read("http://www.phylopic.org/assets/images/submissions/b6c7dab1-a2f8-43f6-98d5-ab6000f58957.512.png") 
out5<-image_composite(out4,image_scale(rodentia,"x30"), offset = "+135+255")

carnivora <- image_read("http://www.phylopic.org/assets/images/submissions/040da77c-f13e-4962-ab09-4e4d0474e204.512.png") 
carnivora2<-image_flop(carnivora)
out6<-image_composite(out5,image_scale(carnivora2,"x30"), offset = "+135+310")

pecari <- image_read("http://www.phylopic.org/assets/images/submissions/44fb7d4f-6d59-432b-9583-a87490259789.512.png") 
out7<-image_composite(out6,image_scale(pecari,"x35"), offset = "+130+345")

armadillo <- image_read("http://www.phylopic.org/assets/images/submissions/5d59b5ce-c1dd-40f6-b295-8d2629b9775e.512.png") 
out8<-image_composite(out7,image_scale(armadillo,"x30"), offset = "+135+385")

lagomorpha <- image_read("http://www.phylopic.org/assets/images/submissions/c3987570-9c82-41b9-bc72-cb5d3de1ff87.original.png") 
out9<-image_composite(out8,image_scale(lagomorpha,"x40"), offset = "+140+415")

micro <- image_read("http://www.phylopic.org/assets/images/submissions/33eb7221-53b6-4d16-b79a-ce96e24e3754.512.png") 
out10<-image_composite(out9,image_scale(micro,"x30"), offset = "+120+465")

tapirus <- image_read("http://www.phylopic.org/assets/images/submissions/8f6b8802-52f9-4f16-8429-0b86ea4a4aa8.512.png") 
out11<-image_composite(out10,image_scale(tapirus,"x45"), offset = "+120+500")

image_browse(out11)

# And overwrite the plot without a logo
image_write(out11, "felids_diet2.png")

