install.packages("tidyverse")
install.packages("sf")
install.packages("tmap")

library(tidyverse)
library(sf)
library(tmap)

bmp <- read.csv("./BMPreport2016_landbmps.csv")

glimpse(bmp)

bmpcosts<- bmp %>% select(StateAbbreviation, Cost)
Task1.1<- bmpcosts %>% group_by (StateAbbreviation) %>% summarise (totalcost = sum(Cost,na.rm=T), Mean = mean(Cost,na.rm=T), Median = median(Cost, na.rm=T), Maximum_Cost = max(Cost, na.rm=T), Minimum_Cost = min(Cost, na.rm=T))

glimpse(Task1.1)

costvamountcredited<- bmp %>% filter (Unit == "Acres") %>% select(Unit,Cost,TotalAmountCredited,BMPShortName)

Task1.2 <- costvamountcredited 

Task1.2%>% ggplot (., aes(x = TotalAmountCredited, y = Cost)) + geom_point() 

Task1.2skwd <- costvamountcredited %>% mutate (skwdcost = log1p(Cost), TACskwd = log1p(TotalAmountCredited ))

Task1.2skwd %>% ggplot(., aes(x = TACskwd, y = skwdcost)) + geom_point() + labs(x = "Total Amount Credited", y = "Cost") #attempt to skew data values, numerous 0 values, but values are extremely large use log1p instead of log10


Task1.3 <- bmp %>% filter(str_detect(BMP,"Cover Crop")) %>%select(StateAbbreviation, TotalAmountCredited, BMP)

Task1.3 %>% ggplot(.,aes(x=StateAbbreviation, y= TotalAmountCredited)) + geom_boxplot(aes(fill = StateAbbreviation))
Task1.3 %>% filter (., TotalAmountCredited > 1 & TotalAmountCredited<200) %>% ggplot(.,aes(x=StateAbbreviation, y= TotalAmountCredited)) + geom_boxplot(aes(fill = StateAbbreviation)) #Transformed option if the graph looks too skewed 

Dams<- read_sf("./Dam_or_Other_Blockage_Removed_2012_2017.shp")

Task1.4<- Dams %>% filter(YEAR != 0 & !is.na(YEAR)) %>% select(YEAR, STATE) 

Task1.4%>% ggplot(., aes (x = YEAR, y = STATE)) + geom_point()

Task1.5<- bmp %>% select(StateAbbreviation, BMPType) %>% filter (StateAbbreviation == "PA") %>% count(BMPType)
Task1.5 %>% ggplot(., aes(x = BMPType, y = n)) + geom_bar (stat = "identity") + labs ( title = "Distribution of BMP Types in Pennsylvania", x = "BMP Type", y = "Frequency", fill = "BMP Type") #Wanted to look at the frequency of certain BMP within the state of Pennsylvania 

Streams<- read_sf ("./Streams_Opened_by_Dam_Removal_2012_2017.shp")

Task2.1<- Streams %>% select(GNIS_Name, LengthKM) %>% arrange(LengthKM)
Task2.1 %>% slice_max(order_by = LengthKM, n = 5)

Task2.2 <- Streams %>% select (GNIS_Name, LengthKM, FCode) %>% group_by(FCode) %>% summarise(Totallength = sum (LengthKM))
Task2.2 %>% slice_max(order_by = Totallength, n=3)


Counties<- read_sf("./County_Boundaries.shp")

Counties %>% sf:: st_is_valid()
Counties <- Counties %>% sf::st_make_valid()

countybmp<- bmp %>% group_by (GeographyName) %>% summarise(Totalcost = sum(Cost, na.rm = T)) 
countybmp<- countybmp %>% mutate(GEOID10 = stringr:: str_sub(GeographyName, 1, 5))

countybmp<- Counties %>% left_join(countybmp, by = "GEOID10")

#Results for Task 2.3

tm_shape(countybmp) + tm_polygons(fill = "Totalcost") # this is the map generated for the spatial operation task 2.3 

Counties %>% sf::st_crs() == Streams %>% sf::st_crs()
Counties %>% sf::st_crs() == Dams %>% sf::st_crs()

D<- Dams %>% select (DAM_NAME, DamRemoval)
S<- Streams %>% select (GNIS_Name)

distance<- sf::st_distance (D, S) %>% as_tibble()

mindist<- apply(distance, 1, which.min) 

strmname<- S$GNIS_Name [mindist]

Task2.4<- data.frame(D, strmname)

Statedams<- st_join(Dams, Counties) 

Task2.5<- Statedams %>% group_by(STATE) %>% summarise(Dams_removed = n())









                      


