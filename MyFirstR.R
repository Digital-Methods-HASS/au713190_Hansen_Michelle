#My first R script

#create order
#dir.create("data") #make sure you spell this exactly as it's the data destination 
#dir.create("figures")
#dir.create("output")

#Get data from the internet
download.file(
  "https://raw.githubusercontent.com/datacarpentry/r-socialsci/main/episodes/data/SAFI_clean.csv",
  "data/SAFI_clean.csv", mode = "wb"
)
#playing around with options
?download.file()

#create digital objects
10*10
area_hectares <- 1.0
area_hectares*10
x <- 132
y <- "animal"

area_hectares <- 2.5

area_acres <- area_hectares*2.47

#funktions 
b <- sqrt(a)
a <- 100

round(3,14159)
round(3.14159, digits = 2)#argument is named 
round(3.14159, 2) #argument not named
round(2, 3.14159) # this doesn't work 
round(digits = 2, x = 5.2345)
round(digits = 2, x = 5.2345)

#vectors and data types 

years <- c(10,3,7,6)
animals <- c("horse", "duck", "canary")
thing <- c(a,b,x,y)

#inspecting objects 

length(years)
typeof(years)
class(years)

#mental exesize 
num_char <- c(1,2,3,a)
class(thing)#the numbers would change into charekters 

num_logical <- c(1,2,3,TRUE)
char_logical

num_char <- c(1,2,3,"a")
num_logical <- c(1,2,3,TRUE)
char_logical <- c("a","b","c",TRUE)
tricky <- c(1,2,3, "4")
num_logical

years > 5

#subsetting 
animals
animals <- c("chicken",animals,"guinea pig")
animals <- c(animals,animals)
animals[2:5]
animals[9:6]
animals [10:1]
animals [-10]
animals [c(10,2,5)]

newanimals <- animals[c(10,2,2,5)]

years
years <- c(years,years)

years>5
years[c(TRUE, FALSE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE)]

years[years>5]


install.packages("tidyverse")
library(tidyverse)

#missing data
years <- c(2,1,1,NA,7)
#calculaing with missing data
mean(years, )
mean(years, na.rm = TRUE)#this means i know that data is missing
max(years,na.rm = T)
#stripping out missing data
is.na(years)#false mean no and true mean yes meaning 4 value is missing
years[!is.na(years)]
years_no_na <- years[!is.na(years)]
#other ways to do the same
years[complete.cases(years)]
na.omit(years)

#starting with data
interview <- read_csv("data/SAFI_clean.csv",na= c ("NULL"))#may have to use csv2

head(interview)
tail(interview)
interview$affect_conflicts

library(tidyverse)
#inspections functions
head(interview)#is a function that give you the first six rows in your data set 
colnames(interview)
view(interview)#lets you see the spreadsheet but it's not good to use because it doesn't show that R has understood the data.
tail(interview)#shows the last 6 rows of you spreadsheet

#subsetting dataframes 
interview$respondent_wall_type
#if you want to se specifik colums 
interview[,5:10]
interview["village"]
interview[["village"]]#to see it as a vector print out 
interview_100 <- c(1:100)
interview_100 <- interview

#solutions 
interview100 <- interview[1:100,]
interview100[100,]
ncol(interview)
nrow(interview)
# to get the last row
lastrow <- interview[131,]
lastrow2 <- interview[nrow(interview),]
lastrow3 <- interview[nrow(interview)-1,]

class(interview)

#subset with tidyverse 

filter(interview, village =="God" & no_meals>2)
colnames(interview)
select(interview, village, no_meals, years_liv)

#pipes

interview %>%
  select(village, no_meals, no_membrs, years_liv) %>% 
  filter(village=="God")

#exisize 

interview5<- interview %>% 
  select(affect_conflicts, liv_count, no_meals, memb_assoc) %>% 
  filter(memb_assoc=="yes")
#column create 
interview %>% 
  mutate(people_per_room=no_membrs/rooms) %>% 
  glimpse()

interview$memb_assoc

interview %>% 
  filter(!is.na(memb_assoc)) %>% 
  mutate(people_per_room=no_membrs/rooms) %>% 
  glimpse()

#sumerize 


interview %>% 
  filter(!is.no(memb_assoc)) %>%
  group_by(village, memb_assoc)) %>% 
  summarize(mean_no_members=mean(no_membrs)) %>% 
  arrage(desc(mean_no_members))

#count 
interview %>% 
  count(village, sort = TRUE)

## Not run, but can be used to load in data from previous lesson!
interview_plotting <- interview %>%
  ## pivot wider by items_owned
  separate_rows(items_owned, sep = ";") %>%
  ## if there were no items listed, changing NA to no_listed_items
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE)) %>%
  ## pivot wider by months_lack_food
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical = TRUE) %>%
  pivot_wider(names_from = months_lack_food,
              values_from = months_lack_food_logical,
              values_fill = list(months_lack_food_logical = FALSE)) %>%
  ## add some summary columns
  mutate(number_months_lack_food = rowSums(select(., Jan:May))) %>%
  mutate(number_items = rowSums(select(., bicycle:car)))

interview_plotting %>% 
  ggplot(aes(x=no_membrs, y= number_items))+
  geom_jitter(aes(color=respndent_wall_type),alpha=0.5,
              width=0.3)
interview_plotting %>% 
  ggplot(aes(x=respondent_wall_type, y = rooms))+
  geom_violin()+
  geom_jitter()+
  theme_classic()+
  labs(x="wall type", y="number of romm in a household")
