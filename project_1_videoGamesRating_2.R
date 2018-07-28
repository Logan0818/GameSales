#############################################################
#### Load packages and data #################################
#############################################################

getwd()
library(dplyr)
library(ggplot2)
library(corrplot)
GameSales  <- read.csv("Video_Games_Sales.csv")
GameRatings <- read.csv("Video_Games_with_ratings.csv")

############################################################
##### Browse Data ##########################################
############################################################

#Unique Publisher
unique(GameSales$Publisher)
#unique platform
unique(GameSales$Platform)
unique(GameSales$Year_of_Release) #found N/A, 2020

###################### not using for now ######
# Total 31 Platforms, find top 10 sales platforms
#Sales_per_platform <- GameSalesg %>%
#                      group_by(Platform)%>%
#                      summarise(ttl_sales_pf = sum(Global_Sales))%>%
#                      arrange(desc(ttl_sales_pf))
#Top10_Sales_platform <- Sales_per_platform[1:10,]
#ggplot(Top10_Sales_platform) + 
#  geom_bar(mapping = aes(x = reorder(Platform,-ttl_sales_pf),y = ttl_sales_pf,fill = Platform)
#           ,stat = "identity")

#################################################################
#### Data Cleaning ##############################################
#################################################################

## If column Name has blank, remove that row
GameSales <- filter(GameSales, !is.na(Name))
NA %in% GameSales$Name  #there're blank cells, check "" string#

## There are "N/A" in Year_of_Release, but we don't use this variable.
## How do you know which columns contain NA? We use Excel to check, but it's not a smart way.

#######Data Clean: check if there's duplicate publisher name in Top 10 publishers#############
## First, change all publishers to lowercase, use base R tolower() function
GameSales$publisher <- tolower(GameSales$Publisher)

# There's no other "nintendo"
index_nintendo <- grep(pattern = "nintendo", x = GameSales$publisher)
table(GameSales$publisher[index_nintendo])

# There's a subsidiaries called electronic arts victor with 2 games
index_ea <- grep(pattern = "electronic arts", x = GameSales$publisher)
table(GameSales$publisher[index_ea])

# we found there're 2 activision subsidiaries with 30 games 
index_activision <- grep(pattern = "activision", x = GameSales$publisher)
table(GameSales$publisher[index_activision])

# we found there're 4 sony subsidiaries with 27 games 
index_sony <- grep(pattern = "sony", x = GameSales$publisher)
table(GameSales$publisher[index_sony])

# we found there're 1 ubisoft subsidiaries with 14 games 
index_ubisoft <- grep(pattern = "ubisoft", x = GameSales$publisher)
table(GameSales$publisher[index_ubisoft])

# no take-two interactive subsidiaries (try "interactive" or "take")
index_interactive <- grep(pattern = "interactive", x = GameSales$publisher)
table(GameSales$publisher[index_interactive])

# no thq subsidiaries
index_thq <- grep(pattern = "thq", x = GameSales$publisher)
table(GameSales$publisher[index_thq])

# no konami subsidiaries
index_konami <- grep(pattern = "konami", x = GameSales$publisher)
table(GameSales$publisher[index_konami])

# no sega subsidiaries
index_sega <- grep(pattern = "sega", x = GameSales$publisher)
table(GameSales$publisher[index_sega])

# no namco subsidiariess
index_namco <- grep(pattern = "namco", x = GameSales$publisher)
table(GameSales$publisher[index_namco])

##copy a new column, merge those duplicate data into one publisher name##
GameSales$Publisher_Company <- GameSales$Publisher
GameSales$Publisher_Company <- sub("^Electronic Arts.*", "Electronic Arts", GameSales$Publisher_Company)
GameSales$Publisher_Company <- sub("^Activision.*", "Activision", GameSales$Publisher_Company) 
GameSales$Publisher_Company <- sub("^Sony.*", "Sony", GameSales$Publisher_Company) 
GameSales$Publisher_Company <- sub("^Ubisoft.*", "Ubisoft", GameSales$Publisher_Company) 


#################################################################
#### Top 10 Publishers ##########################################
#################################################################

#With total 582 publishers, we want to find top 10 global sales publisher
Sales_per_publisher <- GameSales %>%
                        group_by(Publisher) %>%
                        summarise(ttl_sales = sum(Global_Sales)) %>%
                        arrange(desc(ttl_sales))
Top10_Sales_publisher <- Sales_per_publisher[1:10,]

#plot, stat = 'identity', use directly the value, 
#   not using default count

# reorder legend, it's factor, so we need to reorder levels of factor (default by alphabet)
Top10_Sales_publisher$Publisher <- factor(Top10_Sales_publisher$Publisher,levels = rev(Top10_Sales_publisher$Publisher[order(Top10_Sales_publisher$ttl_sales)]),ordered = TRUE)

# how to make publisher name clear and sort by order?
# reorder function, minus sign means desc order
Top10 <- ggplot(Top10_Sales_publisher) + 
  geom_bar(mapping = aes(x = reorder(Publisher,ttl_sales),y = ttl_sales, fill = Publisher)
           ,stat = "identity") 
Top10 + labs(x = "Publishers", y = "Total Sales") + theme(legend.position = "top", axis.text.x = element_text(angle = 30, vjust = .6))+
  coord_flip()

#### Game Release Rate each year ################################
#################################################################
#game release rate each year by top 5 publishers: check Nintendo first
NumGameNintendo <- GameSales %>%
  filter(Publisher == "Nintendo", Year_of_Release != "N/A") %>%
  group_by(Year_of_Release) %>%
  summarise(num_of_games = n_distinct(Name))

# change Factor to Numeric value of varaible: Year_of_Release
NumGameNintendo$year<- as.numeric(as.character(NumGameNintendo$Year_of_Release))

ggplot(NumGameNintendo) +
  geom_point(mapping = aes(x=year, y=num_of_games),
                     stat = 'identity') +geom_line(mapping = aes(x=year, y=num_of_games),
                    stat = 'identity')+ labs(x = "Year", y = "No. of Games - Nintendo")
#2004 spike: NDS launch, 2006 launch wii

## Now let's see how EA's perform ##
NumGameEA <- GameSales %>%
  filter(Publisher == "Electronic Arts", Year_of_Release != "N/A") %>%
  group_by(Year_of_Release) %>%
  summarise(num_of_games = n_distinct(Name))

# change Factor to Numeric value of varaible: Year_of_Release
NumGameEA$year<- as.numeric(as.character(NumGameEA$Year_of_Release))

ggplot(NumGameEA) +
  geom_point(mapping = aes(x=year, y=num_of_games),
             stat = 'identity') +geom_line(mapping = aes(x=year, y=num_of_games),
                                           stat = 'identity')+ labs(x = "Year", y = "No. of Games - EA")
# how to explain?

####################################################################
#### 7/7/2018 ######################################################
####################################################################

# Top 5 publishers by total sales: Nintendo, EA, Activision, Sony, Ubisoft
# Take a look of above 5 publishers total sales by Genre

#####################################################################
########## Nintendo #################################################
#####################################################################

Nintendo_sales_Genre <- GameSales %>%
                  filter(Publisher == "Nintendo") %>%
                  group_by(Genre)  %>%
                  summarise(sales = sum(Global_Sales)) %>%
                  arrange(desc(sales)) %>%
                  mutate(p_sales = sales/sum(sales)*100)
Nintendo_sales_Genre
# Platform (23.8%) > Role-Play(16.3%) > Sports(12.1%)
# pie chart... not a good representation 

# Number of Games per Genre, is it correlated with above sales per Genre?
Nintendo_nGame_Genre <- GameSales %>%
                        filter(Publisher == "Nintendo") %>%
                        group_by(Genre)  %>%
                        summarise(nGame = n()) %>%
                        arrange(desc(nGame)) %>%
                        mutate(p_nGame = nGame/sum(nGame)*100)
Nintendo_nGame_Genre
# Platform (15.9%) > Role-Play (15.3%) > Misc (14.3%) ---Sports(7.8%)


Nintendo_sales_Genre_Name <- GameSales %>%
                        filter(Publisher == "Nintendo") %>%
                        group_by(Genre,Name)  %>%
                        summarise(sales = sum(Global_Sales)) %>%
                        arrange(desc(sales)) 
Nintendo_sales_Genre_Name[1:10, ]

#####################################################################
########## Electronic Arts (EA) #####################################
#####################################################################

#Let's see EA , 43% of sales comes from Sports
EA_sales_Genre <- GameSales %>%
  filter(Publisher == "Electronic Arts") %>%
  group_by(Genre)  %>%
  summarise(sales = sum(Global_Sales)) %>%
  arrange(desc(sales)) %>%
  mutate(p_sales = sales/sum(sales)*100)
EA_sales_Genre

# 41% of Games is Sports, same as sales result
EA_nGame_Genre <- GameSales %>%
  filter(Publisher == "Electronic Arts") %>%
  group_by(Genre)  %>%
  summarise(nGame = n()) %>%
  arrange(desc(nGame)) %>%
  mutate(p_nGame = nGame/sum(nGame)*100)
EA_nGame_Genre

#See which games are more popular...very stable for each game
EA_sales_Genre_Name <- GameSales %>%
  filter(Publisher == "Electronic Arts") %>%
  group_by(Genre,Name)  %>%
  summarise(sales = sum(Global_Sales)) %>%
  arrange(desc(sales))
EA_sales_Genre_Name[1:10, ]

#####################################################################
########## Activision ###############################################
#####################################################################

#Let's see Activision , 42% of sales comes from Shooter
Activision_sales_Genre <- GameSales %>%
  filter(Publisher == "Activision") %>%
  group_by(Genre)  %>%
  summarise(sales = sum(Global_Sales)) %>%
  arrange(desc(sales)) %>%
  mutate(p_sales = sales/sum(sales)*100)
Activision_sales_Genre

# However, Shooter only counts for 16%, Action is 31%
Activision_nGame_Genre <- GameSales %>%
  filter(Publisher == "Activision") %>%
  group_by(Genre)  %>%
  summarise(nGame = n()) %>%
  arrange(desc(nGame)) %>%
  mutate(p_nGame = nGame/sum(nGame)*100)
Activision_nGame_Genre

#See which games are more popular...Top 8 games are all "Call of Duty"
Activision_sales_Genre_Name <- GameSales %>%
  filter(Publisher == "Activision") %>%
  group_by(Genre,Name)  %>%
  summarise(sales = sum(Global_Sales)) %>%
  arrange(desc(sales))
Activision_sales_Genre_Name[1:10, ]

#####################################################################
#############  SONY  ################################################
#####################################################################

#Let's see Sony , Racing 18%, Platform 17%, Action 15%
Sony_sales_Genre <- GameSales %>%
  filter(Publisher == "Sony Computer Entertainment") %>%
  group_by(Genre)  %>%
  summarise(sales = sum(Global_Sales)) %>%
  arrange(desc(sales)) %>%
  mutate(p_sales = sales/sum(sales)*100)
Sony_sales_Genre

# Racing games is ranked 5th, but sales is 1st
Sony_nGame_Genre <- GameSales %>%
  filter(Publisher == "Sony Computer Entertainment") %>%
  group_by(Genre)  %>%
  summarise(nGame = n()) %>%
  arrange(desc(nGame)) %>%
  mutate(p_nGame = nGame/sum(nGame)*100)
Sony_nGame_Genre

#See which games are more popular...GT racing game and FF7 is Sony's
# top selling games overall
Sony_sales_Genre_Name <- GameSales %>%
  filter(Publisher == "Sony Computer Entertainment") %>%
  group_by(Genre,Name)  %>%
  summarise(sales = sum(Global_Sales)) %>%
  arrange(desc(sales))
Sony_sales_Genre_Name[1:10, ]

#####################################################################
############# Ubisoft ###############################################
#####################################################################

#Let's see Ubisoft, Action 30% of sales
Ubisoft_sales_Genre <- GameSales %>%
  filter(Publisher == "Ubisoft") %>%
  group_by(Genre)  %>%
  summarise(sales = sum(Global_Sales)) %>%
  arrange(desc(sales)) %>%
  mutate(p_sales = sales/sum(sales)*100)
Ubisoft_sales_Genre

# Action 21%, 
Ubisoft_nGame_Genre <- GameSales %>%
  filter(Publisher == "Ubisoft") %>%
  group_by(Genre)  %>%
  summarise(nGame = n()) %>%
  arrange(desc(nGame)) %>%
  mutate(p_nGame = nGame/sum(nGame)*100)
Ubisoft_nGame_Genre

#See which games are more popular...Assassin's Creed, Just Dance
Ubisoft_sales_Genre_Name <- GameSales %>%
  filter(Publisher == "Ubisoft") %>%
  group_by(Genre,Name)  %>%
  summarise(sales = sum(Global_Sales),sales_NA = sum(NA_Sales),
    sales_EU = sum(EU_Sales),sales_other = sum(Other_Sales)) %>%
  arrange(desc(sales))
Ubisoft_sales_Genre_Name[1:10, ]

####################################################################
##### 7/14/2018 ####################################################
####################################################################

#### merge 2 dataframes, GameSales and GameRatings    ##############

# Create an unique key
#GameSalesKey <- GameSales %>%
#                  mutate(Key = paste(Name, Platform,Year_of_Release, sep = "_"))
#GameRatingsKey <- GameRatings %>%
#                    mutate(Key = paste(Name, Platform,Year_of_Release, sep = "_"))


# Use left join
GameSalesRatings <- merge(GameSales, GameRatings, by.x=c("Name", "Platform", "Year_of_Release", "Genre"), 
                          by.y = c("Name", "Platform", "Year_of_Release", "Genre"), all.x = TRUE)

# The total obs. increase to 16721? why? That means our key is not unique.
# How to find duplicate key and decide which one to remove?

###############################################################################################
# find the 2 duplicates
#######There's something wrong with below codes, Rmd can't run####################3

unique(GameSales[, c("Name", "Platform",  "Year_of_Release")])

GameSales %>%
  select(Name, Platform, Year_of_Release) %>%
  group_by(Name, Platform, Year_of_Release) %>%
  summarise(n=n())  %>%
  filter(n>1)

GameSales %>%
  filter(Name == "")  

# filter out the 2 rows with blank data in Name
GameSales <- GameSales[GameSales$Name!= "", ]

GameSales %>%
  filter(Name == "Madden NFL 13", Platform == "PS3")

#find index of the duplicate row
which(GameSales$Name == "Madden NFL 13"& GameSales$Platform == "PS3" & GameSales$EU_Sales == 0.01 )
GameSales <- GameSales[-16232,]
GameSales$EU_Sales[605] <- 0.23
GameSales$Global_Sales[605] <- 2.57
GameSales[605, ]

##### Correlation test #################################################
########################################################################
## want to know if Global Sales correlates with User_score

str(GameSalesRatings)
# found User_Score is Factor data type, need to change to numeric type
GameSalesRatings$User_score <- as.numeric(GameSalesRatings$User_Score)
GameSalesRatings$User_Score <- NULL
GameSalesRatings %>%
  select_if(is.numeric) %>%
  filter(!is.na(Critic_Score), !is.na(Critic_Count),
         !is.na(User_Count), !is.na(User_score)) %>%
  cor() %>%
  corrplot.mixed(tl.col = "black", lower.col = "black", number.cex = 0.7)
  
########################################################################
## check which Genre has top sales each year
########################################################################
GameSales %>%
  group_by (Year_of_Release, Genre)  %>%
  summarise(Top_Sales_Genre = sum(Global_Sales))  %>%
  arrange(Year_of_Release, desc(Top_Sales_Genre)) %>%
  filter(Year_of_Release != "N/A")  -> Year_Genre 

# change data type of Year_of_Release to numeric
as.numeric(as.character(Year_Genre$Year_of_Release)) -> Year_Genre$Year_of_Release
str(Year_Genre)

#Year_Genre  %>%
#  group_by(Year_of_Release)  %>%
#  summarise(max(Top_Sales_Genre))  

df0 <- NULL

for (p in (unique(Year_Genre$Year_of_Release))){
  df <- Year_Genre %>%
            filter(Year_of_Release == p, Top_Sales_Genre == max(Top_Sales_Genre))
  df0 <- rbind(df0, df)
}  

df0
print(tbl_df(df0), n=39)

########################clean data: Year_of_Release 2020 #############
## not yet

#############by region#######################################
library(dplyr)
##NA - Action > Sports > Shooter > Platform > Racing > Role-Playing
NAsales_Genre <- GameSales %>%
  group_by(Genre)  %>%
  filter(Genre != "Misc") %>%
  summarise(NAsales = sum(NA_Sales))  %>%
  arrange(desc(NAsales))

NAsales_Genre$Genre <- factor(NAsales_Genre$Genre,levels = rev(NAsales_Genre$Genre[order(NAsales_Genre$NAsales)]),ordered = TRUE)

ggplot(NAsales_Genre) + geom_bar(aes(x = Genre, y = NAsales,fill = Genre),stat = "identity")

##EU - Action > Sports > Shooter > Racing > Platform > Role-Playing (only a little bit different on 4th and 5th)
EUsales_Genre <- GameSales %>%
  group_by(Genre)  %>%
  filter(Genre != "Misc") %>%
  summarise(EUsales = sum(EU_Sales))  %>%
  arrange(desc(EUsales))

EUsales_Genre$Genre <- factor(EUsales_Genre$Genre,levels = rev(EUsales_Genre$Genre[order(EUsales_Genre$EUsales)]),ordered = TRUE)

ggplot(EUsales_Genre) + geom_bar(aes(x = Genre, y = EUsales,fill = Genre),stat = "identity")

##JP - Role-Playing > Action > Sports > Platform > Fighting (Very different from NA & EU.
## Role-Playing is the 1st, doubling the 2nd one. Shooter is the last one!)
JPsales_Genre <- GameSales %>%
  group_by(Genre)  %>%
  filter(Genre != "Misc") %>%
  summarise(JPsales = sum(JP_Sales))  %>%
  arrange(desc(JPsales))

JPsales_Genre$Genre <- factor(JPsales_Genre$Genre,levels = rev(JPsales_Genre$Genre[order(JPsales_Genre$JPsales)]),ordered = TRUE)

ggplot(JPsales_Genre) + geom_bar(aes(x = Genre, y = JPsales,fill = Genre),stat = "identity")