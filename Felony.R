library(tidyverse)
library(dplyr)
library(sf)
library(QuantPsyc)
library('RSocrata')
library(viridis)
library(caret)
library(spatstat)
library(data.table)
library(ggplot2)
library(ridge)
library(dummies)
library(caret)
library(forcats) 
library(BAMMtools)
library(rpart.plot)


setwd("/Users/hemingyi/Documents/musa507/Felony/")
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

# download NYPD Complaint Data Historic
FelonyData <- fread('https://data.cityofnewyork.us/api/views/qgea-i56i/rows.csv?accessType=DOWNLOAD')
head(FelonyData)
FelonyData <- read_csv('2018Felony.csv')
dim(FelonyData2018)
# filter, limit in 2018, category only violation and felony
FelonyData2018 <- FelonyData %>%
  mutate(year = unlist(strsplit(CMPLNT_FR_DT, "/"))[3],
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  filter(LAW_CAT_CD == "FELONY", year=="2018", Longitude>-74.1, Longitude<-73, Latitude>40, Latitude<41.1) %>%
  select(Latitude,Longitude,BORO_NM) %>%
  st_as_sf(coords = c('Longitude','Latitude'), crs = 4326, agr = "constant") %>%
  st_transform(crs=2263)


ggplot() + 
  geom_point(data=FelonyData2018, aes(Longitude,Latitude), colour="red", size=0.1) +
  labs(title= "Felony, New York City - 2018") +
  mapTheme()
#download nyc boundary
CensusTractBoundary <- 
  st_read("https://data.cityofnewyork.us/api/geospatial/fxpq-c8ku?method=export&format=GeoJSON") %>%
  st_transform(crs=2263)

StreetLight <- fread('https://data.cityofnewyork.us/api/views/tbgj-tdd6/rows.csv?accessType=DOWNLOAD')
StreetLight <- StreetLight %>%
  filter(`Pole type` == "Streetlight Pole") %>%
  st_as_sf(coords = c('Longitude','Latitude'), crs = 4326, agr = "constant") %>%
  select(`Pole type`) %>%
  st_transform(crs=226)


Trees <- fread('https://data.cityofnewyork.us/api/views/5rq2-4hqu/rows.csv?accessType=DOWNLOAD')
Trees <- Trees %>%
  st_as_sf(coords = c('longitude','Latitude'), crs = 4326, agr = "constant") %>%
  select(tree_dbh) %>%
  st_transform(crs=226)
view(Trees)
# nyc property sales data
#  "https://data.cityofnewyork.us/resource/w2pb-icbu.json?$where=sale_price > 1 AND sale_date between '2018-01-10T12:00:00' and '2019-01-10T14:00:00'"
PropertySales <- read.socrata(
  "https://data.cityofnewyork.us/resource/w2pb-icbu.json?$where=sale_price > 1",
  app_token = "Ijw4Y764tprjepffgHSfXDIhi",
  email     = "mh5172@nyu.edu",
  password  = "NYUhmy950218,,"
)
PropertySales <- PropertySales[!is.na(PropertySales$latitude)&!is.na(PropertySales$longitude)&!is.na(PropertySales$tax_class_as_of_final_roll),]
PropertySalesDF <- PropertySales %>%
  mutate(sale_date = as.Date(sale_date)) %>%
  filter(sale_price>0,gross_square_feet>0) %>% #,sale_date>'2016-01-01'
  select(latitude,longitude,sale_price,tax_class_as_of_final_roll,gross_square_feet) 
PropertySalesDF <- st_as_sf(PropertySalesDF, coords = c('longitude','latitude'), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(CensusTractBoundary))

view(PropertySales)

ggplot() + 
  geom_sf(data=CensusTractBoundary, aes(), fill=NA, colour="black") +
  labs(title = "New York City Boundaries of Census Tracts") +
  mapTheme()    


Demographics <- read_csv('new-york-city-census-data//nyc_census_tracts.csv')
GenerateCensusCode <- function(x){
  return(substring(x,6,))
}
Demographics <- Demographics %>%
  mutate(CensusTract = as.character(CensusTract),
         CensusCode = apply(Demographics[1], 2, GenerateCensusCode),
         CensusCode = paste(CensusCode,Borough,sep=''))

# merge demographics data on demographics boundaries

CensusTractDF <- CensusTractBoundary %>%
  select(boro_name, geometry,ct2010) %>%
  mutate(ct2010=as.character(ct2010),
         CensusCode=paste(ct2010,boro_name,sep=''),
         CensusCode=as.character(CensusCode)) %>%
  merge(Demographics, by.x='CensusCode', by.y = 'CensusCode')
dim(CensusTractDF)

# join neighborhood rent price on census tract

CensusTractPropertySale <- st_join(CensusTractDF, PropertySalesDF, join= st_contains)
CensusTractPropertySale <- na.omit(CensusTractPropertySale)

CensusTractPropertySaleGrouped <- CensusTractPropertySale %>%
  mutate(sale_price = as.numeric(sale_price),
         gross_square_feet = as.numeric(gross_square_feet),
    price_per_square = sale_price/gross_square_feet) %>%
  group_by(CensusCode,tax_class_as_of_final_roll) %>%
  summarise(median_sale_price = median(price_per_square)) %>%
  st_drop_geometry() %>%
  select(CensusCode,tax_class_as_of_final_roll,median_sale_price) %>%
  drop_na() %>%
  spread_(key_ = "tax_class_as_of_final_roll",
          value_ = "median_sale_price", 
          convert = TRUE,
          drop = FALSE) %>%
  rename(class1 = '1', class1A = '1A', class1C = '1C', class2 = '2', class2C = '2C', class4 = '4') %>%
  mutate(class1 = as.numeric(replace_na(class1,0)),
         class1A = as.numeric(replace_na(class1A,0)),
         class1C = as.numeric(replace_na(class1C,0)),
         class2 = as.numeric(replace_na(class2,0)),
         class2C = as.numeric(replace_na(class2C,0)),
         class1= max(class1,class1A,class1C),
         class2 = max(class2,class2C)) %>%
  select(CensusCode,class1,class2)
view(CensusTractPropertySaleGrouped)
CensusTractDF <- merge(CensusTractDF,CensusTractPropertySaleGrouped,by.x='CensusCode',by.y='CensusCode',all.x = TRUE)
dim(CensusTractDF)

CensusTractDF$ResidencePrice <- CensusTractDF$class1 + CensusTractDF$class2

# spatial join felony location to each census tract
FelonyCount <- st_join(CensusTractDF, FelonyData2018, join= st_contains) %>%
  group_by(CensusCode) %>%
  summarise(FelonyAmount = n()) %>%
  st_drop_geometry() 

StreetLightCount <- st_join(CensusTractDF, StreetLight, join= st_contains) %>%
  group_by(CensusCode) %>%
  summarise(StreeLightAmount = n())%>%
  st_drop_geometry() 

TreesCount <- st_join(CensusTractDF, Trees, join= st_contains) %>%
  group_by(CensusCode) %>%
  summarise(TreeAmount = n(),
            TreeDiameterSum = sum(tree_dbh)) %>%
  st_drop_geometry()

CensusTractDF <- merge(CensusTractDF,FelonyCount,by.x='CensusCode',by.y='CensusCode',all.x = TRUE)
CensusTractDF <- merge(CensusTractDF,StreetLightCount,by.x='CensusCode',by.y='CensusCode',all.x = TRUE)
CensusTractDF <- merge(CensusTractDF,TreesCount,by.x='CensusCode',by.y='CensusCode',all.x = TRUE)
dim(CensusTractDF)

CensusTractDF <- CensusTractDF %>%
  mutate(ResidencePrice = ifelse(is.na(ResidencePrice), 0, ResidencePrice),
         FelonyAmount = ifelse(is.na(FelonyAmount), 0, FelonyAmount),
         StreeLightAmount = ifelse(is.na(StreeLightAmount), 0, StreeLightAmount),
         TreeDiameterSum = ifelse(is.na(TreeDiameterSum), 0, TreeDiameterSum),
         TreeAmount = ifelse(is.na(TreeAmount), 0, TreeAmount),
         Salesin3Years = case_when(ResidencePrice > 0 ~ '1',
                               TRUE ~ '0')) %>%
  select(Borough,TotalPop,Men,Women,Hispanic,White,Black,Native,Asian,Citizen,
         Income,Poverty,Unemployment,ResidencePrice,StreeLightAmount,TreeAmount,Salesin3Years,FelonyAmount)
dim(CensusTractDF)  
view(CensusTractDF)
CensusTractDF <-  na.omit(CensusTractDF) 

write_csv(CensusTractDF,'censustractdf.csv')
# exploratory analysis
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")
ggplot() +
  geom_sf(data = CensusTractDF, mapping = aes(fill = FelonyAmount), color = 'white', size=0.1) +
  scale_fill_gradient(low="#66B904",high="red") +
  labs(title = "Felony Amount",
       subtitle = "New York; 2018") +
  mapTheme()

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

ggplot() +
  geom_histogram(data=CensusTractDF, mapping = aes(x = FelonyAmount), binwidth = 500)

p1 <- ggplot() +
  geom_point(data=CensusTractDF, mapping = aes(x=Salesin3Years,y=FelonyAmount))

p2 <- ggplot() +
  geom_point(data=CensusTractDF %>%
               filter(ResidencePrice>0)
             , mapping = aes(x=ResidencePrice,y=FelonyAmount))

p3 <- ggplot() +
  geom_point(data=CensusTractDF, mapping = aes(x=StreeLightAmount,y=FelonyAmount))

multiplot(p1,p2,p3)
ggplot() +
  geom_point(data=CensusTractDF, mapping = aes(x=Black,y=FelonyAmount))

ggplot() +
  geom_point(data=CensusTractDF, mapping = aes(x=Income,y=FelonyAmount))

ggplot() +
  geom_point(data=CensusTractDF, mapping = aes(x=Poverty,y=FelonyAmount))

ggplot() +
  geom_point(data=CensusTractDF, mapping = aes(x=Unemployment,y=FelonyAmount))
ggplot() +
  geom_point(data=CensusTractDF, mapping = aes(x=TreeAmount,y=FelonyAmount))
ggplot() +
  geom_point(data=CensusTractDF, mapping = aes(x=TreeDiameterSum,y=FelonyAmount))

# model
X <- cbind(CensusTractDF, dummy(CensusTractDF$Borough, sep = "_")) %>%
  st_drop_geometry() %>%
  select(-Borough) %>%
  sapply(as.numeric ) 

colnames(CensusTractDF)

AllData <- scale(X[, -c(17)]) %>%
  as.data.frame() %>%
  cbind(CensusTractDF %>% select(FelonyAmount) %>% st_drop_geometry() %>% as.data.frame()) %>%
  rename(Bronx = Borough..sep...._........st_drop_geometry.......select..Borough._Bronx,
         Brooklyn= Borough..sep...._........st_drop_geometry.......select..Borough._Brooklyn,
         Manhattan = Borough..sep...._........st_drop_geometry.......select..Borough._Manhattan,
         Queens = Borough..sep...._........st_drop_geometry.......select..Borough._Queens,
         Staten.Island = Borough..sep...._........st_drop_geometry.......select..Borough._Staten.Island)
names(AllData)

model <- train(
  FelonyAmount ~ ., 
  AllData,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)

print(model)

model$bestTune
model$metric
model$finalModel

regressor <- lm(formula = FelonyAmount ~ ., data = AllData %>% select(-FelonyAmount))
coef <- summary(regressor)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Variables")

view(coef)

ggplot() +
  geom_bar(data=coef, mapping = aes(x=reorder(Variables,Estimate),y=Estimate),stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

names(AllData)

predict(model, AllData[-c(22)])

# classification

ClassDF <- CensusTractDF %>%
  mutate(FelonyClass = cut(CensusTractDF$FelonyAmount, 
                           breaks = getJenksBreaks(CensusTractDF$FelonyAmount, 5, subset = NULL)),
         FelonyClass = as.character(FelonyClass),
         FelonyClass = fct_explicit_na(ClassDF$FelonyClass, na_level = "1")) %>%
  select(-FelonyAmount) %>%
  st_drop_geometry()

caret.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3)
grid <- expand.grid(method = "class",size=c(5,10,20,50), k=c(1,2,3,4,5))

rpart.cv <- train(FelonyClass ~ ., 
                  data = ClassDF,
                  method = "rpart",
                  trControl = caret.control,
                  tuneLength = 15)
print(rpart.cv)
names(ClassDF)
rpart.best <- rpart.cv$finalModel
rpart.best


prp(rpart.best, type = 0, extra = 1, under = TRUE)
predict(rpart.cv,ClassDF %>% select(-FelonyClass), type = "raw")
