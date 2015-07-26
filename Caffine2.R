rm(list=ls())
library(dplyr)
library(ggvis)
library(ggplot2)
library(psych)
library(rCharts)

#------------------------------------------------
# Load data 

caff = read.csv("C:\\Users\\cfrenzel\\Documents\\Code\\Practice\\caff.csv", 
                stringsAsFactors=F)
str(caff)
attach(caff)
caff$type = factor(caff$type)

# rounding the serving sizes
caff$fl_oz = round(caff$fl_oz,0)

#removing drinks without caffeine
caff = caff[ which(caff$caff_mg != 0) , ]

#-----------------------------------------------
# Explore

summary(caff)

# Caffeine Content

# Drinks with the highest caff content vs with the lowest caff content 
caff <- caff[order(-caff$caff_mg),]
head(caff) # Energy Drinks and Shots
tail(caff) # Soda

# Confirm findings
describe.by(caff_mg, group=type)

caff %>%
  group_by(type) %>%
    summarize( avg_caf = round(mean(caff_mg),2)
               , med_caf = median(caff_mg)
               , count = n()
               , ttl_caff = round(sum(caff_mg),2)
               , ttl_oz = sum(fl_oz)) %>%
      mutate(type_ratio = round(ttl_caff/ttl_oz,2)) %>%
        select( -ttl_caff, -ttl_oz) %>%
          arrange(desc(avg_caf, count))

# Shots have the highest average amount of caffine and undoubelty have the
# highest ratio of serving size to caffeine amount
# Surprisingly, coffee apears to be the rarest in the data, although it is the 2nd
#most potent

describe(caff_mg)
caff %>% ggvis(x = ~caff_mg , fill := "#FF9900") %>%
  layer_histograms(width=20) %>%
  add_axis("x", title="Caffeine Mg", title_offset = 40) %>% 
  add_axis("y", title="Counts", title_offset = 40) 
# left skwed distorbtion fo the caffine - as expected.
# 75% of drinks are between the 0 to 150 mg mark
# splitting the histogram by type of drink might be a bit more revealing


#taking out soda and energy drink out for scaling
ggplot(
  subset(caff, type != "Soda" & type != "Energy Drink")
  , aes(x=caff_mg)) + geom_histogram(binwidth=20, color="black", fill="orange") +
  facet_wrap( ~type) + 
    xlim(0,300) +
     xlab("Caffeine Mg") + ylab("Counts") +
      ggtitle("Distrobution of Caffeine in Drinks") +
        theme_bw()

#this is what soda looks like and is contributing greatly to the skewness of 
# the caffeine distrobution along with Tea
ggplot(
  subset(caff, type == "Soda"  | type == "Energy Drink")
  , aes(x=caff_mg)) + geom_histogram(binwidth=20, color="black", fill="orange") +
    xlim(0,300) +  facet_wrap( ~type) +
     xlab("Caffeine Mg") + ylab("Counts") +
      ggtitle("Distrobution of Caffeine in Drinks") +
        theme_bw()

# Finally, one more way to look at the contious variable across type
ggplot(caff, aes(x=type, y=caff_mg)) + geom_boxplot(aes(fill=type)) +
  coord_flip() + ylim(0,500) +
    xlab("") + ylab("Caffeine Mg") +
      ggtitle("Caffeine Drinks Boxplot") +
        scale_fill_hue(name="Drink Type") 
  
#----------------------------------------------------
# Serving Sizes

# Largest drinks versus smallest drinks
caff <- caff[order(-caff$fl_oz),]
head(caff,10) # Tea and energy drinks
tail(caff,10) # mixable energy drinks

# Majority of servigs are in : 12 oz 24% , 8 oz 21.8% , 16 oz 19%, 2 oz 12.5%
# These make up about 78% of all of the observations presents
serv = data.frame(prop.table(table(round(fl_oz,0)))*100)
serv %>% arrange(desc(Freq)) %>% head()

describe(fl_oz)
caff %>% ggvis(x = ~fl_oz , fill := "#0000FF") %>%
  layer_histograms(width=2) %>%
  add_axis("x", title="Serving Sizes (Fl oz)", title_offset = 40) %>% 
  add_axis("y", title="Counts", title_offset = 40) 

# Splitting by the Drink Type again - dsicrete so using a bar plot
# excluding outliers for plot and treating energy drinks seperately

serv2 = as.data.frame(ftable(prop.table(table(fl_oz,type),2)))
#xtabs(Freq ~  type, data= serv2)

nPlot(Freq ~ fl_oz, group='type', data=serv2, type='multiBarChart')
# This graphs was built in r wtih D3JS - you can hover over it to see the valus
# Looking at the distrbution is clear that the majority of shots come in 2 ounces
# and the majoirty of sodas are in 12.
# Energy drinks appear to span the spectrum and Coffee is showing up 30% of the time as
# 16 oz and 20% of the times as 8.

#------------------------------------------------------------------------------
# ratio Ounces to Miligrams of Caffeeine

# This is true measure of purity fo the drink.
# The higher this ratio is the more caffeine per ounce is taken in.
# Even though the last 2 variables has show a top 10 - this one will be the most 
# reavling because of the potency - or lack thereof - of the brews.

# Drinks with the highest conent per oz vs the lowest content per oz
caff <- caff[order(-caff$ratio),]
head(caff) # DynaPep and Liquid Caffeine
tail(caff) # Choclate Drinks and Tea

describe(ratio)

caff %>% 
  filter(ratio < 400 ) %>%
  ggvis(x = ~ratio , fill := "orange") %>%
  layer_histograms(width=5) %>%
  add_axis("x", title="Ratio of Miligrams per Ounce", title_offset = 40) %>% 
  add_axis("y", title="Counts", title_offset = 40) 

describe.by(ratio, type)

ggplot(caff, aes(x=type, y=ratio)) + geom_boxplot(aes(fill=type)) +
  coord_flip() + ylim(0,500) +
    xlab("") + ylab("Ratio of Miligrams per Ounce") +
      ggtitle("Caffeine Drinks Boxplot") +
        scale_fill_hue(name="Drink Type") 

#It becomes relatively clear that the shots are packing the higest amount of caffine per 
# ounce. Coffee is far behind  in distant second wiht is median falling much closer to 
# energy drinks. The media for coffe is 10.25 mg of caffine per ounce while energy drinks
# have a median of 9.5. The Energy drink average is far higher again - reflective of the numberf
# of energy drink information avialable compared to coffee in the database.
# It's hard to see on the chart but tea has a higher median than, Soda.
# The reason for this, is that Soda's not normally thought as caffienated - like sprite 
# have trace amounts in them.

ggplot(data=  subset(caff, caff_mg < 500)
       , aes(caff_mg, ratio, color=type)) + 
  geom_point() +
  facet_grid( .~type) +
  stat_smooth(mehtod=lm, aes(fill = type))

# Graphs of the linear relationship present between ratio and caffeine amount

caff %>%    #excluding higher outliers for scale
  filter(caff_mg < 400 & type == 'Energy Drink' & ratio < 200) %>%
  ggvis(x= ~caff_mg, y= ~ratio) %>%
  layer_points( fill := "#009933") %>%
  layer_smooths(stroke := "red", se=T) %>% 
  add_axis("x", title="Caffine Mgs", title_offset = 40) %>% 
  add_axis("y", title="Ratio of Miligrams per Ounce", title_offset = 40) 

caff %>%    #excluding higher outliers for scale
  filter(caff_mg < 400 & type == 'Soda' & ratio < 200) %>%
  ggvis(x= ~caff_mg, y= ~ratio) %>%
  layer_points( fill := "#663300") %>%
  layer_smooths(stroke := "red", se=T) %>% 
  add_axis("x", title="Caffine Mgs", title_offset = 40) %>% 
  add_axis("y", title="Ratio of Miligrams per Ounce", title_offset = 40) 

caff %>%    #excluding higher outliers for scale
  filter(caff_mg < 400 & type == 'Tea' & ratio < 200) %>%
  ggvis(x= ~caff_mg, y= ~ratio) %>%
  layer_points( fill := "#0000CC") %>%
   layer_model_predictions(stroke := "red", model = "lm" , se=T) %>%
  add_axis("x", title="Caffine Mgs", title_offset = 40) %>% 
  add_axis("y", title="Ratio of Miligrams per Ounce", title_offset = 40) 

# That's enough to be clear that the relationship here is certain.
# As the amount of caffiene goes up so does the ratio of caffeine per ounce.
# Make sense right? The interesting part here is that since the ratio is correlated
# directly with caffeine ( as dervied variable of it), caffeiene content can roughly be
# predicted with ols for a few of the types.
# That being said, the above also reveals that not all of the relationships across types
# can be fitted with simple liner line... They are more complex and ols would not be the 
# best way to predit the outcome.

summary(lm( caff_mg ~ log(ratio), data=subset(caff, type == "Tea")))
summary(lm( caff_mg ~ log(ratio), data=subset(caff, type == "Soda" & ratio != 0)))
summary(lm( caff_mg ~ log(ratio), data=subset(caff, type == "Shots")))


#-----------------------------------------------------------------

# Classify the Drinks by caffine amount

cor(caff[3:5])

dedup = caff[!duplicated(caff$drink), ]

c_scl = as.data.frame(scale(dedup[3:4])) # ratio is intercorrelate and derived

row.names(c_scl) = dedup$drink

e_d =dist(c_scl, method="euclidean")

fit <- hclust(e_d, method="ward.D")
plot(fit)

#http://gastonsanchez.com/blog/how-to/2012/10/03/Dendrograms.html

#caffiene

summary(caffmg)

drinks %>% ggvis(x = ~caffmg , fill := "red") %>%
  layer_histograms(width=5) %>%
  add_axis("x", title="Caffiene in Mg", title_offset = 40) %>% 
  add_axis("y", title="Counts", title_offset = 40) 

# holy outlier batman

drinks %>% select(bev_name, caffmg) %>% arrange(desc(caffmg)) %>%
  head()
# and the winner is Starbucks

# oppiste spectrum
drinks %>% select(bev_name, caffmg) %>% arrange(desc(caffmg)) %>%
  tail()

# serving size is a discrete measuare

drinks %>% 
  filter(servsize %in% c(4, 6, 11, 13)) %>%
  ggvis(x = ~factor(servsize), y = ~caffmg , fill = ~factor(servsize)
        , fillOpacity := 0.5) %>%
  layer_boxplots(width=0.5) %>% 
  add_axis("x", title="Serving Size", title_offset = 40) %>% 
  add_axis("y", title="Caffine Mgs", title_offset = 40)  %>%
  add_legend("fill" , title="Selected Serving Size")

# one more way to look at this...

drinks %>%
  group_by(servsize) %>%
  summarize( avg_caff = mean(caffmg), counts = n(), ttl_caf = sum(caffmg)) %>% 
  mutate( ttl_mgoz = ttl_caf/counts, scale = counts *30 ) %>% #blowing it out for chart
  ggvis(~servsize, ~avg_caff, fill = ~ttl_mgoz ) %>% 
  layer_points(  size := ~scale  ) %>%
  add_axis("x", title="Serving Size (Oz)", title_offset = 50) %>% 
  add_axis("y", title="Average Caffine Mg", title_offset = 50) 

#-----------------------------------------------------------------

# Classify the Drinks by caffine amount
drinks2 = drinks
row.names(drinks2) <- drinks2$bev_name
drinks2$bev_name <- NULL
drinks2$mgoz <- NULL  # weak correlation present

clus = scale(drinks)
e_d =dist(drinks, method="euclidean")
fit <- hclust(e_d, method="ward.D")
plot(fit)










