geo_vis <- ga$getData(id
                      ,start.date=start_cur_yr
                      ,end.date=end_cur_yr
                      ,metrics = "ga:sessions"
                      ,dimensions = "ga:region,ga:metro,ga:latitude,ga:longitude"
                      ,sort = ""
                      ,filter="ga:country==United States"
                      ,batch =  TRUE)

geo_vis <- geo_vis[!geo_vis$region %in% c("Alaska", "(not set)", "Hawaii"), ]
geo_vis <- geo_vis[geo_vis$latitude > 1, ] 


#dev.off()
# http://zevross.com/blog/2014/07/16/mapping-in-r-using-the-ggplot2-package/
# https://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/

all_states <- map_data("state")
p <- ggplot()
p <- p + geom_polygon(data = all_states
                       ,aes(x = long, y = lat, group = group)
                       ,colour = "black"
                       ,fill = "#F9F9F6")

p + geom_point(data = geo_vis 
               ,aes(x = longitude
                   ,y = latitude
                   ,size = log(sessions))
                   ,colour = "#0099FF") + # grey20
  labs(x = NULL, y = NULL, title = NULL) +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1),
        legend.position = "") +   
  #scale_color_distiller(palette = "Blues", labels = geo_vis$sessions) +
  coord_equal(ratio = 1) 
 

plot(geo_vis$longitude, geo_vis$latitude)





