install.packages("sp")
install.packages("rgdal")
install.packages("tmap")
install.packages("ggsoccer")
install.packages("adehabitatHR")

library(sp)
library(rgdal)
library(ggmap)
library(tmap)
library(adehabitatHR)
library(raster)
library

FootyGames <- FootyGames %>% group_by(gameID, byUs, playNumber) %>%
  mutate(maxSeq = max(sequenceNumber), 
         playByUs = first(byUs))
footy_pass <- FootyGames %>% filter(competition == "2019 Icc Football Mens Open" & 
                                 #   theirName == "Hornsby Heights" & 
                                      playByUs == "false" & #sequenceNumber == 1 &
                   adjEventName %in% c("first touch", "kickoff",
                                       "throw in", "goal kick", "keeper punt", 
                                       "offside restart"))
# points from scratch
coords = cbind(footy_pass[footy_pass$playNumber > 0, ]$lengthMetres, 
               footy_pass[footy_pass$playNumber > 0, ]$widthMetres)
sp = SpatialPoints(coords)
# make spatial data frame
spdf = SpatialPointsDataFrame(coords, footy_pass[footy_pass$playNumber > 0, ])

kde.output <- kernelUD(spdf, h="href", grid = 1000)
plot(kde.output)


spdf = SpatialPointsDataFrame(sp, footy_match[footy_match$playNumber > 0, ])
# promote data frame to spatial
coordinates(data) = cbind(x, y)
coordinates(data) = ~lon + lat
# back to data
as.data.frame(data)

plot(spdf)
spplot(spdf)

tm_shape(spdf) +
  tm_dots(col="adjEventName")


?kernelUD
#convert our kernel density map to a raster object
kde <- raster(kde.output)
# set the coordinate projection of our object to British National Grid (the one we have been using all along)
projection(kde) <- CRS("+init=EPSG:27700")
#map it!
tm_shape(kde) + tm_raster("ud")

masked_kde <- mask(kde, Output.Areas)
plot(masked_kde) # a quick sanity check!

coordinates(spdf)
proj4string(spdf)  

# Generate data
pp <- function (n,r=4) {
  x <- seq(-r*pi, r*pi, len=n)
  df <- expand.grid(x=x, y=x)
  df$r <- sqrt(df$x^2 + df$y^2)
  df$z <- cos(df$r^2)*exp(-df$r/6)
  df
}
p <- ggplot(pp(20), aes(x=x,y=y))

p + geom_tile() #pretty useless!

# Add aesthetic mappings
p + geom_tile(aes(fill=z))

# Change scale
p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="green", high="red")

# Use qplot instead
qplot(x, y, data=pp(20), geom="tile", fill=z)
qplot(x, y, data=pp(100), geom="tile", fill=z)

# Missing values
p <- ggplot(pp(20)[sample(20*20, size=200),], aes(x=x,y=y,fill=z))
p + geom_tile()

throws <- footy_match %>% filter(adjEventName=="throw in") %>%
  mutate(zone_x = trunc(lengthFraction*12)+1,
         zone_y = trunc(widthFraction*2)+1 )

ggplot(footy_match %>% filter(adjEventName=="throw in") %>%
  mutate(zone_x = trunc(lengthFraction*12)+1,
         zone_y = widthFraction ) %>%
  group_by(zone_x, zone_y, byUs) %>%
    summarise(ctr = n()), aes(x=zone_x, y=zone_y, fill= ctr)) +
  geom_tile() + facet_wrap(~byUs)

abc<-trunc(footy_match$lengthFraction*10)+1

trunc(3.5555, 2)
quartiles(c(1:100))
decile(c(1:100), decreasing = FALSE)  

weights <- c(69, 70, 75, 66, 83, 88, 66, 63, 61, 68, 73, 57, 52, 58, 77)
quantile(weights, prob = seq(0, 1, length = 11), type = 5)


library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 3.4.4
library(ggsoccer)

ggplot() +
  annotate_pitch() +
  theme_pitch()
pass_data <- data.frame(x = c(24, 18, 64, 78, 53),
                        y = c(43, 55, 88, 18, 44),
                        x2 = c(34, 44, 81, 85, 64),
                        y2 = c(40, 62, 89, 44, 28))

ggplot(pass_data) +
  annotate_pitch() +
  geom_segment(aes(x = x, y = y, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed")) +
  theme_pitch() +
  direction_label() +
  ggtitle("Simple passmap", 
          "ggsoccer example")


shots <- data.frame(x = c(90, 85, 82, 78, 83, 74, 94, 91),
                    y = c(43, 40, 52, 56, 44, 71, 60, 54))

pitch_opta
ggplot(shots) +
  annotate_pitch(colour = "white",
                 fill   = "chartreuse4",
                 limits = FALSE) +
  geom_point(aes(x = x, y = 100 - y),
             colour = "yellow", 
             size = 4) +
  theme_pitch() +
  theme(plot.background = element_rect(fill = "chartreuse4"),
        title = element_text(colour = "white")) +
 # coord_flip(xlim = c(0, 101),
#             ylim = c(-1, 101)) +
  ggtitle("Simple shotmap",
          "ggsoccer example")

pitch_custom <- list(
  length = 110,
  width = 75,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

ggplot() +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch()
