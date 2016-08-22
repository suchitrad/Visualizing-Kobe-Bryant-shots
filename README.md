# R-Sample-Code

#Looping
#while loop
i <- 1
while (i <= 10) {
  print(3* i )
  if ( 3 * i %% 8 ==0  ) {
    print ( 3* i )
    break 
  }
  i <- i + 1
}

#for loop 
linkedin <- c(16, 9, 13, 5, 2, 17, 14)  # sample vector containing visits to a linkedin account over a week
for (li in linkedin) {
  if (li > 10) {
    print("You're popular!")
  } else {
    print("Be more visible!")
  }
  if( li > 16) {
      print( "Very very popular")
      break
  }
   if ( li < 5) {
       print("Really low visibility" )
       next 
   }
  print(li)
}

#Apply
#lapply with anonymous function
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")   #vector with name and year data
split <- strsplit(pioneers, split = ":")                                   # split the vector data into name and year 

names <- lapply(split , function(x){ x[1] })
years <- lapply(split , function(x) { x[2]})

#merging vectors into a list
v1 <- c( 3,7,9,6,-1)
v2 <- c( 6,9,12,13,5)
v3 <- c(4,8,3,-1,-3)
v4 <- c(1,4,7,2,-2) 
temp <- list( v1,v2,v3,v4)# a list containing temperatures of 4 weeks, a vector representing each week's temperatures

#sapply to find each week's minimum temp
sapply( temp, min)
#sapply to find each week's maximum temp
sapply(temp, max )

#vapply
#defining a function to find out min and max temperatures
basics <- function(x) {
  c(min = min(x), mean = mean(x), max = max(x))
}

#Applying basics() over temp 
vapply( temp, basics, numeric(3))

#reading a csv file
mydata = read.csv("data.csv") %>% filter(!is.na(shot_made_flag))  
str(mydata)

#visualizing the types of shots of Kobe Bryant during his career
install.packages("ggplot2")
library("ggplot2")
install.packages("knitr")
library("knitr")
install.packages("latticeExtra")
library("latticeExtra")
install.packages("dplyr")
library("dplyr")
install.packages("hexbin")
library("hexbin")
# barchart showing the number of shots of each shot type of kobe bryant

barchart(sort(table(mydata$combined_shot_type))
         , col = "lightsalmon4"
         , border = "transparent"
         , xlim = c(0, 27000)
         , xlab = "Number of shots"
         , main = "Number of shots by shot type"
         , panel = function(...){
           panel.abline(v = seq(0, 26000, 1000), col = "gray90")
           args <- list(...)
           panel.text(
             args$x, args$y, paste0(args$x, " (", round(prop.table(args$x), 3)*100, "%)")
             , pos = 4)
           panel.barchart(...)})



#comparing the type of shots of kobe bryant during regular season and playoffs

comparision <- as.data.frame(xtabs(~ playoffs + combined_shot_type, mydata))
comparision $combined_shot_type <- factor(comparision $combined_shot_type, levels(comparision $combined_shot_type)[c(1, 3, 6, 2, 5, 4)])
levels(comparision $playoffs) <- c("Regular season", "Playoffs")

levels(comparision $playoffs) <- paste0(levels(comparision $playoffs), ": "
         , table(mydata$playoffs), " (", round(prop.table(table(mydata$playoffs)), 4)*100, "%)")

barchart(combined_shot_type ~ Freq | playoffs
         , col = "#0080ff"
         , border = "transparent"
         , scales = list(x = "free")
         , xlim = list(c(0, 27000), c(0, 4500))
         , strip = strip.custom(bg = "white")
         , xlab = "Number of shots"
         , main = "Number of shots, regular season vs playoffs"
         , comparision 
         , panel = function(...){
           args <- list(...)
           panel.text(
             args$x, args$y, paste0(args$x, " (", round(prop.table(args$x), 3)*100, "%)")
             , pos = 4)
           panel.barchart(...)})

# visualizing kobe bryant's hits vs misses data

hitvsmiss <- ggplot(data = mydata) +
  geom_hex(aes(x=loc_x, y=loc_y), binwidth = c(15,15)) +
  scale_fill_gradient(trans = "log", low = "darkorchid", high = "olivedrab1") +
  theme(legend.position="none") + 
  facet_wrap(~ shot_made_flag) +
  coord_fixed() + 
  ggtitle(paste("Misses vs Hits"))
  
# hitvsmiss graph
hitvsmiss

#visualizing the plot that maps position by feature

shotplot <- function(feat) {
        feat <- substitute(feat)
    mydata %>% 
    ggplot(aes(x = lon, y = lat)) +
        geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
        ylim(c(33.7, 34.0883)) +
        scale_color_brewer(palette = "Set1") +
        theme_void() +
        ggtitle(paste(feat))
}
#position by feature plot
shotplot (combined_shot_type) 

