###################################################
#Animation Dollar and Euro Signs
##################################################
#http://stackoverflow.com/questions/19234796/how-
#do-we-plot-images-at-given-coordinates-in-r
##################################################

library(animation)
library(maps)
library(png)

par(mfrow=c(1,1))

#map("world")

img = readPNG("DollarTransB.png")
img2 = readPNG("EuroTransB.png")

resize = function(x, y, images, 
                  width = 0.1*diff(range(x)), 
                  height = 0.1*diff(range(y))){
  images = replicate(length(x), images, simplify=FALSE)
  stopifnot(length(x) == length(y))
  
  for (j in seq_along(x)){
    rasterImage(images[[j]], xleft=x[j] - 0.7*width,
                ybottom= y[j] - 0.7*height,
                xright=x[j] + 0.7*width, 
                ytop= y[j] + 0.7*height, 
                interpolate=FALSE)
  }
}
#function replicates a picture and places them randomly on the
#screen
firstPic = function() {
  z  =runif(5, 0, 100)
  x  =x+z
  y  =y+z
  xy = data.frame(x, y)
  plot(xy, t="n",axes=FALSE,ann=FALSE)
  resize(xy[,1], xy[,2], img)
  
  op = par(new=T)
  xy2= data.frame(x2=runif(5, 0, 100), 
                  y2=runif(5, 0, 100))
  plot(xy2, t="n",axes=FALSE,ann=FALSE)
  resize(xy2[,1], xy2[,2], img2)
}

#function draws the world map and calls the first function 
secondPic= function() {
  lapply(seq(1, 100, by = 1), function(i) {
    map("world",col="orange",bg="black")
    op = par(new=T)
    firstPic()
    animation::ani.pause()
  })
}

dir= "C:/Users/Helen/Desktop/Final SSPL R Code"
#saving the sequence of pictures as a video
saveVideo(secondPic(), interval = 0.2, outdir = dir, 
          ffmpeg = "C:/ffmpeg/bin/ffmpeg.exe",
          video.name = "USD-EUR_Bewegung.mp4")
