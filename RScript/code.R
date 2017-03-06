library(png)
library(pixmap)
img <- readPNG("/home//tianhd//RColor//color_wheel_730.png")
ID <- which(img[,,4] == 1)
img[,,1][-ID]

img[,,1][which(img[,,4] != 1)] <- 1
img[,,2][which(img[,,4] != 1)] <- 1
img[,,3][which(img[,,4] != 1)] <- 1

plot(pixmapGrey(img[,,1]))
plot(pixmapGrey(img[,,2]))
plot(pixmapGrey(img[,,3]))
plot(pixmapGrey(img[,,4]))
plot(pixmapRGB(img[,,-4]))

writePNG(img, "/home//tianhd//RColor//color_wheel.png")
img <- readPNG("/home//tianhd//RColor//color_wheel.png")

axis(1)
axis(2)

dim(img)

id <- sample(ID, 10, replace=FALSE)
y <- 730 - id %% 730
x <- id %/% 730

plot(pixmapRGB(img[,,4]))
points(x, y, pch = 15, col = rgb(img[,,1][id], img[,,2][id], img[,,3][id]))
points(x, y,  col = 'red')

D <- dist(as.matrix(data.frame(x, y)))

image(img[,,1])
