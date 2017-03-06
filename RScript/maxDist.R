library(png)
library(pixmap)
img <- readPNG("/home/tianhd/RColor/color_wheel_730.png")

## Generate random color points;
ID <- sample(which(img[,,4] == 1), 10000, replace = FALSE)
IDx <- ceiling(ID/730)
IDy <- 730-(ID/730-floor(ID/730))*730; IDy[which(IDy == (730+1))] <- 1;
N <- 20
IDs <- sample(ID, N, replace=FALSE)
IDsx <- ceiling(IDs/730)
IDsy <- 730-(IDs/730-floor(IDs/730))*730; IDsy[which(IDsy == (730+1))] <- 1;
Pts  <- data.frame(x = IDsx, y = IDsy)

## Calculate the shortest points pt_rir;
M_IDs <- matrix(NA, nrow = N, ncol = N)
for (i in 1:N) {
  M_IDs[i,] <- (Pts$x[i]-Pts$x)^2+(Pts$y[i]-Pts$y)^2
}
M_IDs[upper.tri(M_IDs, diag = TRUE)] <- NA

Dist <- min(M_IDs, na.rm = TRUE)
IDsm <- which.min(M_IDs)
pt_r <- IDsm %% N; if (pt_r == 0) pt_r <- N
pt_c <- ceiling(IDsm/N)

## Plot
plot(pixmapRGB(img[,,-4]))
points(Pts)
points(Pts[pt_r,], col = "red", pch = 20, cex = 1.5)
points(Pts[pt_c,], col = "blue", pch = 20, cex = 1.5)


#########################
Do <- TRUE; 
I <- 0
while (Do) {
  I <- I + 1
  print(I)
  # point a;
  pts <- Pts
  ## Distance of all points to selected points;
  M_ID <- matrix(NA, ncol = length(ID), nrow = N)
  for (m in 1:N) {
    M_ID[m,] <- (IDx - pts$x[m])^2 + (IDy - pts$y[m])^2
  }
  V_ID <- sapply(1:length(ID), function(x) min(M_ID[-pt_r,x]))
  ID_pt <- ID[which.max(V_ID)]
  pt_x <- ceiling(ID_pt/730)
  pt_y <- 730-(ID_pt/730-floor(ID_pt/730))*730; pt_y[which(pt_y == (730+1))] <- 1;
  pts[pt_r,] <- c(pt_x,  pt_y)
  
  M_pts <- matrix(NA, nrow = N, ncol = N)
  for (i in 1:N) {
    M_pts[i,] <- (pts$x[i]-pts$x)^2+(pts$y[i]-pts$y)^2
  }
  M_pts[upper.tri(M_pts, diag = TRUE)] <- NA
  dist <- min(M_pts, na.rm = TRUE)

  print(dist)
  if (dist > Dist) {
    Pts <- pts
    Dist <- dist
    
    points(pt_x, pt_y, col = "white", pch = 20)
    ID_m <- which.min(M_pts)
    pt_r <- ID_m %% N; if (pt_r == 0) pt_r <- N
    pt_c <- ceiling(ID_m/N)

    plot(pixmapRGB(img[,,-4]))
    points(pts)
    points(pts[pt_r,], col = "red", pch = 20, cex = 1.5)
    points(pts[pt_c,], col = "blue", pch = 20, cex = 1.5)
  } else {
    Do <- FALSE
  }

}

plot(pixmapRGB(img[,,-4]))
points(pts)
points(pts[pt_r,], col = "red", pch = 20, cex = 1.5)
points(pts[pt_c,], col = "blue", pch = 20, cex = 1.5)
