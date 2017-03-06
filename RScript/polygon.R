r1 <- input$R[1]
r2 <- input$R[2]
a1 <- input$A[1]
a2 <- input$R[2]

p1_x <- 365.5 + r1 * cos(a1/180*pi)
p2_x <- 365.5 + r2 * cos(a1/180*pi)
p3_x <- 365.5 + r2 * cos(a2/180*pi)
p4_x <- 365.5 + r1 * cos(a2/180*pi)

p1_y <- 365.5 + r1 * sin(a1/180*pi)
p2_y <- 365.5 + r2 * sin(a1/180*pi)
p3_y <- 365.5 + r2 * sin(a2/180*pi)
p4_y <- 365.5 + r1 * sin(a2/180*pi)

R1_x <- 365.5 + r2 * cos(seq(a1, a2)/180*pi)
R1_y <- 365.5 + r2 * sin(seq(a1, a2)/180*pi)

R2_x <- 365.5 + r1 * cos(seq(a2, a1)/180*pi)
R2_y <- 365.5 + r1 * sin(seq(a2, a1)/180*pi)

Pyx <- c(p1_x, p2_x, R1_x, p3_x, p4_x, R2_x)
Pyy <- c(p1_y, p2_y, R1_y, p3_y, p4_y, R2_y)

polygon(x = Pyx, y = Pyy)

img <- readPNG("Data/color_wheel.png")
plot(pixmapRGB(img[,,-4]))
img4 <- img[,,4]
img4[seq(1, 730, 5), seq(1, 730, 5)] <- 2
ID <- which(img4 == 2 & img[,,4] == 1)
length(ID)

IDx <- ceiling(ID/730)
IDy <- 730-(ID/730-floor(ID/730))*730; IDy[which(IDy == (730+1))] <- 1;
points(IDx, IDy, pch = 15, cex = .2)

dat <- point.in.polygon(IDx, IDy, Pyx, Pyy)
ID <- ID[point.in.polygon(IDx, IDy, Pyx, Pyy) == 1]


