## Load libraries;
library(sp)
library(png)
library(shiny)
library(pixmap)

shinyServer(function(input, output, session) {
  img <- readPNG("../Data/color_wheel.png")
  img4 <- img[,,4]
  img4[seq(1, 730, 5), seq(1, 730, 5)] <- 2
  ID0 <- which(img4 == 2 & img[,,4] == 1)
  p <- reactiveValues()
  
  output$pt <- renderPlot({
    ## Polygon;
    r1 <- input$R[1]
    r2 <- input$R[2]
    a1 <- input$A[1]
    a2 <- input$A[2]
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
    ## Points inside of the polygon;
    IDx <- ceiling(ID0/730)
    IDy <- 730-(ID0/730-floor(ID0/730))*730; IDy[which(IDy == (730+1))] <- 1;
    ID <- ID0[point.in.polygon(IDx, IDy, Pyx, Pyy) == 1]
    IDx <- ceiling(ID/730)
    IDy <- 730-(ID/730-floor(ID/730))*730; IDy[which(IDy == (730+1))] <- 1;
    ## Potential color number;
    
    ## Generate random color points;
    N <- input$N
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
    IDsm <- which(M_IDs == Dist)
    pt_r <- IDsm %% N; 
    pt_r[which(pt_r == 0)] <- N
    pt_c <- ceiling(IDsm/N)

    #########################
    Do <- TRUE; 
    I <- 0
    while (Do) {
      I <- I + 1; if (I > 200) Do <- FALSE
      # point a;
      pts <- Pts
      ## Distance of all points to selected points;
      M_ID <- matrix(NA, ncol = length(ID), nrow = N)
      for (m in 1:N) {
        M_ID[m,] <- (IDx - pts$x[m])^2 + (IDy - pts$y[m])^2
      }
      V_ID <- sapply(1:length(ID), function(x) min(M_ID[-pt_r[1],x]))
      ID_pt <- ID[which.max(V_ID)]
      pt_x <- ceiling(ID_pt/730)
      pt_y <- 730-(ID_pt/730-floor(ID_pt/730))*730; 
      pt_y[which(pt_y == (730+1))] <- 1;
      pts[pt_r[1],] <- c(pt_x,  pt_y)
      
      M_pts <- matrix(NA, nrow = N, ncol = N)
      for (i in 1:N) {
        M_pts[i,] <- (pts$x[i]-pts$x)^2+(pts$y[i]-pts$y)^2
      }
      M_pts[upper.tri(M_pts, diag = TRUE)] <- NA
      dist <- min(M_pts, na.rm = TRUE)
      ID_m <- which(M_pts == dist)
      

      if (length(IDsm) > 1 & length(IDsm) != length(ID_m)) {
        Pts <- pts
        Dist <- dist
        IDsm <- ID_m
        pt_r <- ID_m %% N; 
        pt_r[which(pt_r == 0)] <- N
        pt_c <- ceiling(ID_m/N)
      } else {
        if (dist > Dist) {
          Pts <- pts
          Dist <- dist
          IDsm <- ID_m
          pt_r <- ID_m %% N; 
          pt_r[which(pt_r == 0)] <- N
          pt_c <- ceiling(ID_m/N)
        } else {
          Do <- FALSE
        }
      }
    }
    
    plot(pixmapRGB(img[,,-4]))
    polygon(x = Pyx, y = Pyy)
    points(Pts)
    
    ID_col <- (730-Pts$y+1) + 730 * (Pts$x - 1)
    Pts$color <- rgb(img[,,1][ID_col], img[,,2][ID_col], img[,,3][ID_col])
    p$tab <- Pts
    
    ID_colx <- ceiling(ID_col/730)
    ID_coly <- 730-(ID_col/730-floor(ID_col/730))*730; ID_coly[which(ID_coly == (730+1))] <- 1;
    col <- rgb2hsv (img[,,1][ID_col], img[,,2][ID_col], img[,,3][ID_col])
    col[3,] <- 1- (sqrt((ID_colx - 365.5)^2 + (ID_coly - 365.5)^2 )/366)^4/4
    Pts$color <- hsv(h = col[1,], s = col[2,], v = col[3,], alpha = 1)
    p$tabDim <- Pts
    
  }, height = 730)
  
  ## Color output;
  output$tab <- renderTable({
    if (input$dim) {
      dat <- p$tabDim
    } else {
      dat <- p$tab
    }
    dat
  })

  output$txt <- renderText({
    if (input$dim) {
      dat <- p$tabDim
    } else {
      dat <- p$tab
    }
    paste('c("', paste(dat$color, collapse  = '", "'), '")', sep = '')

  })
  
  output$plot <- renderPlot({
    if (input$dim) {
      dat <- p$tabDim
    } else {
      dat <- p$tab
    }
    par(mar = c(5, 4,  5, 4))
    plot(dat$x, dat$y, col = as.character(dat$color), 
         xlim = c(0, 730), ylim = c(0, 730), pch = 19, cex = 5)
    text(dat$x, dat$y, labels = 1:nrow(dat))
  }, height = 730)
  
}) 
