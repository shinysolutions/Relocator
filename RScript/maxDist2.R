
for (i in 1:100) {
  print(i)
  pt <- Pt
  M1 <- matrix(NA, ncol = length(ID), nrow = N)
  for (m in 1:N) {
    M1[m,] <- sqrt((x - pt$x[m])^2 + (y - pt$y[m])^2)
  }
  
  p_  <- sample(1:N, 1)
  M2 <- sapply(1:length(ID), function(x) min(M1[-p_,x]))
  id <- ID[which.max(M2)] 
  
  pt[p_,] <- c(730 - id %% 730,  id %/% 730)
  
  Pt <- pt
  plot(pixmapRGB(img[,,4]))
  points(pt)
  points(pt[p_,], col = 'red')
  
  
}





M2b <- sapply(1:length(ID), function(x) min(M1[-pb,x]))
id0 <- ID[which.max(M2a)] 
idb <- ID[which.max(M2b)] 
da  <- max(M2a)
db  <- max(M2b)

if (da >= db) {
  p = pa
  id = id0
} else {
  p = pb
  id = idb
}

pt[p,] <- c(730 - id %% 730,  id %/% 730)

M3 <- matrix(9999, nrow = N, ncol = N)
for (m in 1:(N-1)) {
  pt <- pt[m, ]
  for (n in (m+1):N) {
    ptn <- pt[n, ]
    M3[n, m] <- sqrt((pt[1]-ptn[1])^2+(pt[2]-ptn[2])^2)
  }
}

dnew <- min(M3)
panew <- which.min(M3) %% N
pbnew <- ceiling(which.min(M3) / N)
if (panew == 0) panew = N

print(dnew)
if (dnew > d0) {
  d0 <- dnew
  pa <- panew
  pb <- pbnew
  pt0 <- pt
  plot(pixmapRGB(img[,,-4]))
  points(pt)
  points(pt[c(pa, pb),], col = 'red')
}