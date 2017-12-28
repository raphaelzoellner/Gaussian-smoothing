#Block 2 
#Einlesen der Daten
vals <- scan(file = "http://www.math.hu-berlin.de/~bibinger/Praktikum%202012/snowfall.txt", what = double(), nmax = 63)

#Plot der empirischen Verteilungsfunktion
plot(ecdf(vals), main="ECDF of snowfall in Buffalo", sub="Winters from 1910 to 1973", xlab="Snow in inch")

windows(8,6, rescale="fixed")

#Vermutung: multimodal 3Moden, nahezu symmetrisch um 80 bzw. leicht linksschief
hi <- hist(vals, breaks = 11, plot=FALSE)

#Plot Histogramm, Epanechnikovschaetzer mit 3 Bandbreiten
jpeg(filename='output/snowfall_epach.jpg', width=640, height=480, units = "px", type = c("windows", "cairo"), quality=100)
hist(vals, breaks = 11, freq = FALSE, col = "grey", main='Histogram of snowfall', sub='Epanechnikovestimator', ylim=c(0,0.022), xlim=c(10, 140))
lines(density(vals, bw=3, kernel = "epanechnikov"), col = "red", lw=2)
lines(density(vals, bw=5, kernel = "epanechnikov"), col = "blue", lw=2)
lines(density(vals, bw="nrd0", kernel = "epanechnikov"), col = "green", lw=2)
legend(18, 0.021, legend=c("Standard", "bw = 5", "bw = 3"), col=c("green", "blue", "red"), lwd=3)
dev.off()

#Plot Histogramm, Gaussianschaetzer mit 3 Bandbreiten
jpeg(filename='output/snowfall_gausian.jpg', width=640, height=480, units = "px", type = c("windows", "cairo"), quality=100)
hist(vals, breaks = 11, freq = FALSE, col = "grey", main='Histogram of snowfall', sub='Gausianestimator', ylim=c(0,0.022), xlim=c(10, 140))
lines(density(vals, bw="nrd0", kernel = "gaussian"), col = "green", lw=2)
lines(density(vals, bw=5, kernel = "gaussian"), col = "blue", lw=2)
lines(density(vals, bw=3, kernel = "gaussian"), col = "red", lw=2)
legend(18, 0.021, legend=c("Standard", "bw = 5", "bw = 3"), col=c("green", "blue", "red"), lwd=3)
dev.off()

#Test auf Normalverteilung mit Shapiro-Wilk Test
shapiro.test(vals)

#Read noisysignal
signal <- scan('http://www.math.hu-berlin.de/~bibinger/Praktikum%202012/noisysignal.txt')

#Funktion zum Gl?tten eines Vektors mit der Methode der n?chsten 2k-Nachbarn
smooth <- function(k, vals){
  newVals <- numeric(0)
  for(i in 1:k){
    newVals[i] <- mean(vals[0:(i+k)])
    newVals[length(vals)-(i-1)] <- mean(vals[(length(vals)-(i-1)-k):length(vals)])
  }
  for(i in (k+1):(length(vals)-k)){
    newVals[i] <- mean(vals[(i-k):(i+k)])
  }
  newVals
}

#Plotten des Signals
plot(signal, main ="Noisy Signal")
#Plotten des geglätteten Signals
plot(smooth(5, signal), main ="Smoothed Signal")


#Einbinden der library bmp
#install.packages("bmp")
library(bmp)
#install.packages("pixmap")
library(pixmap)
#install.packages("adimpro")
library(adimpro)

#Read noisyimage
prc <- pixmapGrey(read.bmp("noisyimage.bmp"))
imageMatrix <- prc@grey
image <- make.image(imageMatrix)
show.image(image)

#Funktion zum Glaetten einer Matrix mit der Methode der naechsten Nachbarn im Umkreis mit Radius k
smooth2 <- function(vals,k){
  #uniCircle <- c(0,0)
  #umkreisj <- numeric(1)
  uniI <- 0
  uniJ <- 0
  for(i in -k:k){
    for(j in -k:k){
      len <- norm(c(i,j), type="2")# alternativnorm: max(abs(i),abs(j))
      if(len <= k && len > 0)
      {
        #uniCircle <- cbind(uniCircle, c(i,j))
        #umkreisj <- c(umkreisj,j)
        uniI <- c(uniI, i)
        uniJ <- c(uniJ, j)
      }
    }
  }
  
  newVals <- matrix(nrow = dim(vals)[1], ncol = dim(vals)[2])
  
  for(i in 1:dim(vals)[1])
  {
    for(j in 1:dim(vals)[2])
    {
      circleI <- uniI+i
      circleJ <- uniJ+j
      list <- numeric(0)
      
      for(k in 1:length(circleI))
      {
        if(circleI[k]>0 && circleJ[k]>0 && circleI[k]<=dim(vals)[1] && circleJ[k]<=dim(vals)[2]){
          list <- c(list,vals[circleI[k],circleJ[k]])
        }
      }
      newVals[i,j] <- mean(list)
    }
  }
  newVals
}
newMatrix <- smooth2(imageMatrix,5)
newimg <- make.image(newMatrix)
show.image(newimg)