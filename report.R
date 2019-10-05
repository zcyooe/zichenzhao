library(png)
library(ggplot2)
library(png)
library(ggplot2)
fa<-function(z,sigema2)
{
  return((1/sigema2)*exp(-1*z/sigema2))
}
ggplot(data=data.frame(z=seq(0, 7, length.out = 500)), aes(x=z)) + scale_y_log10()+
  stat_function(fun=fa, geom = "line", size=2, col="black", args = (mean=1)) +
  stat_function(fun=fa, geom = "line", size=2, col="red", args = (mean=.5)) +
  stat_function(fun=fa, geom = "line", size=2, col="blue", args = (mean=2))

f2<-function(z,L,sigem2)
{
  
  return(L^L*z^(L-1)*exp(-1*L*z/sigem2)/(gamma(L)*sigem2^L))
}
ggplot(data=data.frame(z=seq(0, 7, length.out = 500)), aes(x=z)) +  scale_y_log10()+
  stat_function(fun=f2, geom = "line", size=2, col="black", args = list(L=1,sigem2=1)) +
  stat_function(fun=f2, geom = "line", size=2, col="red", args = list(L=3,sigem2=1)) +
  stat_function(fun=f2, geom = "line", size=2, col="blue", args = list(L=8,sigem2=1))

dKI <- function(z, p_alpha, p_lambda, p_Looks, log=FALSE) {
  
  if(log==FALSE) {
    
    lLz <- p_lambda * p_Looks* z
    
    return((2*p_lambda*p_Looks/(gamma(p_alpha)*gamma(p_Looks))) *
             (lLz)^((p_alpha+p_Looks)/2-1) *
             besselK(x = 2*sqrt(lLz), nu = p_alpha-p_Looks)
    )
  }
  
}

ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) + scale_y_log10()+
  stat_function(fun=dKI, geom = "line", size=2, col="red", args = list(p_alpha=1, p_lambda=1, p_Looks=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="blue", args = list(p_alpha=3, p_lambda=3, p_Looks=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="black", args = list(p_alpha=8, p_lambda=8, p_Looks=1))

fz <-function(z,a,r,L)
{
  L^L*gamma(L-a)*z^(L-1)/(r^a*gamma(L)*gamma(-a)*(r+L*z)^(L-a))
}
ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) + 
  stat_function(fun=fz, geom = "line", size=2, col="red", args = list(a=-1.5, r=0.5, L=1)) +
  stat_function(fun=fz, geom = "line", size=2, col="blue", args = list(a=-3, r=1, L=1)) +
  stat_function(fun=fz, geom = "line", size=2, col="black", args = list(a=-8, r=7, L=1))




img = readPNG('C:/Users/asus/Desktop/test_image.png')
gray<-function(img)
{
  r = 0.299
  g = 0.587
  b = 0.114
  R = img[,,1]
  G = img[,,2]
  B = img[,,3]
  gray_image = r*R+g*G+b*B
  return (gray_image)
}
gray_image = gray(img)
a = hist(gray_image, breaks = seq(0,1,0.01), freq = F)
b = matrix(unlist(a))
b = b[103:152]
b = rev(b)
inputdata = data.frame(gray_value=seq(0,0.998,0.02),gray_frequency=b)
ggplot(data=inputdata,aes(x=gray_value,y=as.numeric(gray_frequency)/16)) + 
  geom_bar(stat="identity") +
  stat_function(fun=dgamma, geom = "line", size=2, col="black", args = list(shape=4, scale=1/12)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="red", args = list(shape=4, scale=1/11)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="blue", args = list(shape=4, scale=1/10)) +
  xlab("Grey Value") + ylab("Gray Histogram and Gamma Densities")
