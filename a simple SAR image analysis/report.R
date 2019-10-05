library(png)
library(ggplot2)
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
b = b[102:151]
b = rev(b)
inputdata = data.frame(gray_value=seq(0,0.998,0.02),gray_frequency=b)

