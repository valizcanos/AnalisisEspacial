setwd("C:/Users/victor.lizcano/Downloads/MATERIAS/AnalisisEspacial/")

#####KERNEL DE DENSIDAD:

x=sample(1:10,15,replace = T)
y=sample(1:10,15,replace = T)
plot(x,y,col="red",pch=20, cex=1)

# Gaussiano:

K_Gauss = function(bandW, x_data, y_data, nrowOut, ncolOut){
  
  n = length(x_data)
  
  minX = min(x_data)
  maxX = max(x_data)
  minY = min(y_data)
  maxY = max(y_data)
  
  x_out= seq(minX,maxX,length.out= nrowOut)
  y_out= seq(minY,maxY,length.out= ncolOut)
  x_out_ = rep(x_out, ncolOut)
  y_out_ = sort(rep(y_out,nrowOut))
  data_out = cbind(x_out_,y_out_)
  densidades = matrix(NA,ncol=1,nrow=nrowOut*ncolOut)
  
  for(i in 1:nrow(densidades)){
    distancias= sqrt((data_out[i,1]-x_data)^2 + (data_out[i,2]-y_data)^2)
    u = distancias/bandW
    c1 = 1/sqrt(2*pi)
    kernel = sum(c1*exp(-0.5*u^2))
    densidades[i,1]= (1/(n*bandW^2)) * kernel
  }
  
  data_ouput = cbind(data_out,densidades)
  colnames(data_ouput) = c("x","y","f(x)")
  return(data_ouput)
}

probar_modelo_Gauss = K_Gauss(2.5, x, y, 30, 30)
probar_modelo_Gauss

plot(probar_modelo_Gauss[,1],probar_modelo_Gauss[,2],col="red",
     pch=20, cex=probar_modelo_Gauss[,3]*100, xlab="x",ylab="y")
par(new=T)
plot(x,y,col="blue",pch=20, cex=2)

#####MIREMOS LAS FUNCIONES EMPLEADAS EN ARCGIS

#https://pro.arcgis.com/es/pro-app/3.3/tool-reference/spatial-analyst/how-kernel-density-works.htm
