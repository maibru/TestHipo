dif.medias<-function(mu1=4700,mu2=4900,sig=100,n1=4,n2=4){
  
  ## Obtenemos una muestra de tamaño n1 de la normal con media mu1
  ## y desviación típica sig
  
  y1<-rnorm(n=n1,mean=mu1,sd=sig)
  
  ## calculamos la media muestral de y1
  m1<-mean(y1)
  ## calculamos la varinza muestral de y1
  v1<-var(y1)
  
  ## Obtenemos una muestra de tamaño n2 de la normal con media mu2
  ## y desviación típica sig
  
  y2<-rnorm(n=n2,mean=mu2,sd=sig)
  
  ## calculamos la media muestral de y2
  m2<-mean(y2)
  ## calculamos la varinza muestral de y2
  v2<-var(y2)
  
  ## calculamos las diferencias de las dos medias muestrales
  dif.med<-m1-m2
  ## calculamos la varianza agregada
  s2p<-((n1-1)*v1+(n2-1)*v2)/(n1+n2-2)
  ## Error estandar de la diferencias de medias estimadas
  se.dif<-sqrt(s2p*((1/n1)+(1/n2)))
  ## T test para compara medias
  res<-t.test(y1,y2,var.equal=TRUE,alternative="two.sided")
  
  
  ## limite inferior del IC
  li<-res$conf.int[1]
  ## limite superior del IC
  ls<-res$conf.int[2]
  ## salida de la funcion. Lista con la diferencia y con los
  ## limites del IC
  sal<-c(stat.T=res$statistic,li.Inf=li,li.Sup=ls)
  return(sal)
}
