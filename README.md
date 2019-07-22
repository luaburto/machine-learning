# machine-learning
Code for introduction curse about Machine Learning
library("ggplot2")
library("GGally")
library("reshape", lib.loc="~/R/win-library/3.5")

rm(list=ls())
nweeks=1200
nprod=nrow(prod2)

elast=matrix(c(-2.93,0.15,0.18,0.17,0.38,-1.84,0.03,-0.01,0.89,0.32,-3.72,0.36,1.24,0.92,-0.13,-3.82), nrow=nprod, ncol=nprod)
runelast<-elast
elastsd=matrix(c(1,0.75,0.65,0.39,0.22,0.96,0.3,0.22,0.43,0.98,1.22,0.42,0.76,0.9,0.77,0.85), nrow=nprod, ncol=nprod)

nrow(prod2)
rm(el1)
el2=matrix(0,nrow(prod2),22)
i=2
demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0&STORE_NUM==tienda,list(WEEK_END_DATE,UNITS,FEATURE,DISPLAY,TPR_ONLY)]
datos=merge(demand,tlogprecio,by="WEEK_END_DATE")
datos=datos[,-1]
datos
model1=lm(log(UNITS)~.,data=datos)
summary(model1)
t(summary(model1)$coefficients[,2])
summary(model1)
el2=cbind(prod2[i,1],t(sqrt(diag(vcov(model1)))))
sqrt(diag(vcov(model1)))
rm(el1)




for (i in 1:nrow(prod2) ) {
  demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0&STORE_NUM==tienda,list(WEEK_END_DATE,UNITS,FEATURE,DISPLAY,TPR_ONLY)]
  datos=merge(demand,tlogprecio,by="WEEK_END_DATE")
  datos=datos[,-1]
  model1=lm(log(UNITS)~.,data=datos)
  el1[i,]=cbind(prod2[i,1],t(sqrt(diag(vcov(model1)))))
}


## MATRIZ DE DESV DE HBM ES betahbmsd

# 
# Prices=matrix(0,nrow=nweeks, ncol=nprod)
# for (i in 1:nprod ) {
#   Prices[,i]=round(rnorm(nweeks, mean=30*i, sd=3*i))
#   if (elast[i,i] > 0) elast[i,i] <- elast[i,i]*-1
#   }
# alpha=c(10.65,1.18,9.64,1.07)
# #alpha=matrix(rnorm(nweeks*nprod,mean=15,sd=3), nrow=nweeks, ncol=nprod)
# Sales=exp(matrix(rnorm(nweeks*nprod,mean=,sd=3), nrow=nweeks, ncol=nprod)+log(Prices)%*%elast)
# #demanda=cbind(sales,Prices)
# #library(gdata)
# #concat_data <- cbindX(sales, Prices)
# colnames(Prices)=c("Price1", "Price2", "Price3", "Price4")
# colnames(Sales)=c("Sales1", "Sales2", "Sales3", "sales4")
# #Estim_elast=solve(t(Prices)%*%Prices)%*%(t(Prices)%*%Sales)
# #aux=solve(t(Prices)%*%Prices)
# for (i in 1:nprod ) {
#   aux2=paste("fitmodel",i,sep="_")
#   assign(aux2,lm(Sales[,i]~Prices))
# 
# }


f_demand4 <- function(Precio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(log(Precio))))
  #c=as.matrix(elasticity3[,2:20])
  c=as.matrix(runelast[,2:20])
  demand <- sum((Precio-(pricelast[,2]/1.3))*(exp(c%*%b)))
  return(demand)
}
f_demand3(t(precioini))


runelast=matrix(0,nrow(betahbm),ncol(betahbmsd))
colnames(runelast)=colnames(betahbmsd)
for (i in 1:nrow(runelast) ) {
  for (j in 1:ncol(runelast))  {
    runelast[i,j]=rnorm(1, mean=betahbm[i,j], sd=0.1*as.numeric(betahbmsd[i,j]))
    
  }
}
runelast=data.frame(temp=0,runelast)

f_demand4 <- function(Precio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(log(Precio))))
  #c=as.matrix(elasticity3[,2:20])
  c=as.matrix(runelast[,2:20])
  demand <- sum((Precio-(pricelast[,2]/1.3))*(exp(c%*%b)))
  return(demand)
}


Pricemin=matrix(0,nruns,nprod+1)
colnames(Pricemin)=c(colnames(elasticity3)[6:20],"Profit")
Pricemin2<-Pricemin
nruns=50
Pricemin1=Pricemin2
Pricemin3=Pricemin2
runelast=matrix(0,nrow(betahbm),ncol(betahbmsd))

for (k in 1:nruns ) {

#creando instancia de matriz
  runelast=matrix(0,nrow(betahbm),ncol(betahbmsd))
  for (i in 1:nrow(runelast) ) {
    for (j in 1:ncol(runelast))  {
      #runelast[i,j]=rnorm(1, mean=betahbm[i,j], sd=betahbm[i,j]*0.1)
      runelast[i,j]=rnorm(1, mean=betahbm[i,j], sd=0.1*as.numeric(betahbmsd[i,j]))
      }
  }
  runelast=data.frame(temp=0,runelast)
  #caso 1 con rest laxas
  #Priceopt<-optim(t(precioini),f_demand3, method="SANN", control=list(maxit=100))
  #Priceopt<-optim(Priceopt$par,f_demand3, lower=precioini*0.01, upper=precioini*4,method="L-BFGS-B")
  #Priceopt <- constrOptim(t(tablaprecio[1,2:16]), f_demand4, NULL, ui=uiprom, ci=ciprom, control=list(fnscale=-1))
  Priceopt  <- f_optim2(uiprod,ciprod2,niter)
  Pricemin[k,c(1:nprod)]<-Priceopt$price
  Pricemin[k,(nprod+1)]= Priceopt$pi #prod-order
  # Precio=c(10,10,10,10)
  #caso 2 con rest mas apretadas 
  #Priceopt2<-optim(t(precioini),f_demand3, method="SANN", control=list(maxit=100))
  #Priceopt<-optim(Priceopt2$par,f_demand3, lower=precioini*0.8, upper=precioini*1.2,method="L-BFGS-B")
 
  #Priceopt<- constrOptim(precio3, f_demand4, NULL, ui=uiesc5, ci=ciesc5b, control=list(fnscale=-1))
  Priceopt<-optim(Priceopt2$par,f_demand4, lower=precioini*0.7, upper=precioini*1.3,method="L-BFGS-B",control=list(fnscale=-1))
  Pricemin1[k,c(1:nprod)]<-Priceopt$par
  Pricemin1[k,(nprod+1)]= Priceopt$value #initial price box
  
  Priceopt  <- f_optim2(uiesc3,ciesc3b,niter)
  Pricemin2[k,c(1:nprod)]<-Priceopt$price
  Pricemin2[k,(nprod+1)]= Priceopt$pi #all-order

  Priceopt  <- f_optim2(uiesc5,ciesc5b,niter)
  Pricemin3[k,c(1:nprod)]<-Priceopt$price
  Pricemin3[k,(nprod+1)]= Priceopt$pi #all-rules
    
}

#ggpairs(data.frame(Pricemin), title="Opt prices with 5%-95% constraints",upper = list(continuous = "density", combo = "box"))
#ggpairs(data.frame(Pricemin3), title="Opt prices with all constraints",upper = list(continuous = "density", combo = "box"))

p1min=data.frame(melt(Pricemin),type="prod0-100")
colnames(p1min)[1:3]=c("","Product","Price")
p2min=data.frame(melt(Pricemin1),type="initial-price-box")
#colnames(p2min)[1:16]=colnames(Pricemin)
colnames(p2min)[1:3]=c("","Product","Price")
p3min=data.frame(melt(Pricemin2),type="all-order")
#colnames(p2min)[1:16]=colnames(Pricemin)
colnames(p3min)[1:3]=c("","Product","Price")

p4min=data.frame(melt(Pricemin3),type="all-rules")
#colnames(p2min)[1:16]=colnames(Pricemin)
colnames(p4min)[1:3]=c("","Product","Price")


p5min=rbind.data.frame(p1min,p2min,p3min,p4min)
p5min=p5min[,-1]
q6min=p5min[p3min$Product=="Profit",]
q5min=p5min[p3min$Product!="Profit",]
ggplot(q5min, aes(x=Product, y=Price, fill=type)) +
  geom_boxplot()

p10 <- ggplot(q5min, aes(x = Product, y = Price, fill = type)) +
  geom_boxplot(alpha=0.7,outlier.size = NA,outlier.colour = NA) +
    scale_y_continuous(name = "Optimal prices") +
  scale_x_discrete(name = "Product") +
  ggtitle("Optimal pricing solutions using different set of constraints (hbm, 10%desvest)") +
  theme_bw() +
  theme(plot.title = element_text(size = 7, family = "Tahoma",hjust=0.5),
        text = element_text(size = 5, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 90,size = 5),
        legend.position = "bottom",
        legend.text=element_text(size=7)) +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Type of Constraints")
p10
# p11<-ggplot(p6min[p6min$type=="all-rules",], aes(x=Price, fill=type)) +
#   geom_density(alpha=0.4)# + xlim(0,10000)
# p11
# 
# p11<-ggplot(p6min, aes(x=Price, fill=type)) +
#   geom_density(alpha=0.4) + xlim(0,10000)
# p11

ylim1 = boxplot.stats(q6min$Price)$stats[c(1, 5)]

p11 <- ggplot(q6min, aes(x = Product, y = Price, fill = type)) +
  geom_boxplot(alpha=0.7,outlier.size = NA,outlier.colour = NA) + coord_cartesian(ylim = c(0,100)) +
    # scale_y_continuous(name = "Optimal prices") +
  # scale_x_discrete(name = "Product") +
  ggtitle("Optimal pricing solutions using different set of constraints (hbm, 10%desvest)") +
  theme_bw() +
  theme(plot.title = element_text(size = 7, family = "Tahoma",hjust=0.5),
        text = element_text(size = 5, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 90,size = 5),
        legend.position = "bottom",
        legend.text=element_text(size=7)) +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Type of Constraints") +
  geom_boxplot(outlier.size = NA,outlier.colour = NA)

p11

#ggpairs(iris, aes(colour = Species, alpha = 0.4))

################### optim example
#
# cambiando a 100% de desvest
#
# ##############

Pricemin=matrix(0,nruns,nprod+1)
colnames(Pricemin)=c(colnames(elasticity3)[6:20],"Profit")
Pricemin2<-Pricemin
nruns=50
niter=50
Pricemin1=Pricemin2
Pricemin3=Pricemin2
runelast=matrix(0,nrow(betahbm),ncol(betahbmsd))

for (k in 1:nruns ) {
  
  #creando instancia de matriz
  runelast=matrix(0,nrow(betahbm),ncol(betahbmsd))
  for (i in 1:nrow(runelast) ) {
    for (j in 1:ncol(runelast))  {
      #runelast[i,j]=rnorm(1, mean=betahbm[i,j], sd=betahbm[i,j]*0.1)
      runelast[i,j]=rnorm(1, mean=betahbm[i,j], sd=as.numeric(betahbmsd[i,j]))
    }
  }
  runelast=data.frame(temp=0,runelast)
  #caso 1 con rest laxas
  #Priceopt<-optim(t(precioini),f_demand3, method="SANN", control=list(maxit=100))
  #Priceopt<-optim(Priceopt$par,f_demand3, lower=precioini*0.01, upper=precioini*4,method="L-BFGS-B")
  #Priceopt <- constrOptim(t(tablaprecio[1,2:16]), f_demand4, NULL, ui=uiprom, ci=ciprom, control=list(fnscale=-1))
  Priceopt  <- f_optim2(uiprod,ciprod2,niter)
  Pricemin[k,c(1:nprod)]<-Priceopt$price
  Pricemin[k,(nprod+1)]= Priceopt$pi #prod-order
  # Precio=c(10,10,10,10)
  #caso 2 con rest mas apretadas 
  #Priceopt2<-optim(t(precioini),f_demand3, method="SANN", control=list(maxit=100))
  #Priceopt<-optim(Priceopt2$par,f_demand3, lower=precioini*0.8, upper=precioini*1.2,method="L-BFGS-B")
  
  #Priceopt<- constrOptim(precio3, f_demand4, NULL, ui=uiesc5, ci=ciesc5b, control=list(fnscale=-1))
  Priceopt<-optim(Priceopt2$par,f_demand4, lower=precioini*0.7, upper=precioini*1.3,method="L-BFGS-B",control=list(fnscale=-1))
  Pricemin1[k,c(1:nprod)]<-Priceopt$par
  Pricemin1[k,(nprod+1)]= Priceopt$value #initial price box
  
  Priceopt  <- f_optim2(uiesc3,ciesc3b,niter)
  Pricemin2[k,c(1:nprod)]<-Priceopt$price
  Pricemin2[k,(nprod+1)]= Priceopt$pi #all-order
  
  Priceopt  <- f_optim2(uiesc5,ciesc5b,niter)
  Pricemin3[k,c(1:nprod)]<-Priceopt$price
  Pricemin3[k,(nprod+1)]= Priceopt$pi #all-rules
  
}

#ggpairs(data.frame(Pricemin), title="Opt prices with 5%-95% constraints",upper = list(continuous = "density", combo = "box"))
#ggpairs(data.frame(Pricemin3), title="Opt prices with all constraints",upper = list(continuous = "density", combo = "box"))


p1min=data.frame(melt(Pricemin),type="prod0-100")
colnames(p1min)[1:3]=c("","Product","Price")
p2min=data.frame(melt(Pricemin1),type="initial-price-box")
#colnames(p2min)[1:16]=colnames(Pricemin)
colnames(p2min)[1:3]=c("","Product","Price")
p3min=data.frame(melt(Pricemin2),type="all-order")
#colnames(p2min)[1:16]=colnames(Pricemin)
colnames(p3min)[1:3]=c("","Product","Price")

p4min=data.frame(melt(Pricemin3),type="all-rules")
#colnames(p2min)[1:16]=colnames(Pricemin)
colnames(p4min)[1:3]=c("","Product","Price")


p5min=rbind.data.frame(p1min,p2min,p3min,p4min)
p5min=p5min[,-1]
r6min=p5min[p3min$Product=="Profit",]
r5min=p5min[p3min$Product!="Profit",]
ggplot(p5min, aes(x=Product, y=Price, fill=type)) +
  geom_boxplot()

p10 <- ggplot(r5min, aes(x = Product, y = Price, fill = type)) +
  geom_boxplot(alpha=0.7,outlier.size = NA,outlier.colour = NA)  +
  scale_y_continuous(name = "Optimal prices") +
  scale_x_discrete(name = "Product") +
  ggtitle("Optimal pricing solutions using different set of constraints (hbm, desvest)") +
  theme_bw() +
  theme(plot.title = element_text(size = 7, family = "Tahoma",hjust=0.5),
        text = element_text(size = 5, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 90,size = 5),
        legend.position = "bottom",
        legend.text=element_text(size=7)) +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Type of Constraints")
p10
# p11<-ggplot(p6min[p6min$type=="all-rules",], aes(x=Price, fill=type)) +
#   geom_density(alpha=0.4)# + xlim(0,10000)
# p11
# 
# p11<-ggplot(p6min, aes(x=Price, fill=type)) +
#   geom_density(alpha=0.4) + xlim(0,10000)
# p11

ylim1 = boxplot.stats(r6min$Price)$stats[c(1, 5)]

p11 <- ggplot(r6min, aes(x = Product, y = Price, fill = type)) +
  geom_boxplot(alpha=0.7,outlier.size = NA,outlier.colour = NA) + coord_cartesian(ylim = c(0,100000000)) +
  # scale_y_continuous(name = "Optimal prices") +
  # scale_x_discrete(name = "Product") +
  ggtitle("Optimal pricing solutions using different set of constraints (hbm, desvest)") +
  theme_bw() +
  theme(plot.title = element_text(size = 7, family = "Tahoma",hjust=0.5),
        text = element_text(size = 5, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 90,size = 5),
        legend.position = "bottom",
        legend.text=element_text(size=7)) +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Type of Constraints") +
  geom_boxplot(outlier.size = NA,outlier.colour = NA)

p11
