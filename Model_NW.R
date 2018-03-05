#Code the damn model

input=read.csv('precip_all.csv')

input2=subset(input,input$STNM=='Punjab')

input3=as.character(input2$ANN)
input3=as.numeric(input3)

precip=input3/10000

precip=precip[1:50]

#precip=rep(750/1000,50)

recharge_coeff=1

porosity=0.5

effort=0.02

aquifer_thickness=100 #meters

n = 0

x=1-(effort+n*(1-effort))
c=(effort+n*(1-effort))*aquifer_thickness


drawdown=vector()
drawdown[1]=0

storage=vector()
storage[1]=porosity*(aquifer_thickness)

i=1

loss=vector()
recharge=vector()

pumping=vector()
pumping[1]=1

for (i in 1:length(precip)){
  recharge[i]=recharge_coeff*precip[i]
  if(i>1){
    pumping[i]=effort*porosity*(aquifer_thickness-drawdown[i])
  }
  loss[i]=n*(1-effort)*storage[i]
  
  storage[i+1]=x*storage[i]+(recharge_coeff*precip[i])
  pumping[i+1]=x*pumping[i]+(recharge_coeff*effort*precip[i])
  drawdown[i+1]=x*drawdown[i]-(recharge_coeff/porosity)*(precip[i])+c
}


barplot(precip)
plot(-drawdown,type = 'l')
plot(pumping,type = 'l')
