#Code the damn model

precip=rep(750/1000,50)

recharge_coeff=1

porosity=0.25

effort=0.4

aquifer_thickness=15 #meters

n = 0

x=1-(effort+n*(1-effort))
c=(effort+n*(1-effort))*aquifer_thickness


drawdown=vector()
drawdown[1]=0

storage=vector()
storage[1]=0

i=1

loss=vector()
recharge=vector()
pumping=vector()
pumping[1]=1.5

for (i in 1:length(precip)){
  recharge[i]=recharge_coeff*precip[i]
  if(i>1){
  pumping[i]=effort*porosity*(aquifer_thickness-drawdown[i])
  }
  loss[i]=n*(1-effort)*storage[i]
  
  storage[i+1]=x*storage[i]+(recharge_coeff*precip[i])
  pumping[i+1]=x*pumping[i]+(recharge_coeff*effort*precip[i])
  drawdown[i+1]=x*drawdown[i]-(recharge_coeff/effort)*(precip[i])+c
}


barplot(precip)
plot(-drawdown,type = 'l')
plot(pumping,type = 'l')
