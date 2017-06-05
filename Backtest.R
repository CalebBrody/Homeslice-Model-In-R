Universe='San Francisco';

#Load data from csvs
Mortgage.Rate = read.csv("hsh.csv", header= FALSE)
Default.Rate= read.csv("fed_default.csv", header= TRUE)
Default.Rate[,2]=Default.Rate[,2]/100;
Price=list();
Zipcodes=list();
for (i in 1:5) {
  Price[[i]]=(data.frame(
    read.csv(paste(sep='', "Zip_Zhvi_",i,"bedroom.csv"), header= TRUE)
    ));
  names(Price[[i]]) = gsub('\\.','-',names(Price[[i]]) );
  Zipcodes[[i]]=which(Price[[1]]['Metro']==Universe);
}

#Get packages for calc
library(FinCal)
x11(width=8,height=4)

#Setup assumptions
GetReturns=function(
Down.Payment=.2,
Years.of.Mortgage=30,
Years.Remaining=25,
Equity.Percentage=.5,
Investor.Percentage=.5,
Refinance.Length=5,
Purchase.Discount=.3,
Percentage.of.Mortgage.Payment=.25,
Reclaim=.5,
net=TRUE){
  
  #We need to use data from today (when the house is sold) (Lag0) and 1 period ago (When the house was bought)
  Lag=4*(Years.of.Mortgage-Years.Remaining+Refinance.Length);
  Lag1=Default.Rate[0:(length(Default.Rate[,1])-Lag),1];
  Lag0=Default.Rate[(1+Lag):length(Default.Rate[,1]),1];
  
  
  #assume the house cost $1 to appr will be easier to calc
  #We are at time of refinance so if 30 mort. was bought in w 25 years left 10 years ago we are at time 30-25+10
  Payment=  pmt(r=(Mortgage.Rate[Lag1,2]/12),n=(Years.of.Mortgage)*12, pv=(1-Down.Payment)*1, fv=0);
  Mortgage.val = -fv(r=Mortgage.Rate[Lag1,2]/12,n=(Years.of.Mortgage-Years.Remaining+Refinance.Length)*12,
                    pv=(1-Down.Payment)*1, pmt=Payment);
  
  #How much did the equity cost?
  Equity.cost = (1-fv(r=Mortgage.Rate[Lag1,2]/12,n=(Years.of.Mortgage-Years.Remaining)*12,
                     pv=(1-Down.Payment)*1, pmt=Payment)) *Investor.Percentage*(1-Purchase.Discount);
  
  Equity=c();
  for (Bedrooms in 1:5){
  Equity=cbind(Equity,t(
  Price[[Bedrooms]][paste(sep='','X',Lag0)][Zipcodes[[Bedrooms]],] /
    Price[[Bedrooms]][paste(sep='','X',Lag1)][Zipcodes[[Bedrooms]],] ));
  }
  rownames(Equity)=Lag1;
  
  Equity.net =  Equity *(1-Default.Rate[Lag0,2])^(Lag/4)+
    Equity *(1-(1-Default.Rate[Lag0,2])^(Lag/4))* Reclaim;
  
  #Use Equity.net for default
  Gross.Return=(Equity- Mortgage.val)*Investor.Percentage/Equity.cost ;
  if (net)
    Gross.Return=(Equity.net- Mortgage.val)*Investor.Percentage/Equity.cost ;
  
  
  Net.Return=(1+Gross.Return)^(1/Refinance.Length)-1 ;
  

return(Net.Return);
}

GraphReturn =function(net=FALSE){
  boxplot(t(GetReturns(net=net)*100), main="Return",  ylab="Return %", xlab="Date")
}

GraphTime=function(net=FALSE, Remaining=25){

  y=list();
  for (i in 4:29){
    y[[i]]=((rowMeans(GetReturns( Refinance.Length=i,net=net, Years.Remaining=Remaining-5+i)*100 ,TRUE))  );
  }
  boxplot((y), main="Refinance Length",  ylab="Avg Return %", xlab="Length")
  
}

GraphEquity=function(net=FALSE){
  
  y=list();
  for (i in 20:80){
    y[[i]]=((rowMeans(GetReturns( Equity.Percentage =i/100,net=net)*100 ,TRUE))  );
  }
  boxplot((y), main="",  ylab="Avg Return %", xlab="Percent Equity")
  
}

GraphDiscount=function(net=FALSE, ...){
  
  y=list();
  for (i in 20:80){
    y[[i]]=((rowMeans(GetReturns( Purchase.Discount  =i/100,net=net, ...)*100 ,TRUE))  );
  }
  boxplot((y), main="",  ylab="Avg Return %", xlab="Purchase Discount")
  
}

GraphReclaim=function(net=FALSE, ...){
  
  y=list();
  for (i in 20:80){
    y[[i]]=((rowMeans(GetReturns(Reclaim  =i/100,net=net, ...)*100 ,TRUE))  );
  }
  boxplot((y), main="",  ylab="Avg Return %", xlab="Reclaim Rate")
  lines((1:100-1:100));
  
}

GraphReclaim(TRUE)