bruto_neto<-function(x){
        if(x>=555 & 
           x<2555){ #kodel tax.lt puslapyje, kai bruto yra 2555 npd yra 0, o pvz kai 2554 tai npd=0.15???
                MMA<-555 
                NPD<-300-0.15*(x-MMA)
        }else if(x>=2555){
                NPD<-0
        }else{NPD<-300}
        
        
        VDU<-1283.2
        if (x>=VDU*10){
                GPM<-(10*VDU*0.2+(x-10*VDU)*0.27) 
        }else if(x<=10*VDU &&
                 x>300){
                GPM<-(x-NPD)*0.2
        }else{GPM<-0}
        
        PSD<-x*0.0698
        if(x<10*VDU){
                SOC<-x*0.1252
        }else{SOC<-10*VDU*0.1252}
        y<-x-GPM-PSD-SOC
        y
}

