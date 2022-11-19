
inventory1=function()
{
  cat("============================================================\n")
  cat("PROGRAM SIMULASI INVENTORY MULTI PERIOD LEAD-TIME : 2 PERIOD\n")
  cat("---------------------------------------------------\n")
  cat("\nDisusun Oleh \t: \n")
  cat("\n")

  cat("Salsabylla Nada Apsariny	\t(081711833027)\n")
 
  cat("\n")
  cat("Dosen Pengampu\t:\n")
  cat("  Dr. Toha Saifudin S.Si., M.Si.\n")
  cat("-----------------------------------------------------------\n")
  cat("\tProgram Studi S1-Statistika\n") 
  cat("\t   Universitas Airlangga	   \n")
  cat("\t   Tahun Ajaran 2020/2021	   \n")
  cat("===========================================================\n")
  cat("\n")
  
  cat("Deskripsi Masalah : \n")
  cat("1. Lead time diatur = 2 periode \n")
  cat("2. Barang dibayar 30% saat pesan, sisanya dibayarkan saat barang datang \n")
  cat("3. Hitung rata-rata profit per periode. \n")
  cat("\n")
  
  #------------------Inputan Data Sesuai Informasi Soal-------------------------
  
  cat("Informasi Data Yang Di Inputkan :\n")
  cat("---------------------------------\n")
  rp=as.numeric(readline("  Reorder Point\t\t= "))
  oq=as.numeric(readline("  Order Quantity\t= "))
  cat("\n")
  
  cat("DEMAND per period ~ NORMAL with :\n")
  cat("---------------------------------\n")
  mean=as.numeric(readline("  Mean\t\t\t= "))
  sd=as.numeric(readline("  Simpangan Baku\t= "))
  
  cat("\nCOSTs\n")
  cat("---------------------------------\n")
  DP=as.numeric(readline("  Uang Muka\t\t= "))			#input persenan DP dalam desimal
  pc=as.numeric(readline("  Purchasing Cost\t= "))
  sp=as.numeric(readline("  Selling Price\t\t= "))
  hc=as.numeric(readline("  Holding Cost\t\t= "))
  oc=as.numeric(readline("  Order Cost\t\t= "))
  sc=as.numeric(readline("  Shortage Cost\t\t= "))
  cat("\n")
  BI=as.numeric(readline("\nBeginning Inventory = "))
  cat("\n")
  n=as.numeric(readline("Anda ingin mensimulasikan untuk berapa minggu? "))
  
  set.seed(n)
  Dmd<-round(rnorm(n,mean,sd),0)
  
  #-------------------------Membuat Wadah Vektor Hasil Perhitungan--------------
  
  Minggu=rep(0,n)
  Recv=rep(0,n)
  Stock=rep(0,n)
  Sold=rep(0,n)
  Sisa=rep(0,n)
  Order=rep(0,n)
  OC=rep(0,n)
  HC=rep(0,n)
  SC=rep(0,n)
  Purch=rep(0,n)
  p=rep(0,n)
  TCost=rep(0,n)
  Rev=rep(0,n)
  Profit=rep(0,n)
     
    
  A=(DP*oq*pc)
  B=(1-DP)*oq*pc
      
 #-----------------------Proses Hitung Looping--------------------------------
      
      for(i in 2:n)
      {
        Minggu[1]<-1
        Recv[1]<-0
        Stock[1]<-BI
        if(Stock[1]>Dmd[1]) Sold[1]=Dmd[1]          	  
        else Sold[1]=Stock[1] 
        if(Stock[1]>Dmd[1]) Sisa[1]=Stock[1]-Sold[1]    
        else Sisa[1]=0
        if(Sisa[1]<rp)      Order[1]="Yes"              
        else Order[1]="No"
        if(Sisa[1]<rp)      OC[1]=oc                    
        else OC[1]=0   
        if(Sisa[1]<rp)      Purch[1]=A          
        else Purch[1]=0
        if(Sisa[1]==0)      HC[1]=0                     
        else HC[1]=Sisa[1]*hc
        if(Stock[1]<Dmd[1]) SC[1]=(Dmd[1]-Stock[1])*sc  
        else SC[1]=0
  
  
  
 		 TCost[1]=OC[1]+HC[1]+SC[1]+Purch[1]
  	       Rev[1]=Sold[1]*sp
  		 Profit[1]=Rev[1]-TCost[1]



        Minggu[i]=i
        if(Sisa[i-1]<rp)    Recv[i+1]=oq      else Recv[i+1]=0     
        
        Stock[i]=Recv[i]+Sisa[i-1]           
        if(Sisa[i-1]<rp)    		p[i+1]=B			       	 else p[i+1]=0
        if(Stock[i]>Dmd[i])         Sold[i]=Dmd[i]			       else Sold[i]=Stock[i]
        if(Stock[i]>Dmd[i]) 	      Sisa[i]=Stock[i]-Sold[i]	       else Sisa[i]=0
        if(Sisa[i]<rp) 	    	      Order[i]="Yes"			       else Order[i]="No"
        if(Sisa[i]<rp) 	    	      OC[i]=oc				       else OC[i]=0
        if(Sisa[i]<rp)      	      Purch[i]=A+p[i]		             else Purch[i]=p[i]
        if(Sisa[i]==0)      	      HC[i]=0                              else HC[i]=Sisa[i]*hc
        if(Stock[i]<Dmd[i]) 	      SC[i]=(Dmd[i]-Stock[i])*sc	       else SC[i]=0
       
        
        TCost[i]=OC[i]+HC[i]+SC[i]+Purch[i]
        Rev[i]=Sold[i]*sp
        Profit[i]=Rev[i]-TCost[i]  
      }
      
      cat("\n")
      cat("====================================================================================================================================\n")
      cat("\n\t\t\t\tSIMULASI INVENTORY DALAM",n,"MINGGU\n")
      cat("====================================================================================================================================\n")
      
      
      Minggu=Minggu[1:n]			
      Recieve=Recv[1:n]				
      Persediaan=Stock[1:n]			
      Permintaan=Dmd[1:n]			
      Terjual=Sold[1:n]				
      Sisa.Barang=Sisa[1:n]			
      Pemesanan=Order[1:n]
      OC=OC[1:n]			
      HC=HC[1:n]
      SC=SC[1:n]
      Purchase=Purch[1:n]
	Total.Cost=TCost[1:n] 
	Revenue=Rev[1:n]
	Keuntungan=Profit[1:n]


      hasil=data.frame(Minggu,Recieve,Persediaan,Permintaan,Terjual,Sisa.Barang,Pemesanan,OC,HC,SC,Purchase,Total.Cost,Revenue,Keuntungan)
      print(hasil)
      
      
      #-------------------------------Menghitung Mean Profit-----------------------------------
      cat("\n")
      cat("------------------------------------------------------------------------------------------------------------------------------------\n")
      rata=mean(Keuntungan)
      cat("\nRata-rata Profit (keuntungan) yang diperoleh dalam",n,"minggu = ",rata,"\n")
      cat("------------------------------------------------------------------------------------------------------------------------------------\n")
    }
    
    
    
    