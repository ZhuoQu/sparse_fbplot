sparsity<-c("point","peak","partial")
mod2<-execute_outdetect("persistent magnitude outlier",1000,sparsity,c(0.2,0.5,0.8))



mod3<-execute_outdetect("isolated magnitude outlier",1000,sparsity,c(0.1,0.4,0.7))

mod4<-execute_outdetect("shape outlier II",1000,sparsity,c(0.1,0.4,0.7))

mod5<-execute_outdetect("shape outlier I",100,sparsity,c(0.1,0.4,0.7))
