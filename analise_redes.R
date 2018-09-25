################
# LOAD DATASET #
################

scenes.adj = readRDS("scenes.adj.RDS")

prin = readRDS("prin.rds")
sec = readRDS("sec.rds")
per.ger = c(prin,sec)


###################
# ANALISE DATASET #
###################

socio = matrix(NA,nrow = length(per.ger),ncol = length(per.ger))

colnames(socio) = per.ger
rownames(socio) = per.ger

for(i in 1:nrow(socio)){
  for(j in 1:nrow(socio)){
    if(i>j){
      aux = scenes.adj %>% select(rownames(socio)[i],
                                  rownames(socio)[j]) %>% 
        filter_all(all_vars(.==T)) %>% summarise(n=n()) %>% 
        as.numeric()
      
      socio[i,j] = aux
      socio[j,i] = aux
    }
    else{
      if(i==j){
        aux = scenes.adj %>% select(rownames(socio)[i]) %>% 
          filter_all(all_vars(.==T)) %>% summarise(n=n()) %>% 
          as.numeric()
        socio[i,i] = aux
      }
    }
  }
}
