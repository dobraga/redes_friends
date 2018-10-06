require(tidyverse)
require(rvest)
require(stringr)

################
# LOAD DATASET #
################

scenes.adj = readRDS("scenes.adj.RDS")

prin = readRDS("prin.rds")
sec = readRDS("sec.rds")
per.ger = c(prin,sec)

rel_sec = readRDS("rel_sec.rds")

########################
# CRIAÇÃO SOCIO MATRIZ #
########################

# ------------------ #
# CRIAR SOCIO MATRIZ #
# ------------------ #
cria.socio.matriz = function(scenes.adj){
  socio = matrix(NA,
                 nrow = ncol(scenes.adj[,-c(1,2)]),
                 ncol = ncol(scenes.adj[,-c(1,2)]))
  
  colnames(socio) = colnames(scenes.adj[,-c(1,2)])
  rownames(socio) = colnames(scenes.adj[,-c(1,2)])
  
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
    }
  }
  
  diag(socio) = 0 #apply(socio,1,function(x) sum(x,na.rm = T))
  
  return(socio)
}

#cria.socio.matriz(scenes.adj)


# ------------------- #
# OFFSET SOCIO MATRIZ #
# ------------------- #

socio.matriz.off = function(y.matriz){

  diag = diag(y.matriz)
  diag(y.matriz) = NA
  
  m = sum(y.matriz,na.rm = T)/sum(diag) #proporção geral
  
  esp = matrix(0,ncol = ncol(y.matriz),nrow = ncol(y.matriz))
  colnames(esp) = colnames(y.matriz)
  rownames(esp) = colnames(y.matriz)
  
  for(i in 1:ncol(y.matriz)){
    for(j in 1:ncol(y.matriz)){
      if(i==j){
        esp[i,i] = 0
      }else{
        esp[i,j] = m*diag[i] * (diag[j]/(sum(diag)-diag[i]))
        # PRIMEIRA PARTE: QUANTAS PESSOAS SERIA ESPERADO QUE SAISSE DO MUN I
        # SEGUNDA PARTE: DOS CANDIDATOS A RECEBER ESSAS PESSOAS, QUANTO ELA REPRESENTA 
      }
    }
  }
  
  return(esp)

}

#socio.matriz.off(cria.socio.matriz(scenes.adj))


###########
# MODELOS #
###########

# ---------------------- #
# MATRIZ DE DELINEAMENTO #
# ---------------------- #
m.abert = function(k,intercepto = 0,c.intercepto = 1){
  if(c.intercepto>k){
    stop("Coluna de intercepto fora do intervalo")
  }
  
  comb = combn(k,2)
  
  X = matrix(rep(0,ncol(comb)*k),ncol = k)
  
  # Sem intercepto
  if(intercepto == 0){
    for(i in 1:ncol(comb)){
      X[i,comb[2,i]] = 1
      X[i,comb[1,i]] = 1
    }
  }
  
  # Com intercepto
  else{
    for(i in 1:ncol(comb)){
      
      X[i,c.intercepto] = 1
      
      if(comb[1,i]==c.intercepto){
        for(nao_entra in 1:k){
          if(nao_entra != comb[2,i] & nao_entra != comb[1,i]){
            X[i,nao_entra] = -1
          }
        }
        
      } else{
        X[i,comb[2,i]] = 1
        X[i,comb[1,i]] = 1
      }
      
    }
  }
  
  return(X)
}

#m.abert(ncol(cria.socio.matriz.off(scenes.adj)))


# ----------------- #
# MODELO SEM OFFSET #
# ----------------- #

#dataset = scenes.adj

train.model = function(dataset=NULL,offset=FALSE){
  offset=FALSE
  
  dataset = dataset[,apply(dataset,2,max) != 0]
  
  socio = cria.socio.matriz(dataset)
  
  off = diag(socio)
  
  y = socio[lower.tri(socio,diag = F)]
  nomes = colnames(socio)
  
  X = m.abert(ncol(socio))
  colnames(X) = nomes
  
  if(offset){
    socio = socio.matriz.off(socio)
    
    esp = socio

    esp.c = esp[lower.tri(esp,diag = F)]
    
    model = glm(y~X-1+offset(log(esp.c)),family = "poisson")
  }else{
    model = glm(y~X-1,family = "poisson")
  }
  
  df = summary(model)$coefficients
  rownames(df) = colnames(socio)
  
  return(df)
}

#barplot(train.model(scenes.adj,F)[,1],las = 3)


#######################
# ESCALONAMENTO MULTI #
#######################

escal.multi = function(dataset=NULL){
  socio = cria.socio.matriz(dataset)
  
  aux = tibble(pers = colnames(socio))
  
  bind_cols(aux,cmdscale(socio,2) %>% as.tibble() %>% `colnames<-`(c("x","y")))
}

#escal.multi(dataset) %>% ggplot(aes(x=x,y=y,label=pers)) + geom_text()


##############
# VIEW GRAPH #
##############

view_net = function(aux){ # aux = socio matriz
  networkData <- tibble(src=integer(),target=integer(),value=integer())
  
  for(i in 1:ncol(aux)){
    for(j in 1:ncol(aux)){
      if(i>j){
        networkData[nrow(networkData)+1,] = c(i-1,j-1,aux[i,j])
      }
    }
  }
  networkData = networkData %>% filter(value>0)
  
  networkData$value = networkData$value/100
  
  rel_sec = bind_rows(rel_sec,tibble(sec = prin, rel=prin)) %>% unique() %>% arrange(match(sec,colnames(aux)))
  
  for(i in 1:nrow(rel_sec)){
    sec = rel_sec$sec[i]
    rel = rel_sec %>% filter(sec == rel_sec$sec[i]) %>% .$rel
    
    rel2 = as.character(rel_sec[rel_sec$sec == rel,2])
    
    if(sec == rel2){
      rel_sec[rel_sec$sec == rel,2] = rel
    }
  }
  
  return(forceNetwork(Links = as.data.frame(networkData), Source = "src", Target = "target", Value = "value",
                      Nodes = as.data.frame(rel_sec), NodeID = "sec", Group = "rel", 
                      opacity = 1, zoom = T, fontSize=15,
                      clickAction = MyClickScript))
}
