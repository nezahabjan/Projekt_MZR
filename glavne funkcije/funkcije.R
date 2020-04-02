
## v tej datoteki bom bele?ila glavne funkcije v projektu, ki se bodo potem uporabljale in klicale v shiny aplikaciji
# najprej nastavimo funkcije, ki bodo vrnile osnovne grafovske lastnosti poljubnega grafa
# A) Narisemo najprej graf
narisi <- function(matrika, directed, st_ogl, st_vrst, st_stolp){
  # matrika je lahko vstavljena ali ne
  # directed je lahko "yes" (2) ali "no" (1)
  #st_ogl pride v postev le, ko je matrika prazna in zelimo usmerjen graf
  #st_stolp in st_vrst prideta v postev le, ko je matrika prazna in zelimo neusmerjen graf
  
  if (directed == "2"){
    # usmerjen graf se pravilno izrise, zanj potrebujemo kvadratno matriko s ?tevili povezav, vrstice in stolpci so enako poimenovani, vsaka povezava gre iz vrstic v stolpce
    if (matrika !=""){
      #ce je argument matrika izpolnjen ga upostevamo in uporabimo vstavljen graf uporabnika
      #ta je OK
      data <- matrika
      colnames(data) = rownames(data) = letters[1:dim(matrika)[1]]
      network <- graph_from_adjacency_matrix(data)
      plot(network)
    } else {
      #sicer si program sam izbere nakljucno matriko izbranih dimenzij in izrise njej ustrezen graf
      #ta je OK
      data <- matrix(sample(0:1, st_ogl*st_ogl, replace=TRUE), st_ogl, st_ogl)
      colnames(data) = rownames(data) = letters[1:st_ogl]
      network <- graph_from_adjacency_matrix(data)
      plot(network)
    }
  } else if (directed == "1"){
    # pri neusmerjenih grafih imamo matriko, ki ni nujno kvadratna, v njej je 1 ?e povezava med tockama obstaja in 0 sicer
    if (matrika !=""){
      # ta je OK
      data <- matrika
      colnames(data) <- letters[1:dim(data)[2]]
      rownames(data) <- LETTERS[(dim(data)[2] + 1):(dim(data)[1]+dim(data)[2])]
      network <- graph_from_incidence_matrix(data)
      plot(network)
    } else {
      # ta je OK
      data <- matrix(sample(0:1, st_vrst*st_stolp, repl=TRUE), st_vrst, st_stolp)
      colnames(data) <- letters[1:st_stolp]
      rownames(data) <- LETTERS[(st_stolp + 1):(st_stolp + st_vrst)]
      network <- graph_from_incidence_matrix(data)
      plot(network)
    }
    
  }
  
  return (list("network"=network, "directed"=directed, "matrika"=data))
}

# B) koliksne so stopnje oglisc?
graf <- narisi("", "1", 6, 5, 4)
stopnje <- function(graf){
  seznam <- list()
  network <- graf$network
  directed <- graf$directed
  matrika <- graf$matrika
    if (directed == "1"){
      # v primeru, da imamo opravka z neusmerjenim grafom je vsaka povezava do vozlisca ena stopnja vec
      for (x in 1:length(V(network))){
        vozlisce <- names(network[[x]])
        stopnja <- length(network[[vozlisce]][[1]])
        seznam[vozlisce] <- stopnja 
      } 
    } else if (directed =="2"){
      # v primeru, ko je nas graf usmerjen, delimo stopnje na vhodne in izhodne
      #matrika_grafa <- get.adjacency(graf)
      for (element in rownames(matrika)){
      izhodna <- sum(matrika[element,])
      vhodna <- sum(matrika[,element])
      #seznam <- rbind(seznam, c(element, c(vhodna, izhodna)))
      seznam[element] <- list(c("vhodna"=vhodna, "izhodna"=izhodna))
      }
    }
  

  return(seznam)
}

# C) koliko povezav vsebuje?
#graf <- narisi(inc_matrika, "yes", 4, 5, 2)
povezave <- function(graf) {
  stevilo_povezav <- length(E(graf$network))
  return(stevilo_povezav)
  
}

# D) ali ima nas graf cikle?
#graf <- narisi("", "no", 4, 4, 3)
cikli <- function(graf){
  matrika <- graf$matrika
  network <- graf$network
  obiskani<- list()
  seznam_stopenj <- stopnje(graf)
  k <- 1
  vozlisce <- names(network[1])[k]
    while (seznam_stopenj[[vozlisce]] == 0){
      obiskani[vozlisce] <- "DA"
      k = k+1
      vozlisce <- names(network[1])[k]
    } 
  zacetni <- names(network[1])[k]
  print(zacetni)
  obiskani[zacetni] <- "DA"
  # dobili smo prvega kandidata med vozlisci, kjer zacnemo preverjati ciklicnost
  stars <- zacetni
  otroci <- list()
  cikel <- c()
  while (length(obiskani)<length(names(network[1]))){
    print(stars)
      sosedje <- names(network[[stars]][[1]])
      otroci[stars] <- sosedje
      print(sosedje)
      for (x in sosedje){
        print(x)
         if (obiskani[x]=="DA" && !(x %in% otroci[stars])){
          print("imamo cikel")
          print(cikel)
          cikel <- c()
        } else if (!(x %in% names(obiskani[]))){
          print("Nadaljujmo")
          cikel <- append(cikel, x)
          print(cikel)
          stars <- x
          obiskani[x] <- "DA"
          
        }
          
      }
          
          
      }
      return(cikel)
      
    }
    
# funkcija dela v redu ampak potrebno je izlo?iti iz rezultata cikle, ki se ponovijo le v drugem vrstnem redu!
najdi_cikle <- function(graf) {
  Cikli = list()
  for(v1 in V(graf$network)) {
    #gremo po vseh vozliscih grafa in najprej preverimo ali imajo stopnjo >0
    if(degree(graf$network, v1, mode="in") == 0) { next }
    #tedaj zberemo vse sosede tega vozlisca - kjer je povezava iz njega
    dobri_sosedje = neighbors(graf$network, v1, mode="out")
    dobri_sosedje = dobri_sosedje[dobri_sosedje > v1]
    for(v2 in dobri_sosedje) {
      #v vsakem vozliscu med dobrimi sosedi pogledamo enostavne povezave med v1 in v2
      kandidat = lapply(all_simple_paths(graf$network, v2,v1, mode="out"), function(p) c(v1,p))
      #obdrzimo pa samo tiste, ki imajo dolzino vecjo kot 3
      kandidat = kandidat[which(sapply(kandidat, length) > 3)]
      kandidat = kandidat[sapply(kandidat, min) == sapply(kandidat, `[`, 1)]
      Cikli <- append(Cikli, kandidat)
    }
  }
  if (length(Cikli)!=0){
    return(Cikli)
    
  } else {
    print("Graf nima ciklov!")
    
  }
}


# E) RE?EVANJE PROBLEMOV:
# 1) PROBLEM NAJDALJ?E IN NAJKRAJ?E POTI MED IZBRANIMA VOZLISCEMA

max_min_pot <- function(v1, v2, graf){
  
}


pripravi_vozlisca <- function(graf){
  barve <- list()
  ocetje <- list()
  razdalje <- list()
  
  for (u in rownames(get.adjacency(graf$network))){
    barve[u] <- "bela"
    ocetje[u] <- 0
    razdalje[u] <- 1000
  }
  return(list("barve"=barve, "razdalje"=razdalje, "ocetje"=ocetje))
}


dvodelnost <- function(graf, start){

  matrika <- get.adjacency(graf$network)
  barve <- rep(0,dim(matrika)[1])
  names(barve) <- rownames(matrika)
  k=1
  barve[start] <- k
  while (0 %in% barve){
  sosedje <- neighbors(graf$network, start)
  if (length(sosedje)>0){
    k=k+1
  for (sosed in sosedje){
    barve[sosed] <- k
    
  }
  
  }
  }
  return (barve)
}


preveri_utezi <- function(matrika, utezi){
  for (i in 1:dim(matrika)[1]){
    for (j in 1:dim(matrika)[2]){
      if (matrika [i,j] == 0){
        if (utezi [i,j] != 0){
          print("Ponovno preveri utezi!")
        } else {
          print("Utezi so pravilno nastavljene!")
        }
      }
    }
  }
}














