
## v tej datoteki bom bele?ila glavne funkcije v projektu, ki se bodo potem uporabljale in klicale v shiny aplikaciji


### najprej nastavimo funkcije, ki bodo vrnile osnovne grafovske lastnosti poljubnega grafa
# A) Narisemo najprej graf

#a) izberemo lahko obliko grafa, ki jo zelimo, lahko si zamislimo tudi poln graf - za TSP
narisi_izbor <- function(n, p, directed){
  #vnesemo izbor grafa, stevilo vozlisc (n), stevilo povezav (p), usmerjenost
  #dobimo nakljucno matriko velikosti nxn, ki vsebuje p enic, torej p povezav v grafu - AJD_matrika je vedno kvadratna, ker je to povezavna matrika!!
    if (directed ==1){
      mode <- "FALSE"
    } else if (directed ==2){
      mode <- "TRUE"
    }
  network <- erdos.renyi.game(n, p, type="gnm", directed =mode)
  matrika <- get.adjacency(network)
  
  
    #data <- zeros(n,n)
    #data[randperm(n*n, p)]=1
    #network <- graph_from_adjacency_matrix(data, mode = mode, diag = FALSE)
    #set_vertex_attr(network, "label", value = letters[1:n])
    #plot(network)
  return (list("network"=network, "directed"=mode, "matrika"=data))
}
narisi_poln <- function(n, dir){
  if (dir == 1) {
    directed <- "FALSE"
  } else if (dir == 2){
    directed <- "TRUE"
  }
  network <- make_full_graph(n, directed)
  matrika <- get.adjacency(network)
  
  return(list("network"=network, "matrika"=matrika))
}

#b) v primeru, da imamo svoj graf pripravljen, vnesemo povezavno matriko in ga izrisemo, dodamo še atribut usmerjenosti
# diagonalo tudi upoštevamo, glede na to da je izbor naš
narisi_pripravljen <- function(adj_matrika, directed){
  if (directed ==1){
    mode <- "undirected"
  } else if (directed ==2){
    mode <- "directed"
  }
  data <- adj_matrika
  network <- graph_from_adjacency_matrix(data, mode = directed, diag = TRUE)
  #set_vertex_attr(network, "label", value = letters[1:dim(matrika)[1]])
  #plot(network)
  return (list("network"=network, "directed"=mode, "matrika"=data))
}


# B) koliksne so stopnje oglisc?
stopnje_2 <- function(graf){
  seznam <- list()
  network <- graf$network
  directed <- is_directed(graf)
  matrika <- graf$matrika
    if (directed == "1" | directed == "FALSE"){
      # v primeru, da imamo opravka z neusmerjenim grafom je vsaka povezava do vozlisca ena stopnja vec
      for (x in 1:length(V(graf))){
        vozlisce <- names(V(graf))
        stopnja <- length(network[[vozlisce]][[1]])
        seznam[vozlisce] <- stopnja 
      } 
    } else if (directed =="2" | directed == "TRUE"){
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

#ta funkcija je uporabljena
stopnje <- function(graf){
  seznam <- list()
  directed <- is_directed(graf)
  if (directed == "1" | directed == "FALSE"){
    seznam <- degree(graf)
    return (list("stopnje"=seznam))
    
  } else if (directed == "2" | directed == "TRUE"){
    seznam$vhodne <- degree(graf, mode= c("in"))
    seznam$izhodne <- degree(graf, mode= c("out"))
    return (list("vhodne" = seznam$vhodne, "izhodne" = seznam$izhodne))
    
  }
}


# C) koliko povezav vsebuje?
povezave <- function(graf) {
  stevilo_povezav <- length(E(graf))
  return(stevilo_povezav)
  
}


# D) ali ima nas graf cikle? - funkcija je v redu za usmerjene grafe, preverja ce so usmerjeni aciklicni
DAG <- function(graf){
  return(is_dag(graf))
}
    

# E) funkcija dela v redu ampak potrebno je izlo?iti iz rezultata cikle, ki se ponovijo le v drugem vrstnem redu!
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


# F) funkcija vrne najdaljso pot v grafu
najdaljsa_razdalja <- function(graf){
  razdalja <- diameter(graf)
  return(razdalja)
}


# G) funkcija pove ali je graf sestavljen iz ene ali veèih komponent in jih vrne loèene
komponente <- function(graf){
  if (is_connected(graf)) {
    a <-"Graf je povezan!"
  } else {
    stevilo <- count_components(graf)
    a <- paste("Graf je sestavljen iz ", stevilo, "komponent. Tu je prikazana pripadnost vozlisc vsaki od njih.")
    #components(graf)[[1]]
  }
  return(print(a))
}


# H) funkcija izrise graf, ki je komplementaren izbranemu grafu
komplement <- function(graf){
  kompl <- complementer(graf$network)
  plot(kompl)
}


# I) funkcija, ki vrne najkrajso razdaljo med dvema najbolj oddaljenima tockama grafa
najkrajsa_razdalja <- function(graf){
  razdalja <- radius(graf)
return(razdalja)
}


# J) funkcija, ki preveri ali je graf dvodelen ali ne in izriše možni komponenti v primeru dvodelnosti
dvodelen_2 <- function(graf){
  dvo <- bipartite.mapping(graf)[[1]]
  if (dvo){
    plot_1 <- bipartite.projection(graf)[[1]]
    plot_2 <- bipartite.projection(graf)[[2]]
    return(list("dvo"= dvo, "plot_1" = plot_1, "plot_2" = plot_2))
  } else {
    return(list("dvo"=dvo))
  }
}

dvodelen <- function(graf){
  dvo <- bipartite.mapping(graf)[[1]]
  return(dvo)
}

# set_vertex_attr(graph, name, index = V(graph), value) za nastavitev utezi 
# estimate_closeness(graf$network, normalized=TRUE, cutoff = 0, weigths)
# automorphisms za iskanje avto ali izomorfisms izomorfizmov
# bfs ali dfs za iskanje v globino
# are_adjacent(graph, v1, v2) za preverjanje povezave med dvema toèkama grafa
# all_simple_paths je funkcija, ki vrne vse enostavne poti med dvema toèkama
# permute(graf$network, permutation = c(2,5,3,6,7,4,1, 8,9)) za preoblikovanje grafa, èe ti ni všeè




### RE?EVANJE PROBLEMOV:
# problem barvanja grafa


# problem trgovskega potnika
sestavi_matriko_utezi <- function(graf, vektor){
  data_edge <- as.data.frame(get.edgelist(graf, names=TRUE))
  num_oglisc <- length(V(graf))
  
  matrika_utezi <- zeros(num_oglisc)
  for (vrstica in 1:dim(data_edge)[1]){
    i <- data_edge[vrstica,][[1]]
    j <- data_edge[vrstica,][[2]]
    matrika_utezi[i,j]<- vektor[vrstica]
  }
  for (i in 1:num_oglisc){
    for (j in 1:num_oglisc){
      if (matrika_utezi[i,j] == 0){
        matrika_utezi[i,j] <- 10000
      }
    }
  }
  
  diag(matrika_utezi) <- 0
  return(matrika_utezi)
  
}

PTP <- function(graf, v1, matrika_utezi){
  #preberemo povezavno matriko grafa
  #matrika <- graf$matrika

  #preberemo komplement grafa in njegovo matriko povezav
  #graf_kompl <- complementer(graf$network)
  #matrika_kompl <- get.adjacency(graf_kompl)
  
  #ustvarimo poln graf, z dodanimi manjkajocimi povezavami, ki jim dodelimo neskoncne poti, obstojecim pa vektor utezi
  #graf_tsp <- graph.adjacency(matrika, mode = "undirected", weighted = TRUE)
  #E(graf_tsp)$weight <- vect_utezi
  
  #graf_tsp_final <- add.edges(graf_tsp, get.edgelist(graf_kompl), weight=rep(10000, length(E(graf_kompl))))

  tour <- solve_TSP(ATSP(matrika_utezi), method="nearest_insertion", start=v1)
  #tour <- solve_TSP(TSP(distances(graf_tsp_final)), method = "two_opt", start = v1)
  
  
  return(list("dolzina"=tour_length(tour), "pot"=tour))
  
}



