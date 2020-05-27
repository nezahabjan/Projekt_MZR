
## v tej datoteki bom belezila glavne funkcije v projektu, ki se bodo potem uporabljale in klicale v shiny aplikaciji


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
#b) v primeru, da imamo svoj graf pripravljen, vnesemo povezavno matriko in ga izrisemo, dodamo se atribut usmerjenosti
# diagonalo tudi upostevamo, glede na to da je izbor nas
narisi_pripravljen <- function(adj_matrika, directed){
  if (directed ==1){
    mode <- "undirected"
  } else if (directed ==2){
    mode <- "directed"
  }
  data <- adj_matrika
  network <- graph_from_adjacency_matrix(data, mode = mode, diag = TRUE)
  #set_vertex_attr(network, "label", value = letters[1:dim(matrika)[1]])
  #plot(network)
  return (list("network"=network, "directed"=mode, "matrika"=data))
}


# B) koliksne so stopnje oglisc?
stopnje <- function(graf){
  seznam <- list()
  directed <- is_directed(graf)
  if (directed == "1" | directed == "FALSE"){
    seznam <- igraph::degree(graf)
    return (list("stopnje"=seznam))
    
  } else if (directed == "2" | directed == "TRUE"){
    seznam$vhodne <- igraph::degree(graf, mode= c("in"))
    seznam$izhodne <- igraph::degree(graf, mode= c("out"))
    return (list("vhodne" = seznam$vhodne, "izhodne" = seznam$izhodne))
    
  }
}


# C) koliko povezav vsebuje?
povezave <- function(graf) {
  stevilo_povezav <- length(E(graf))
  return(stevilo_povezav)
  
}


# D) ali ima nas graf cikle? 
isci_cikle = function(g) {
  cikli = NULL
  for(v1 in V(g)) {
    #vsi ki nimajo vhodnih stopenj, se izločijo
    if(igraph::degree(g, v1, mode="in") == 0) { next }
    #izberemo sosede uporabnih vozlišč
    dobri_sosedje = neighbors(g, v1, mode="out")
    dobri_sosedje = dobri_sosedje[dobri_sosedje > v1]
    for(v2 in dobri_sosedje) {
      preizkus = lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p))
      preizkus = preizkus[which(sapply(preizkus, length) > 3)]
      preizkus = preizkus[sapply(preizkus, min) == sapply(preizkus, `[`, 1)]
      cikli  = c(cikli, preizkus)
    }
  }
  return(cikli)
}

precisti_cikle <- function(seznam){
  urejeni <- lapply(seznam, sort)
  indeksi_ven <- c()
  obdrzi <- list()
  
  if (length(urejeni) > 1){
  for (i in (2:(length(urejeni)))){
    izbor <- urejeni[1:(i-1)]
    izbor <- izbor[which(sapply(izbor, length) ==length(urejeni[[i]]))]

    if (urejeni[i] %in% izbor){
      indeksi_ven <- append(indeksi_ven, i)
    }
    
  }
    if (length(indeksi_ven)==0){
      obdrzi <- seznam
    } else {
    obdrzi <- seznam[-indeksi_ven]
    }
    } else {
      obdrzi <- seznam
    }
  return(obdrzi)
}



# F) funkcija vrne najdaljso pot v grafu
najdaljsa_razdalja <- function(graf){
  razdalja <- diameter(graf)
  return(razdalja)
}


# G) funkcija pove ali je graf sestavljen iz ene ali vecih komponent in jih vrne locene
komponente <- function(graf){
  if (is_connected(graf)) {
    a <-"Graf je povezan!"
    tipi <- components(graf)[[1]]
  } else {
    stevilo <- count_components(graf)
    a <- paste("Graf je sestavljen iz ", stevilo, "komponent. Tu je prikazana pripadnost vozlisc vsaki od njih.")
    tipi <- components(graf)[[1]]
  }
  barvanje <- tipi
  graf <- graf %>% set_vertex_attr("color", value = barvanje)
  
  
  return(list("a"=a, "graf"=graf))
}


# I) funkcija, ki vrne najkrajso razdaljo med dvema najbolj oddaljenima tockama grafa
najkrajsa_razdalja <- function(graf){
  razdalja <- radius(graf)
return(razdalja)
}


# J) funkcija, ki preveri ali je graf dvodelen ali ne in izrise mozni komponenti v primeru dvodelnosti
dvodelen <- function(graf){
  dvo <- bipartite.mapping(graf)[[1]]
  logical <- as.vector(bipartite.mapping(graf)$type)
  return(list("dvo"=dvo, "logical"=logical))
}


# estimate_closeness(graf$network, normalized=TRUE, cutoff = 0, weigths)
# automorphisms za iskanje avto ali izomorfisms izomorfizmov
# permute(graf$network, permutation = c(2,5,3,6,7,4,1, 8,9)) za preoblikovanje grafa, ce ti ni vsec








### RESEVANJE PROBLEMOV:
# a) problem trgovskega potnika
#PTP je funkcija, ki vnesenemu grafu, zacetni tocki in matriki utezi(sestavljena iz inputa vektorja utezi) priredi problem trgovskega potnika in ga resi
#vrne dolzino najkrajse poti in njen potek po vozliscih
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
  if (is_directed(graf) == FALSE){
    for (i in 1:num_oglisc){
      for (j in 1:num_oglisc){
      matrika_utezi[j,i] <- matrika_utezi[i,j]
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
# dodaj izris poti


# b) problem barvanja grafa
## funkcija vrne najmanjse stevilo barv, s katerimi lahko pobarvamo graf - kromaticno stevilo grafa
## izrise nam obarvan graf in poda seznam vozlisc, s pripadajocimi barvami
kromaticno_stevilo <- function(g) {
  #najvecje mozno kromaticno stevilo je stevilo vozlisc grafa
  vse_barve <- c(1:length(V(g)))
  print(vse_barve)
  V(g)$color <- c(0)
  v_zaporedje <- order(igraph::degree(g), decreasing = TRUE)
  print(v_zaporedje)
  "%ni%" <- Negate("%in%")
  lapply(v_zaporedje, function(i) V(g)$color[i] <<- min(vse_barve[vse_barve %ni%
                                                                 sapply(neighbors(g, i, "total"), function(j) {
                                                                   V(g)$color[j]
                                                                 })]))
  barvanje <- V(g)$color
  names(barvanje) <- V(g)

  g <- g %>% set_vertex_attr("color", value = barvanje)

  return(list("krom_stevilo" = max(V(g)$color), "barvanje" = barvanje, "slika"=g))
}



# c) ravninskost grafa
#funkcija pove ali je graf ravninski ali ne, vsak graf najprej prevede na neusmerjenega.
#lep prikaz je izris z layout.fruchterman.reingold()
ravninski <- function(g){
  g <- as.undirected(g)
  g_nel <- as_graphnel(g)
  planar <- boyerMyrvoldPlanarityTest(g_nel)
  
return(list("planar" = planar))
       
}



# d) najkrajsa pot med izbranima vozliscema
#funkcija vrne seznam najkrajsih poti in dolzino najkrajse
najdi_min_pot <- function(g, iz, v, vect_utezi){
  g <- set_edge_attr(g, "weight",E(g), vect_utezi)
  stevilo_min_poti <- suppressWarnings(length(all_shortest_paths(g, iz, v, weights=vect_utezi)$res))
  stevilo_enostavnih_poti <- length(all_simple_paths(g, iz, v))
  
  
  if (stevilo_enostavnih_poti == 0){
    min_poti <- list()
    min_dolzina <- 0
    #max_dolzina <- 0
    #max_primeri <- 0
    min_primeri <- 0
  } else {
    min_poti <- all_shortest_paths(g, iz, v, weights=vect_utezi)$res[[1]]
    enostavne_poti <- all_simple_paths(g, iz, v)
    min_dolzina <- min(distances(g, iz, v, weights = g$weight))
    min_primeri <- min_poti
    #[which(sapply(min_poti, function(x) {distances(g, iz, v)}) < (min_dolzina+1))]
    #max_primeri <- enostavne_poti[which(sapply(enostavne_poti, length) > (max_dolzina-1))]
  }

  
  return(list("min_dolzina"=min_dolzina, "min_primeri"=min_primeri))

}
#funkcija nam vrne stevilo pojavov vozlisca v1 na vseh najkrajsih poteh med izbranima vozliscema
pojavitve_na_min_poteh <- function(g, v1, iz, v, vect_utezi){
   pojavitev <- sum(sapply(get.all.shortest.paths(g,iz,v, weights = vect_utezi)$res,function(x){v1 %in% x}))
return("pojavitev"=pojavitev)
}



# e) problem minimalne elektricne napeljave
#resujemo s pomocjo iskanja minimalnega vpetega drevesa v grafu, funkcija vrne drevo ki predstavlja napeljavo
#dobimo tudi ceno napeljave po tem minimalnem drevesu
PMEN <- function(g, vect_utezi){
  g <- g %>% set_edge_attr("weight", value = vect_utezi)
   min_drevo <- mst(g, weights=vect_utezi)
   cena <- sum(E(min_drevo)$weight)
   
   return(list("cena"=cena, "napeljava"=min_drevo))
}



# f) problem ugasanja luci
#igramo igro na grafu, pri kateri moramo ugasniti vse luci, kar pomeni da postavimo vse elemente matrike na 0
#igro se igra le na grafih, ki imajo liho stevilo oglisc, usmerjenost pa ni pomembna
#koncu vidimo v funkciji PUL tudi resitev igre, podana je z matriko, ki vsebuje 1 na mestih, ki morajo biti prizgana, ni vazno v kaksnem vrstnem redu
PUL <- function(g){
  #funkcija nam pove ali je igra resljiva in poda resitev, ce jo zelimo videti
  
  if (length(V(g)) %% 2 == 0){
    opomba <- "Problem resujemo le na grafih z lihim stevilom vozlisc!"
    resitev <- 0
    resljivo <- "NE"
  } else {
    opomba <- "Tvoj graf je pravih dimenzij za resevanje problema"

  board <- new_board(get.adjacency(g))
  
  if (is_solvable(board) == FALSE){
    resljivo <- "NE"
    resitev <- 0
  } else {
    resljivo <- "DA"
    resitev <- as.matrix(solve_board(board))
  }
}
  return(list("resitev"=resitev, "resljivo"=resljivo, "opomba"=opomba))
}
#ta funkcija je funkcija, ki sprejme mesto zarnice ki jo kliknemo, graf na katerem delamo poskus, 
#stopnjo igre speljano do sedaj (board) in korak na katerem smo (stevilka poskusa)
play_PUL <- function(g,igra, x,y,matrika_stanj, izbor_nadaljevanja){
  #pogledamo kaj smo imeli na zacetku
  if (izbor_nadaljevanja == 1){
    start_board <- new_board(get.adjacency(g))
  } else if (izbor_nadaljevanja == 3){
    start_board <- new_board(igra$entries)
 }
  
    board <- new_board(matrika_stanj)
    poskus <- board %>% play(x,y)
  
  start_board %>% play(matrix = poskus$entries)
  vrni_poskus <- board_entries(poskus)
  return(vrni_poskus)
}



# g) Eulerjev graf
#raziskujemo, ali je graf Eulerjev ali ne (ali vsebuje Eulerjev obhod)
#ali je graf poleulerjev? (ali vsebuje Eulerjev sprehod)
#s koliko najmanj potezami lahko graf narisemo?
Euler <- function(g){
  graph <- as_graphnel(g)
  min_st_potez <- 0
  komentar <- ""
  cikel <- NULL
  #najprej preverimo obstoj obhoda ali vsaj poti
  if (hasEulerianCycle(graph)==TRUE){
    cikel <- eulerian(graph, NULL)
    min_st_potez <- 1
    komentar_cikel <- "Graf je Eulerjev, tu je cikel:"
   } else {
    cikel <- NULL
    komentar_cikel <- "Tvoj graf ni Eulerjev."
    }
  
  
  #sedaj poiscimo najmanjse stevilo potez za risanje grafa
  if (min_st_potez ==0){
    min_st_potez <- stej_liho(igraph::degree(g))/2
  }

  return(list("cikel"=cikel,"min_st_potez"=min_st_potez, "komentar_cikel" = komentar_cikel))
}




#pomozna funkcija za stetje lihih vozlisc grafa
stej_liho <- function(x) { 
  k <- 0 
  for (n in x) {
    if (n %% 2 == 1){
      k <- k+1
    }
  }
  return(k)
} 



# h) Graficnost zaporedja
#ali je zaporedje graficno ali ne? (podamo zaporedje stopenj vozlisc)
#funkcija poda nakljucen graf, ki uposteva dane stopnje
graficnost <- function(izhodne, vhodne){
    obstaja <- is_graphical(izhodne, vhodne)
    if (obstaja ==TRUE){
      primer <- sample_degseq(izhodne, vhodne)
    } else {
      primer <- 0
    }
    return(list("obstaja"=obstaja, "primer"=primer))
}



# i) ustvari povezavni graf
#funkcija poda in izrise povezavni graf nasega osnovnega grafa
povezavni <- function(g){
  line_graph <- make_line_graph(g)
  network <- line_graph
  return(list("line_graph"=line_graph, "network"=network))
}



# j) ustvari komplementaren graf
#funkcija poda in izrise komplementaren graf nasega osnovnega
komplement <- function(g){
  kompl <- complementer(g)
  network <- kompl
  return(list("kompl"=kompl, "network"=network))
}



















