
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


# D) ali ima nas graf cikle? 
#funkcija preveri ali je usmerjen graf aciklicen ali ne
#tretja funkcija poisce vse cikle in jih izpise
DAG <- function(graf){
  return(is_dag(graf))
}
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
poisci_cikle <- function(g){
  izbrani_cikli = hash()
  stevilo <- 0
  for (i in V(g)){
    if (length(neighbors(g,i)) > 0){
    for (j in neighbors(g,i)){
      
      if (are_adjacent(g,j,i)){
        cikli <- all_simple_paths(g,i,j)
        cikli <- lapply(cikli, function(x) c(j, x))
        dobri_cikli <- cikli[which(sapply(cikli, length) > 3)]
        
        
        #grem po vseh do sedaj izbranih ciklih
        #print(izbrani_cikli)
        #for (cikel in range(1:length(izbrani_cikli))){
        
        #ce je kaksen kandidat v dobrih ciklih preverim ali je ta ze izbran ali ne
        if (length(dobri_cikli)>1){
          
          for (kandidat in range(1:length(dobri_cikli))){
            if (dobri_cikli[kandidat] %in% values(izbrani_cikli)){
              #print("Ta cikel že imamo")
            } else {
              #print("ta cikel gre med izbrane")
              stevilo <- stevilo + 1
              izbrani_cikli[as.character(stevilo)] <- dobri_cikli[kandidat]
              
            }
          }
        }
      }
      
      
    }
    
    }
    
  }
  if (length(izbrani_cikli)==0){
    rezultat <- list()
  } else {
    rezultat <- values(izbrani_cikli)
  }
  
  return(rezultat)
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
dvodelen <- function(graf){
  dvo <- bipartite.mapping(graf)[[1]]
  return(dvo)
}


# estimate_closeness(graf$network, normalized=TRUE, cutoff = 0, weigths)
# automorphisms za iskanje avto ali izomorfisms izomorfizmov
# permute(graf$network, permutation = c(2,5,3,6,7,4,1, 8,9)) za preoblikovanje grafa, èe ti ni všeè








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
  V(g)$color <- c(0)
  v_zaporedje <- order(degree(g), decreasing = TRUE)
  "%ni%" <- Negate("%in%")
  lapply(v_zaporedje, function(i) V(g)$color[i] <<- min(vse_barve[vse_barve %ni%
                                                                 sapply(neighbors(g, i), function(j) {
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
najdi_najkrajso_pot <- function(g, iz, v, vect_utezi){
  
  stevilo_vseh_poti <- suppressWarnings(length(all_shortest_paths(g, iz, v, weights=vect_utezi)$res))
  if (stevilo_vseh_poti == 0){
    vse_poti <- list()
  } else {
    vse_poti <- all_shortest_paths(g, iz, v, weights=vect_utezi)$res
  }
  dolzina <- min(distances(g, iz, v, weights = vect_utezi))
  
  return(list("dolzina"=dolzina, "poti"=vse_poti))

}
#funkcija nam vrne stevilo pojavov vozlisca v1 na vseh najkrajsih poteh med izbranima vozliscema
pojavitve_na_min_poteh <- function(g, v1, iz, v, vect_utezi){
   pojavitev <- sum(sapply(get.all.shortest.paths(g,iz,v, weights = vect_utezi)$res,function(x){v1 %in% x}))
return("pojavitev"=pojavitev)
}



# e) problem minimalne elektriène napeljave
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
  
  if (length(E(g)) %% 2 == 0){
    opomba <- "Problem resujemo le na grafih z lihim stevilom vozlisc!"
  } else {
    opomba <- "Tvoj graf je pravih dimenzij za resevanje problema"
  }
  board <- new_board(get.adjacency(g))
  
  if (is_solvable(board) == FALSE){
    resljivo <- "NE"
    resitev <- 0
  } else {
    resljivo <- "DA"
    resitev <- solve_board(board)
  }

  return(list("resitev"=resitev, "resljivo"=resljivo, "opomba"=opomba))
}
#ta funkcija je funkcija, ki sprejme mesto zarnice ki jo kliknemo, graf na katerem delamo poskus, 
#stopnjo igre speljano do sedaj (board) in korak na katerem smo (stevilka poskusa)
play_PUL <- function(g,x,y,board, korak){
  start_board <- new_board(get.adjacency(g))
  if (korak > 1){
    poskus <- board %>% play(x,y)
  } else if (korak == 1){
    poskus <- start_board %>% play(x,y)
  }
  start_board %>% play(matrix = poskus$entries)
  return(poskus)
}
#korak = korak + 1





