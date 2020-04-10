# Projekt_MZR-Problemi_na_grafih

Projekt temelji na preučevanju glavnih značilnosti grafov in reševanju problemov z njihovo pomočjo. Uporabnik si zamisli graf, ali pa mu ga generira program, graf se izriše, na njem pa rešujemo probleme, kot so problem trgovskega potnika, minimalne električne napeljave, barvanja, razporejanje semaforjev,... Pozanimamo se lahko o vseh glavnih lastnostih grafa, ugotavlja grafičnost zaporedij, medsebojno izomorfnost med vnešenimi grafi,... S projektom bi rada prikazala vsakdanjo uporabnost in zanimivost grafov.

NAVODILA ZA UPORABNIKA:

* uporabnik zažene skripto zagon.R v mapi shiny - tam so zbrane potrebne knjižnice in narejeni sklici na ostale potrebne datoteke s funkcijami
* ko se aplikacija odpre lahko na prvem zavihku izbiramo o osnovnih lastnostih grafa, ki ga želimo preučevati, pri čemer lahko programu podamo svoj graf v obliki povezavne matrike, ali pa se graf zgenerira sam na podlagi podatka o usmerjenosti in željenem številu povezav in vozlišč - Po izboru kliknemo gumb NARISI
* na dvigem zavihku lahko poizvemo o naprednejših lastnostih našega grafa - izberemo si po eno in eno lastnost v seznamu
* tretji zavihek je namenjen reševanju kompleksnejših problemov, uporabnik si izbere problem, ki se izvede na grafu iz prvega zavihka



TEORIJA GRAFOV:

Definicija grafa: Graf je urejen par G=(V, E), kjer je V neprazna množica točk oziroma vozlišč, E pa množica povezav. Vsaka povezava je množica dveh različnih vozlišč.
Vsak graf lahko prikažemo z matriko sosednosti (npr. A), ki je kvadratna matrika (n x n) kjer je n število vozlišč grafa, elementi pa so (ai, aj)=0, če med vozliščema ai in aj ni direktne povezave in 1, če povezava (ai, aj) obstaja. Diagonalni elementi so vedno ničelni.
Druga možnost je tudi incidenčna matrika (npr. B) velikosti (n x m), kjer je n število vozlišč in m število povezav, kjer je vsak element bi,j =0 v primeru, da povezava ej ne vsebuje vozlišča vi in 1 sicer. Na diagonali so vrednosti vedno enake 1.

Glavne lastnosti grafov:

* Usmerjenost: poznamo usmerjene in neusmerjene grafe

* Stopnja vozlišča: stopnja vozlišča d(vi) je seštevek pripadajoče vrstice incidenčne matrike. Pri neusmerjenem grafu predstavlja število povezav, katerih vozlišče vi je eno od krajišč. V primeru usmerjenega grafa pa tako poznamo vhodno iz izhodno stopnjo, kjer je izhodna stopnja število povezav z začetkom v vi in vhodna število povezav s koncem v vi. Poznamo minimalno in maksimalno stopnjo grafa.

* Povezanost: graf je povezan, če je sestavljen le iz ene komponente, oziroma če se da po katerikoli poti priti iz vsakega vozlišča do vseh ostalih.

* Cikličnost: usmerjenim grafom določimo cikličnost tako, da preverimo če obstaja usmerjen cikel (usmerjena, sklenjena pot, ki je sestavljena iz vsaj treh povezav). V primeru acikličnosti imenujemo graf »DAG« (directed acyclic graph).
V primeru neusmerjenih grafov so ti ciklični, če obstajajo takšne sklenjene poti dolžine več kot 3. V tem primeru usmerjenost zanemarimo, cikli so lahko usmerjeni v eno ali drugo smer.

* Najdaljša razdalja v grafu: ali premer/diameter, je najdaljša med najkrajšimi potmi med vsakima dvema vozliščema v grafu. Če med dvema vozliščema grafa poti ni, pomeni da pripadata različnima komponentama grafa in da graf ni povezan.

* Najkrajša razdalja v grafu: ali polmer/radij, je najkrajša od omenjenih razdalj (najkrajših možnih poti med poljubnima dvema vozliščema grafa).

* Dvodelnost: graf je dvodelen, če lahko njegova vozlišča pobarvamo z dvema barvama (črna in bela) tako, da ima vsaka povezava raznobarvni krajišči. Graf je dvodelen natanko takrat, ko ne vsebuje nobenega cikla lihe dolžine.

* Polnost: graf je poln, če vsebuje vse možne povezave med točkami, oziroma če je vsaka od točk povezana z vsemi ostalimi.

* Uteženost: Graf je utežen, če vsaki od povezav dodamo tudi cene prehoda po njih, ki jih nato na primer uporabljamo pri reševanju problemov kot je TSP. Cene prikažemo s kvadratno cenovno matriko.

Preučevani problemi na grafu:

*	PROBLEM TRGOVSKEGA POTNIKA: je problem, kjer vozlišča grafa predstavljajo mesta, povezave pa ceste med njimi. Trgovski potnik mora v enem obhodu obiti vsa mesta z najkrajšo možno potjo/stroški in se na koncu vrniti na začetno mesto, pri tem pa lahko vsako mesto obišče le enkrat. Povezavam grafa tako lahko dodamo cene, ki odločajo o optimalnosti obhoda. Cene shranimo v cenovno matriko (n x n), ki prikazuje cene potovanja med posameznima dvema mestoma. Problem rešujemo na polnem neusmerjenem grafu, na vsaki točki izberemo najcenejšo pot naprej do naslednje. V primeru usmerjenosti privzamemo, da so povezave v eno smer enako drage kot v drugo, obstajajo pa vse, zato lahko problem rešujemo kot pri neusmerjenem grafu. Če prvotni graf, na katerem rešujemo problem ni poln, mu dodamo manjkajoče povezave, ki jih ovrednotimo z zelo visokimi cenami, tako da pri končni rešitvi ne bodo imele vloge.
*	RAVNINSKOST GRAFA: ravninski graf je v teoriji grafov graf, ki se ga lahko vloži v ravnino na način, da se nobeni dve povezavi med seboj ne sekata in ima vsak par povezav skupno točko le v krajiščih grafa. Pri ugotavljanju ravninskosti se vsak graf prevede na neusmerjenega oziroma se podatek o usmerjenosti zanemari, saj za rezultat ni pomemben.
*	BARVANJE GRAFA: pravimo, da je graf k-obarvljiv, v primeru da obstaja preslikava c : V (G) → {1, . . . , k}, da je c(u) != c(v) za poljubni sosednji točki u in v grafa G. Z drugimi besedami, vsakemu od krajišč grafa priredimo eno od barv tako, da nobeni dve sosednji krajišči nista enako obarvani. Najmanjšemu številu barv, ki jih pri tem porabimo, pravimo kromatično število grafa. Pri naši aplikaciji bomo iskali ravno to vrednost. Velja več povezav med ravninskostjo in kromatičnim številom, in sicer je vsak ravninski graf 4-obarvljiv, vsak ravninski graf brez trikotnikov (ciklov dolžine 3) pa 3 obarvljiv.
*	NAJKRAJŠA POT MED IZBRANIMA VOZLIŠČEMA: Pot je v teoriji grafov zaporedje vozlišč grafa, pri katerih med vsakima zaporednima obstaja povezava iz prvega v drugega, v celotni poti pa se posamezno vozlišče pojavi le enkrat. Pri tem problemu iščemo torej najkrajšo pot med dvema izbranima vozliščema, pri čemer lahko povezavam v grafu dodamo uteži in tako problem prevedemo na iskanje najcenejše povezave med dvema vozliščema.
*	PROBLEM MINIMALNE ELEKTRIČNE NAPELJAVE: Rešujemo problem, kako postaviti električno napeljavo (po povezavah v grafu) med vsemi mesti (vozlišči grafa) tako, da bomo pri tem imeli čim manj stroškov. Reševanja se lotimo z iskanjem minimalnega vpetega drevesa v grafu. Pri tem vemo, da je vpeto drevo graf brez ciklov, katerega množica povezav je podmnožica povezav osnovnega grafa, množica vozlišč pa je enaka množici prvotnih vozlišč. Problem rešujemo na neusmerjenih, povezanih grafih, zato v primeru več komponent dobimo več dreves, v primeru usmerjenosti pa le to zanemarimo, dobimo drevo, pri katerem usmerjenost nima nobene vloge.
*	PROBLEM UGAŠANJA LUČI: Gre za igro, ki jo rešujemo na kvadratnih matrikah lihih dimenzij, z elementi 0 ali 1, kjer vsako mesto matrike predstavlja eno žarnico. V primeru, da je na (i,j)-tem mestu število 0, pomeni da je žarnica na tem mestu ugasnjena in obratno. Naš cilj je ugasniti vse žarnice s klikanjem na njih. Vsakič ko kliknemo na žarnico, se stanje nje in vseh njenih štirih sosed zamenja. Problem smo prevedli na povezavne matrike grafov z lihim številom vozlišč, postopek pa ponavljamo dokler povezavna matrika ne predstavlja grafa brez povezav.

