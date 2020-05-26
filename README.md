## Problemi na grafih

Projekt temelji na preučevanju glavnih značilnosti grafov in modeliranju problemov z njihovo pomočjo. Uporabnik si zamisli graf, ali pa mu ga generira program, graf se izriše, na njem pa rešujemo probleme, kot so problem trgovskega potnika, iskanje minimalnega vpetega drevesa, barvanja,... S projektom bi rada prikazala uporabnost in zanimivost grafov ter njihovo praktičnost za modeliranje nekaterih povsem vsakdanjih življenjskih problemov.

 ## NAVODILA ZA UPORABNIKA:

* Program je podprt z verzijo 3.5.1 Rstudia.
* Pred zagonom skripte zagon.R naj uporabnik namesti paket BiocManager, iz katerega potem kliče knjižnico RGBL. To naredi z ukazoma:
'install.packages("BiocManager")' in 'BiocManager::install("RBGL")'.
* Uporabnik zažene skripto zagon.R v mapi shiny -  # tam so zbrane potrebne knjižnice in narejeni sklici na ostale potrebne datoteke s funkcijami

###### 1. VNOS
* Na prvem zavihku "vnos" izbiramo o osnovnih lastnostih grafa, ki ga želimo preučevati. Pri tem lahko v obliki povezavne matrike vnesemo tudi že vnaprej izbran graf ali pa podamo le željeni števili vozlišč in povezav, program pa sam generira naključen graf. - Po izboru kliknemo gumb NARISI.
###### 2. LASTNOSTI
* Na dvigem zavihku lahko poizvemo o naprednejših lastnostih našega grafa. To so na primer cikličnost, stopnje, povezanost, radij, diameter, dvodelnost,... - izberemo si po eno lastnost v seznamu in kliknemo gumb POŠLJI POIZVEDBO.
###### 3. PROBLEMI
* Tretji zavihek je namenjen reševanju problema trgovskega potnika. 



 ## TEORIJA GRAFOV:
<img src="https://github.com/nezahabjan/Projekt_MZR/blob/master/slikovno%20gradivo/SLIKA5.gif" width="300" height="200">


#### Splošne definicije:
**GRAF** je urejen par G=(V,E), kjer je V neprazna množica točk oziroma vozlišč grafa, E pa množica njegovih povezav. Vsaka povezava je množica dveh različnih vozlišč (v1,v2).
Vsak graf lahko prikažemo z matriko sosednosti (npr. A), ki je kvadratna matrika (n x n) kjer je n število vozlišč grafa, vsak njen element (ai,aj) pa ima vrednost 0, če med vozliščema ai in aj ni direktne povezave in 1, če povezava (ai,aj) obstaja. Diagonalni elementi so vedno ničelni.
Druga možnost prikaza grafa je tudi incidenčna matrika (npr. B) velikosti (n x m), kjer je n število vozlišč in m število povezav. Pri tem je vsak element 'bi,j' ničeln v primeru, da povezava 'ej' ne vsebuje vozlišča 'vi' in je enak 1 v nasprotnem. Diagonalne vrednosti so vedno enake 1.

#### Glavne lastnosti grafov:
* **Usmerjenost**: Poznamo usmerjene in neusmerjene grafe. Razlikujejo se v tem, da imata krajišči vsake povezave v usmerjenem grafu vlogi začetka in konca, pri neusmerjenih grafih pa lahko po povezavah potujemo v katerikoli smeri.

* **Stopnja vozlišča**: Stopnja vozlišča 'vi', označena z 'd(vi)' je seštevek pripadajoče vrstice incidenčne matrike. Pri neusmerjenem grafu 'd(vi)' predstavlja število povezav, katerih vozlišče 'vi' je eno od krajišč. V primeru usmerjenega grafa pa tako poznamo vhodno in izhodno stopnjo, kjer je izhodna stopnja število povezav z začetkom v 'vi' in vhodna število povezav s koncem v 'vi'. Poznamo minimalno in maksimalno stopnjo grafa.

* **Povezanost**: Graf je povezan, če je sestavljen le iz ene komponente, oziroma če se da po katerikoli poti priti iz vsakega vozlišča do vseh ostalih.

* **Cikličnost**: Usmerjenim grafom določimo cikličnost tako, da preverimo če obstaja usmerjen cikel (usmerjena, sklenjena pot, ki je sestavljena iz vsaj treh povezav). V primeru acikličnosti nato imenujemo graf »DAG« (directed acyclic graph).
V primeru neusmerjenosti pa so grafi ciklični, če obstajajo omenjene sklenjene poti dolžine vsaj 3. V tem primeru usmerjenost zanemarimo, cikli so lahko usmerjeni v eno ali drugo smer.

* **Najdaljša razdalja v grafu**: ali premer/diameter, je najdaljša med najkrajšimi potmi med vsakima dvema vozliščema v grafu. Če med dvema vozliščema grafa poti ni, pomeni da pripadata različnima komponentama grafa in posledično, da graf ni povezan. Torej max(min(d(v,u)).

* **Najkrajša razdalja v grafu**: ali polmer/radij, je najkrajša med vsemi najdaljšimi razdaljami vsakih dveh vozlišč v grafu. Torej min(max(d(v,u)).
<img src="https://github.com/nezahabjan/Projekt_MZR/blob/master/slikovno%20gradivo/SLIKA1.png" width="400" height="200">


* **Dvodelnost**: graf je dvodelen, če lahko njegova vozlišča pobarvamo z dvema barvama (črna in bela) tako, da ima vsaka povezava raznobarvni krajišči. Velja izrek, da je graf dvodelen natanko takrat, ko ne vsebuje nobenega cikla lihe dolžine.
<img src="https://github.com/nezahabjan/Projekt_MZR/blob/master/slikovno%20gradivo/SLIKA2.png" width="300" height="200">


* **Polnost**: graf je poln, če vsebuje vse možne povezave med točkami, oziroma če je vsako od vozlišč grafa povezano z vsemi ostalimi.
<img src="https://github.com/nezahabjan/Projekt_MZR/blob/master/slikovno%20gradivo/SLIKA3.png" width="300" height="200">


* **Uteženost**: Graf je utežen, če vsaki od povezav dodamo tudi cene prehoda po njih, ki jih nato na primer uporabljamo pri reševanju problemov kot je TSP. Cene prikažemo s kvadratno cenovno matriko, predstavljajo pa lahko porabljen čas med posameznima vozliščema, denarni strošek potovanja, obrabo vozila,...

#### Matematično ozadje izbranih preučevanih problemov na grafu:

*	**PROBLEM TRGOVSKEGA POTNIKA**: je problem, kjer vozlišča grafa predstavljajo mesta, povezave pa ceste med njimi. Trgovski potnik mora v enem obhodu obiti vsa mesta z najkrajšo možno potjo/najmanjšimi možnimi stroški in se na koncu vrniti na začetno mesto, pri tem pa lahko vsako mesto obišče le enkrat. Povezavam grafa tako moramo dodati cene, ki odločajo o optimalnosti obhoda. Cene shranimo v cenovno matriko (n x n), ki prikazuje cene potovanj med posameznima dvema mestoma. Problem rešujemo na polnem neusmerjenem grafu, in sicer na vsaki točki izberemo najcenejšo pot iz nje. V primeru usmerjenosti privzamemo, da so povezave v eno smer enako drage kot v drugo, obstajajo pa vse, zato lahko problem rešujemo kot pri neusmerjenem grafu. Če prvotni graf, na katerem rešujemo problem ni poln, mu dodamo manjkajoče povezave, ki jih ovrednotimo z zelo visokimi cenami, tako da pri končni rešitvi ne bodo imele vloge, oziroma je obstoj iskane poti takoj razviden iz njene cene.
*	**RAVNINSKOST GRAFA**: ravninski graf je v teoriji grafov graf, ki se ga lahko vloži v ravnino na način, da se nobeni dve povezavi med seboj ne sekata in ima vsak par povezav skupno točko le v krajiščih grafa. Pri ugotavljanju ravninskosti se vsak graf prevede na neusmerjenega oziroma se podatek o usmerjenosti zanemari, saj za rezultat ni pomemben.
<img src="https://github.com/nezahabjan/Projekt_MZR/blob/master/slikovno%20gradivo/SLIKA4.png" width="500" height="200">


*	**BARVANJE GRAFA**: pravimo, da je graf k-obarvljiv, v primeru da obstaja preslikava *c : V (G) → {1, . . . , k}*, da je *c(u) != c(v)* za poljubni sosednji točki 'u' in 'v' grafa G. Z drugimi besedami, vsakemu od krajišč grafa priredimo eno od barv tako, da nobeni dve sosednji krajišči nista enako obarvani. Najmanjšemu številu barv, ki jih pri tem porabimo, pravimo *kromatično število grafa*. Pri naši aplikaciji bomo iskali ravno to vrednost. Velja več izrekov, ki povezujejo ravninskost in kromatično število, in sicer je vsak ravninski graf 4-obarvljiv, vsak ravninski graf brez trikotnikov (ciklov dolžine 3) pa 3 obarvljiv.
*	**NAJKRAJŠA POT MED IZBRANIMA VOZLIŠČEMA**: Pot je v teoriji grafov zaporedje vozlišč grafa, pri katerih med vsakima zaporednima obstaja povezava iz prvega v drugega, v celotni poti pa se posamezno vozlišče pojavi le enkrat. Pri reševanju tega problema iščemo torej najkrajšo pot med dvema izbranima vozliščema, pri čemer lahko povezavam v grafu dodamo uteži in tako problem prevedemo na iskanje *najcenejše* povezave med dvema vozliščema.
*	**PROBLEM ISKANJA MINIMALNEGA VPETEGA DREVESA**: Rešujemo drugače povedano problem, kako postaviti električno napeljavo (po povezavah v grafu) med vsemi mesti (vozlišči grafa) tako, da bomo pri tem imeli čim manj stroškov. Reševanja se lotimo z iskanjem minimalnega vpetega drevesa v grafu. Pri tem vemo, da je *vpeto drevo* graf brez ciklov, katerega množica povezav je podmnožica povezav osnovnega grafa, množica vozlišč pa je enaka množici prvotnih vozlišč. Problem rešujemo na neusmerjenih, povezanih grafih, zato v primeru več komponent dobimo več dreves, v primeru usmerjenosti pa le to zanemarimo, dobimo drevo, pri katerem usmerjenost nima nobene vloge.
* **UGOTAVLJANJE EULERJEVE LASTNOSTI**: Graf je Eulerjev, če vsebuje Eulerjev obhod, to je sklenjen sprehod, ki se torej začne in konča v isti točki, vsebuje pa vse povezave v grafu. Takšen graf torej brez ponavljanj povezav lahko narišemo z eno potezo. Velja, da je graf Eulerjev natanko tedaj, ko je povezan in imajo vsa njegova vozlišča sode stopnje. Nekje moramo namreč v njih vstopiti in na drugi strani izstopiti. Eulerjev sprehod pa ima enake lastnosti obhodu, le da sta začetek in konec lahko drugi točki. V tem primeru pravimo, da je graf poleulerjev. Velja, da je graf Poleulerjev natanko tedaj, ko ima največ dve točki lihe stopnje. V eni bomo sprehod začeli in v drugi končali. Ista teorija velja tako za usmerjene kot tudi za neusmerjene grafe.
* **KOMPLEMENTAREN GRAF**: Graf, ki je komplementaren prvotnemu, vsebuje enako množico vozlišč kot originalen, njegove povezave pa so vse tiste povezave, ki prvotnemu grafu manjkajo do polnosti. Če je torej naš prvotni graf poln, je njegov komplement prazen graf.
* **POVEZAVNI GRAF**: Povezave v originalnem grafu predstavljajo v povezavnem grafu množico točk, njegove povezave pa so tvorjene med točkami, ki so kot povezave v prejšnjem grafu imele skupno krajišče.


