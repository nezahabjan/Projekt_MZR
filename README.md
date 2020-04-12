# Problemi_na_grafih

Projekt temelji na preučevanju glavnih značilnosti grafov in modeliranju problemov z njihovo pomočjo. Uporabnik si zamisli graf, ali pa mu ga generira program, graf se izriše, na njem pa rešujemo probleme, kot so problem trgovskega potnika, minimalne električne napeljave, barvanja,... S projektom bi rada prikazala uporabnost in zanimivost grafov ter njihovo praktičnost za modeliranje nekaterih povsem vsakdanjih življenjskih problemov.

 ## NAVODILA ZA UPORABNIKA:

* Uporabnik zažene skripto zagon.R v mapi shiny -  # tam so zbrane potrebne knjižnice in narejeni sklici na ostale potrebne datoteke s funkcijami
###### 1. VNOS
* Na prvem zavihku "vnos" izbiramo o osnovnih lastnostih grafa, ki ga želimo preučevati. Pri tem lahko v obliki povezavne matrike vnesemo tudi že vnaprej izbran graf ali pa podamo le željeni števili vozlišč in povezav, program pa sam generira naključen graf. - Po izboru kliknemo gumb NARISI.
###### 2. LASTNOSTI
* Na dvigem zavihku lahko poizvemo o naprednejših lastnostih našega grafa. To so na primer cikličnost, stopnje, povezanost, radij, diameter, dvodelnost,... - izberemo si po eno lastnost v seznamu in kliknemo gumb POŠLJI POIZVEDBO.
###### 3. PROBLEMI
* Tretji zavihek je namenjen reševanju kompleksnejših problemov na grafu iz prvega zavihka. Tu lahko izbiramo med problemom trgovskega potnika, ravninskostjo, barvanjem, problemom minimalne električne napeljave, igro ugašanja žarnic,... Končna rešitev vsakega od problemov se izvede s klikom na gumb REŠI PROBLEM.



 ## TEORIJA GRAFOV:

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

![diameter in radij grafa](https://www.google.com/search?sxsrf=ALeKk00DM9KQ04UuMipbhMY87xTbZrbd7Q:1586679748959&q=graph+diameter&tbm=isch&source=univ&sa=X&ved=2ahUKEwiA_LvWuuLoAhXJ4KYKHXDJCvIQsAR6BAgJEAE&biw=1396&bih=657#imgrc=6_cnyJoovFXrQM)

* **Dvodelnost**: graf je dvodelen, če lahko njegova vozlišča pobarvamo z dvema barvama (črna in bela) tako, da ima vsaka povezava raznobarvni krajišči. Velja izrek, da je graf dvodelen natanko takrat, ko ne vsebuje nobenega cikla lihe dolžine.

* **Polnost**: graf je poln, če vsebuje vse možne povezave med točkami, oziroma če je vsako od vozlišč grafa povezano z vsemi ostalimi.

* **Uteženost**: Graf je utežen, če vsaki od povezav dodamo tudi cene prehoda po njih, ki jih nato na primer uporabljamo pri reševanju problemov kot je TSP. Cene prikažemo s kvadratno cenovno matriko, predstavljajo pa lahko porabljen čas med posameznima vozliščema, denarni strošek potovanja, obrabo vozila,...

#### Matematično ozadje izbranih preučevanih problemov na grafu:

*	**PROBLEM TRGOVSKEGA POTNIKA**: je problem, kjer vozlišča grafa predstavljajo mesta, povezave pa ceste med njimi. Trgovski potnik mora v enem obhodu obiti vsa mesta z najkrajšo možno potjo/najmanjšimi možnimi stroški in se na koncu vrniti na začetno mesto, pri tem pa lahko vsako mesto obišče le enkrat. Povezavam grafa tako moramo dodati cene, ki odločajo o optimalnosti obhoda. Cene shranimo v cenovno matriko (n x n), ki prikazuje cene potovanj med posameznima dvema mestoma. Problem rešujemo na polnem neusmerjenem grafu, in sicer na vsaki točki izberemo najcenejšo pot iz nje. V primeru usmerjenosti privzamemo, da so povezave v eno smer enako drage kot v drugo, obstajajo pa vse, zato lahko problem rešujemo kot pri neusmerjenem grafu. Če prvotni graf, na katerem rešujemo problem ni poln, mu dodamo manjkajoče povezave, ki jih ovrednotimo z zelo visokimi cenami, tako da pri končni rešitvi ne bodo imele vloge, oziroma je obstoj iskane poti takoj razviden iz njene cene.
*	**RAVNINSKOST GRAFA**: ravninski graf je v teoriji grafov graf, ki se ga lahko vloži v ravnino na način, da se nobeni dve povezavi med seboj ne sekata in ima vsak par povezav skupno točko le v krajiščih grafa. Pri ugotavljanju ravninskosti se vsak graf prevede na neusmerjenega oziroma se podatek o usmerjenosti zanemari, saj za rezultat ni pomemben.
*	**BARVANJE GRAFA**: pravimo, da je graf k-obarvljiv, v primeru da obstaja preslikava *c : V (G) → {1, . . . , k}*, da je *c(u) != c(v)* za poljubni sosednji točki 'u' in 'v' grafa G. Z drugimi besedami, vsakemu od krajišč grafa priredimo eno od barv tako, da nobeni dve sosednji krajišči nista enako obarvani. Najmanjšemu številu barv, ki jih pri tem porabimo, pravimo *kromatično število grafa*. Pri naši aplikaciji bomo iskali ravno to vrednost. Velja več izrekov, ki povezujejo ravninskost in kromatično število, in sicer je vsak ravninski graf 4-obarvljiv, vsak ravninski graf brez trikotnikov (ciklov dolžine 3) pa 3 obarvljiv.
*	**NAJKRAJŠA POT MED IZBRANIMA VOZLIŠČEMA**: Pot je v teoriji grafov zaporedje vozlišč grafa, pri katerih med vsakima zaporednima obstaja povezava iz prvega v drugega, v celotni poti pa se posamezno vozlišče pojavi le enkrat. Pri reševanju tega problema iščemo torej najkrajšo pot med dvema izbranima vozliščema, pri čemer lahko povezavam v grafu dodamo uteži in tako problem prevedemo na iskanje *najcenejše* povezave med dvema vozliščema.
*	**PROBLEM MINIMALNE ELEKTRIČNE NAPELJAVE**: Rešujemo problem, kako postaviti električno napeljavo (po povezavah v grafu) med vsemi mesti (vozlišči grafa) tako, da bomo pri tem imeli čim manj stroškov. Reševanja se lotimo z iskanjem minimalnega vpetega drevesa v grafu. Pri tem vemo, da je *vpeto drevo* graf brez ciklov, katerega množica povezav je podmnožica povezav osnovnega grafa, množica vozlišč pa je enaka množici prvotnih vozlišč. Problem rešujemo na neusmerjenih, povezanih grafih, zato v primeru več komponent dobimo več dreves, v primeru usmerjenosti pa le to zanemarimo, dobimo drevo, pri katerem usmerjenost nima nobene vloge.
*	**PROBLEM UGAŠANJA LUČI**: Gre za igro, ki jo rešujemo na kvadratnih matrikah lihih dimenzij, z elementi 0 ali 1, kjer vsako mesto matrike predstavlja eno žarnico. V primeru, da je na (i,j)-tem mestu število 0, pomeni da je žarnica na tem mestu ugasnjena in obratno. Naš cilj je ugasniti vse žarnice s klikanjem na njih. Vsakič, ko kliknemo na žarnico, se stanje nje in vseh njenih štirih sosed (na levi, desni, zgoraj in spodaj) zamenja. Problem smo prevedli na povezavne matrike grafov z lihim številom vozlišč (3,5,7 ali 9), postopek pa ponavljamo dokler povezavna matrika ne predstavlja grafa brez povezav.

