# Netzwerkprojekt „Razzie“
## Gruppenmitglieder: Celine Eckl (cs039), Jacqueline Fritsch (jf079), Jennifer Kögel (jk180), Jana Stäbener (js324), Cosima Staneker (cs238)
## Im nachfolgenden Skript werden die einzelnen Arbeitsschritte durch Kommentare genau erklärt und definiert, damit sie nachvollzogen werden können.

## Basics
# Zunächst müssen die Basis-Pakete installiert und geladen werden. Diese Schritte können bei Bedarf überprungen werden.
install.packages("igraph")
library(igraph)
intall.packages("visNetwork")
library("visNetwork")

# Die Edge- und Nodelist werden direkt aus dem GitHub Verzeichnis ausgelesen.
el <- read.csv("https://raw.githubusercontent.com/SabethHdM/Razzie_HdM/master/Edgelist_Razzies.csv", header=T, as.is=T, sep = ",")
nl <- read.csv("https://raw.githubusercontent.com/SabethHdM/Razzie_HdM/master/Nodelist_Razzies.csv", header=T, as.is=T, sep = ",")

# Es wird fix geprüft, ob die Daten korrekt eingelesen wurden.
head(el)
head(nl)

# Die Edgelist wird in eine Matrix umgewandelt.
mel <- as.matrix(el)

# Das finale, erste igraph Objekt wird erstellt. Dabei werden die Nodelist und ihre Daten miteinbezogen.
razzie <- graph_from_data_frame(d=mel, vertices=nl, directed=T)

# Das igraph Objekt wird aufgerufen
razzie

# Eine erste, einfache Visualisierung erfolgt.
plot(razzie)

# Wir erstellen nun ebenfalls eine simple Form des Netzwerks, das selbstreferentielle Beziehungen ignoriert.

razziesimple <- simplify(razzie, remove.multiple = FALSE, remove.loops = TRUE)
razziesimple

## Netzwerkmaße
# Als nächstes berechnen wir ein paar basale Netzwerkmaßen.

# Besteht das Netzwerk nur aus einer Komponente?
is_connected(razzie)

# Wie viele Komponenten gibt es und welche Knoten befinden sich in ihnen?
components(razzie)

# Wie ist die Dichte des Netzwerks? Dafür nutzen wir razziesimple, da ein Netzwerk mit selbstreferentiellen Beziehungen Werte über 100 Prozent liefert und sie damit verfälscht.
edge_density(razziesimple)

## Akteursmaße
# Wir berechnen ebenfalls die Degrees der jeweiligen Netzwerke.

degree(razzie)
degree(razziesimple)

## Bessere Visualisierungen
# Die Visualisierung des Gesamtnetzwerks wird optisch optimiert. Dabei werden die Knoten danach gefärbt, wie oft sie zwischen 2010 und 2018 nominiert worden sind.
vcolrazzie <- vcount(razzie)
vcolrazzie[V(razzie)$nomtot == "1"] <- "lightpink"
vcolrazzie[V(razzie)$nomtot == "2"] <- "#cd919e"
vcolrazzie[V(razzie)$nomtot == "3"] <- "#db7093"
vcolrazzie[V(razzie)$nomtot == "4"] <- "deeppink"
vcolrazzie[V(razzie)$nomtot == "5"] <- "#8b0a50"
coords <- layout_with_kk(razzie)*0.3
plot(razzie, edge.arrow.size=0.1, vertex.color=vcolrazzie, layout = coords, rescale = FALSE, ylim=c(-1.8,1.3),xlim=c(-1.8,1.5), asp = 0, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Wir visualisieren auch das simplerazzie Netzwerk auf diese Art und Weise
vcolrazziesimple <- vcount(razziesimple)
vcolrazziesimple[V(razziesimple)$nomtot == "1"] <- "lightpink"
vcolrazziesimple[V(razziesimple)$nomtot == "2"] <- "#cd919e"
vcolrazziesimple[V(razziesimple)$nomtot == "3"] <- "#db7093"
vcolrazziesimple[V(razziesimple)$nomtot == "4"] <- "deeppink"
vcolrazziesimple[V(razziesimple)$nomtot == "5"] <- "#8b0a50"
coords <- layout_with_kk(razziesimple)*0.3
plot(razziesimple, vertex.size=10, edge.arrow.size=0.1, vertex.color=vcolrazziesimple, layout = coords, rescale = FALSE, ylim=c(-1.8,1.3),xlim=c(-1.8,1.5), asp = 0, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Gesamtnetzwerks mit visIgraph
E(razzie)$arrow.size <- 0.1
E(razzie)$color <- "lightgrey"
V(razzie)$color <- vcolrazzie
V(razzie)$size <- 10
V(razzie)$frame.color <- "transparent"
V(razzie)$label.color <- "black"

visIgraph(razzie, idToLabel=F, layout = "layout_with_fr", physics=F, type = "full")

##Cluster
# Nun wird das Gesamtnetzwerk auf Cluster hin untersucht.
razziecluster <- cluster_walktrap(razzie)
modularity(razziecluster)
membership(razziecluster)

#Die Cluster werden visualisiert.
plot(razziecluster, razzie, edge.arrow.size=0.1, layout = coords, rescale = FALSE, main="Netzwerk Razzie Schauspieler 2010-2018 Cluster", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Teilnetzwerke
# Im folgenden werden Teilnetzwerke der jeweiligen Bezugsjahre erstellt. Dabei werden die Knoten so eingefärbt, dass die Nominierten und Gewinner hervorgehoben werden.

## Das Teilnetzwerk 2009
razzie2009 <- subgraph.edges(razzie, E(razzie)[time == "2009"])
razzie2009

# Visualisierung des Teilnetzwerks 2009
vcol2009 <- rep("#b3b3b3", vcount(razzie2009))
vcol2009[V(razzie2009)$win9 == "1"] <- "#cd3278"
vcol2009[V(razzie2009)$nom9 == "1"] <- "#eea9b8"
coords2009 <- layout_with_kk(razzie2009)*0.3
plot(razzie2009, vertex.color=vcol2009, edge.arrow.size=0.1, layout=coords2009, main="Teilnetzwerk 2009", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Teilnetzwerks 2009 mit visIgraph

E(razzie2009)$arrow.size <- 0.1
E(razzie2009)$color <- "lightgrey"
V(razzie2009)$color <- vcol2009
V(razzie2009)$size <- 10
V(razzie2009)$frame.color <- "transparent"
V(razzie2009)$label.color <- "black"

visIgraph(razzie2009, layout = "layout_with_fr", physics=F, type = "full")

## Das Teilnetzwerk 2010
razzie2010 <- subgraph.edges(razzie, E(razzie)[time == "2010"])
razzie2010

# Visualisierung des Teilnetzwerks 2010
vcol2010 <- rep("#b3b3b3", vcount(razzie2010))
vcol2010[V(razzie2010)$win10 == "1"] <- "#cd3278"
vcol2010[V(razzie2010)$nom10 == "1"] <- "#eea9b8"
coords2010 <- layout_with_kk(razzie2010)*0.3
plot(razzie2010, vertex.color=vcol2010, edge.arrow.size=0.1, layout=coords2010, main="Teilnetzwerk 2010", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Teilnetzwerks 2009 mit visIgraph

E(razzie2010)$arrow.size <- 0.1
E(razzie2010)$color <- "lightgrey"
V(razzie2010)$color <- vcol2010
V(razzie2010)$size <- 10
V(razzie2010)$frame.color <- "transparent"
V(razzie2010)$label.color <- "black"

visIgraph(razzie2010, idToLabel=F, layout = "layout_with_fr", physics=F, type = "full")

## Das Teilnetzwerk 2011
razzie2011 <- subgraph.edges(razzie, E(razzie)[time == "2011"])
razzie2011

# Visualisierung des Teilnetzwerks 2011
vcol2011 <- rep("#b3b3b3", vcount(razzie2011))
vcol2011[V(razzie2011)$win11 == "1"] <- "#cd3278"
vcol2011[V(razzie2011)$nom11 == "1"] <- "#eea9b8"
coords2011 <- layout_with_kk(razzie2011)*0.3
plot(razzie2011, vertex.color=vcol2011, edge.arrow.size=0.1, layout=coords2011, main="Teilnetzwerk 2011", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Teilnetzwerks 2011 mit visIgraph

E(razzie2011)$arrow.size <- 0.1
E(razzie2011)$color <- "lightgrey"
V(razzie2011)$color <- vcol2011
V(razzie2011)$size <- 10
V(razzie2011)$frame.color <- "transparent"
V(razzie2011)$label.color <- "black"

visIgraph(razzie2011, idToLabel=F, layout = "layout_with_fr", physics=F, type = "full")

## Das Teilnetzwerk 2012
razzie2012 <- subgraph.edges(razzie, E(razzie)[time == "2012"])
razzie2012

# Visualisierung des Teilnetzwerks 2012
vcol2012 <- rep("#b3b3b3", vcount(razzie2012))
vcol2012[V(razzie2012)$win12 == "1"] <- "#cd3278"
vcol2012[V(razzie2012)$nom12 == "1"] <- "#eea9b8"
coords2012 <- layout_with_kk(razzie2012)*0.3
plot(razzie2012, vertex.color=vcol2012, edge.arrow.size=0.1, layout=coords2012, main="Teilnetzwerk 2012", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Teilnetzwerks 2012 mit visIgraph

E(razzie2012)$arrow.size <- 0.1
E(razzie2012)$color <- "lightgrey"
V(razzie2012)$color <- vcol2012
V(razzie2012)$size <- 10
V(razzie2012)$frame.color <- "transparent"
V(razzie2012)$label.color <- "black"

visIgraph(razzie2012, layout = "layout_with_fr", physics=F, type = "full")

## Das Teilnetzwerk 2013
razzie2013 <- subgraph.edges(razzie, E(razzie)[time == "2013"])
razzie2013

# Visualisierung des Teilnetzwerks 2013
vcol2013 <- rep("#b3b3b3", vcount(razzie2013))
vcol2013[V(razzie2013)$win13 == "1"] <- "#cd3278"
vcol2013[V(razzie2013)$nom13 == "1"] <- "#eea9b8"
coords2013 <- layout_with_kk(razzie2013)*0.3
plot(razzie2013, vertex.color=vcol2013, edge.arrow.size=0.1, layout=coords2013, main="Teilnetzwerk 2013", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Teilnetzwerks 2013 mit visIgraph

E(razzie2013)$arrow.size <- 0.1
E(razzie2013)$color <- "lightgrey"
V(razzie2013)$color <- vcol2013
V(razzie2013)$size <- 10
V(razzie2013)$frame.color <- "transparent"
V(razzie2013)$label.color <- "black"

visIgraph(razzie2013, idToLabel=F, layout = "layout_with_fr", physics=F, type = "full")

## Das Teilnetzwerk 2014
razzie2014 <- subgraph.edges(razzie, E(razzie)[time == "2014"])
razzie2014

# Visualisierung des Teilnetzwerks 2014
vcol2014 <- rep("#b3b3b3", vcount(razzie2014))
vcol2014[V(razzie2014)$win14 == "1"] <- "#cd3278"
vcol2014[V(razzie2014)$nom14 == "1"] <- "#eea9b8"
coords2014 <- layout_with_kk(razzie2014)*0.3
plot(razzie2014, vertex.color=vcol2014, edge.arrow.size=0.1, layout=coords2014, main="Teilnetzwerk 2014", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Teilnetzwerks 2014 mit visIgraph

E(razzie2014)$arrow.size <- 0.1
E(razzie2014)$color <- "lightgrey"
V(razzie2014)$color <- vcol2014
V(razzie2014)$size <- 10
V(razzie2014)$frame.color <- "transparent"
V(razzie2014)$label.color <- "black"

visIgraph(razzie2014, idToLabel=F, layout = "layout_with_fr", physics=F, type = "full")

## DasTeilnetzwerk 2015
razzie2015 <- subgraph.edges(razzie, E(razzie)[time == "2015"])
razzie2015

# Visualisierung des Teilnetzwerks 2015
vcol2015 <- rep("#b3b3b3", vcount(razzie2015))
vcol2015[V(razzie2015)$win15 == "1"] <- "#cd3278"
vcol2015[V(razzie2015)$nom15 == "1"] <- "#eea9b8"
coords2015 <- layout_with_kk(razzie2015)*0.3
plot(razzie2015, vertex.color=vcol2015, edge.arrow.size=0.1, layout=coords2015, main="Teilnetzwerk 2015", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Teilnetzwerks 2015 mit visIgraph

E(razzie2015)$arrow.size <- 0.1
E(razzie2015)$color <- "lightgrey"
V(razzie2015)$color <- vcol2015
V(razzie2015)$size <- 10
V(razzie2015)$frame.color <- "transparent"
V(razzie2015)$label.color <- "black"

visIgraph(razzie2015, layout = "layout_with_fr", physics=F, type = "full")

## Das Teilnetzwerk 2016
razzie2016 <- subgraph.edges(razzie, E(razzie)[time == "2016"])
razzie2016

# Visualisierung des Teilnetzwerks 2016
vcol2016 <- rep("#b3b3b3", vcount(razzie2016))
vcol2016[V(razzie2016)$win16 == "1"] <- "#cd3278"
vcol2016[V(razzie2016)$nom16 == "1"] <- "#eea9b8"
coords2016 <- layout_with_kk(razzie2016)*0.3
plot(razzie2016, vertex.color=vcol2016, edge.arrow.size=0.1, layout=coords2016, main="Teilnetzwerk 2016", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Teilnetzwerks 2016 mit visIgraph

E(razzie2016)$arrow.size <- 0.1
E(razzie2016)$color <- "lightgrey"
V(razzie2016)$color <- vcol2016
V(razzie2016)$size <- 10
V(razzie2016)$frame.color <- "transparent"
V(razzie2016)$label.color <- "black"

visIgraph(razzie2016, layout = "layout_with_fr", physics=F, type = "full")

## Das Teilnetzwerk 2017
razzie2017 <- subgraph.edges(razzie, E(razzie)[time == "2017"])
razzie2017

# Visualisierung des Teilnetzwerks 2017
vcol2017 <- rep("#b3b3b3", vcount(razzie2017))
vcol2017[V(razzie2017)$win17 == "1"] <- "#cd3278"
vcol2017[V(razzie2017)$nom17 == "1"] <- "#eea9b8"
coords2017 <- layout_with_kk(razzie2017)*0.3
plot(razzie2017, vertex.color=vcol2017, edge.arrow.size=0.1, layout=coords2017, main="Teilnetzwerk 2017", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Visualisierung des Teilnetzwerks 2017 mit visIgraph

E(razzie2017)$arrow.size <- 0.1
E(razzie2017)$color <- "lightgrey"
V(razzie2017)$color <- vcol2017
V(razzie2017)$size <- 10
V(razzie2017)$frame.color <- "transparent"
V(razzie2017)$label.color <- "black"

visIgraph(razzie2017, layout = "layout_with_fr", physics=F, type = "full")

## Teilnetzwerk Frauen
razziewomen <- delete.vertices(razzie, V(razzie)[sex == "2"])
colwomen=rev(heat.colors(3))
V(razziewomen)$color <- colwomen[V(razzie)$nomtot]
coordswomen <- layout_with_kk(razziewomen)*0.3
plot(razziewomen, vertex.color=colwomen, edge.arrow.size=0.1, layout = coordswomen, rescale = FALSE, main="Netzwerk Razzie Schauspieler 2010-2018 nach Häufigkeit der Nominierung", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Netzwerkmaße des Teilnetzwerks Frauen
components(razziewomen)
is_connected(razziewomen)
diameter(razziewomen)
edge_density(razziewomen)
mean_distance(razziewomen)

# Cluster des Teilnetzwerks Frauen
razzieclusterwomen <- cluster_walktrap(razziewomen)
modularity(razzieclusterwomen)
membership(razzieclusterwomen)
plot(razzieclusterwomen, razziewomen, edge.arrow.size=0.1, layout = coordswomen, rescale = FALSE, main="Teilnetzwerk Frauen 2010-2018 Cluster", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Teilnetzwerk Männer
razziemen <- delete.vertices(razzie, V(razzie)[sex == "1"])
colmen=rev(heat.colors(5))
V(razziemen)$color <- colmen[V(razzie)$nomtot]
coordsmen <- layout_with_kk(razziemen)*0.3
plot(razziemen, vertex.color=colmen, edge.arrow.size=0.1, layout = coordsmen, rescale = FALSE, main="Netzwerk Razzie Schauspieler 2010-2018 nach Häufigkeit der Nominierung", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Netzwerkmaße des Teilnetzwerks Männer
components(razziemen)
is_connected(razziemen)
diameter(razziemen)
edge_density(razziemen)
mean_distance(razziemen)

# Cluster des Teilnetzwerks Männer

razzieclustermen <- cluster_walktrap(razziemen)
modularity(razzieclustermen)
membership(razzieclustermen)
plot(razzieclustermen, razziemen, edge.arrow.size=0.1, layout = coordsmen, rescale = FALSE, main="Teilnetzwerk Frauen 2010-2018 Cluster", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Teilnetzwerk U30
razzieu30 <- delete.vertices(razzie, V(razzie)[age >= "30"])
coordsu30 <- layout_with_kk(razzieu30)*0.3
colu30=rev(heat.colors(2))
V(razzieu30)$color <- colu30[V(razzie)$nomtot]
plot(razzieu30, vertex.color=colu30, edge.arrow.size=0.1, layout = coordsu30, rescale = FALSE, main="Teilnetzwerk U30", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Netzwerkmaße des U30
components(razzieu30)
is_connected(razzieu30)
diameter(razzieu30)
edge_density(razzieu30)
mean_distance(razzieu30)

## Teilnetzwerk 30-50 ##funktion
razzie30to50 <- delete.vertices(razzie, V(razzie)[age < "30", age > "50"])
coords30to50 <- layout_with_kk(razzie30to50)*0.3
col30to50=rev(heat.colors(4))
V(razzie30to50)$color <- col30to50[V(razzie)$nomtot]
plot(razzie30to50, vertex.color=col30to50, edge.arrow.size=0.1, layout = coords30to50, rescale = FALSE, main="Teilnetzwerk 30-50", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

# Netzwerkmaße des 30-50
components(razzie30to50)
is_connected(razzie30to50)
diameter(razzie30to50)
edge_density(razzie30to50)
mean_distance(razzie30to50)

## Teilnetzwerk o50
razzieo50 <- delete.vertices(razzie, V(razzie)[age <= "50"])
razzieo50
plot(razzieo50, edge.arrow.size=.1, layout = layout_with_kk, main = "Teilnetzwerk o50")

#Netzwerkmaße des o50
components(razzieo50)
is_connected(razzieo50)
diameter(razzieo50)
edge_density(razzieo50)
mean_distance(razzieo50)

#EGONETZWERKE

##########Höchster Degreewert
#Egonetzwerk Adam Sandler
vSandler <- subgraph <- make_ego_graph (razzie, order=1, c("Adam Sandler"))
#Visualisierung Sandler
vFarbverlauf <- vcount(vSandler)
vFarbverlauf[V(vSandler[[1]])$nomtot == "1"] <- "pink"
vFarbverlauf[V(vSandler[[1]])$nomtot == "2"] <- "violetred1"
vFarbverlauf[V(vSandler[[1]])$nomtot == "3"] <- "violetred3"
vFarbverlauf[V(vSandler[[1]])$nomtot == "4"] <- "violetred"
vFarbverlauf[V(vSandler[[1]])$nomtot == "5"] <- "violetred4"
coords <- layout_with_kk(vSandler[[1]])*0.3
plot(vSandler[[1]], edge.arrow.size=0.1, vertex.color=vFarbverlauf, layout = coords, rescale = FALSE, ylim=c(-1.8,1.3),xlim=c(-1.8,1.5), asp = 0, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", main="Sandler EGO-Netzwerk")

#Egonetzwerk Nick Swardson 
vSwardson <- subgraph <- make_ego_graph (razzie, order=1, c("Nick Swardson"))
#Visualisierung Swardson
vFarbverlauf <- vcount(vSwardson)
vFarbverlauf[V(vSwardson[[1]])$nomtot == "1"] <- "pink"
vFarbverlauf[V(vSwardson[[1]])$nomtot == "2"] <- "violetred1"
vFarbverlauf[V(vSwardson[[1]])$nomtot == "3"] <- "violetred3"
vFarbverlauf[V(vSwardson[[1]])$nomtot == "4"] <- "violetred"
vFarbverlauf[V(vSwardson[[1]])$nomtot == "5"] <- "violetred4"
coords <- layout_with_kk(vSwardson[[1]])*0.3
plot(vSwardson[[1]], edge.arrow.size=0.1, vertex.color=vFarbverlauf, layout = coords, rescale = FALSE, ylim=c(-1.8,1.3),xlim=c(-1.8,1.5), asp = 0, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", main="Swardson EGO-Netzwerk")

#Egonetzwerk Tyler Perry
vPerry <- subgraph <- make_ego_graph (razzie, order=1, c("Tyler Perry"))
#Visualisierung Perry
vFarbverlauf <- vcount(vPerry)
vFarbverlauf[V(vPerry[[1]])$nomtot == "1"] <- "pink"
vFarbverlauf[V(vPerry[[1]])$nomtot == "2"] <- "violetred1"
vFarbverlauf[V(vPerry[[1]])$nomtot == "3"] <- "violetred3"
vFarbverlauf[V(vPerry[[1]])$nomtot == "4"] <- "violetred"
vFarbverlauf[V(vPerry[[1]])$nomtot == "5"] <- "violetred4"
coords <- layout_with_kk(vPerry[[1]])*0.3
plot(vPerry[[1]], edge.arrow.size=0.1, vertex.color=vFarbverlauf, layout = coords, rescale = FALSE, ylim=c(-1.8,1.3),xlim=c(-1.8,1.5), asp = 0, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", main="Perry EGO-Netzwerk")

###########Top drei Broker
#Egonetzwerk Robert De Niro 
vDeniro <- subgraph <- make_ego_graph (razzie, order=1, c("Robert De Niro"))
#Visualisierung Deniro
vFarbverlauf <- vcount(vDeniro)
vFarbverlauf[V(vDeniro[[1]])$nomtot == "1"] <- "pink"
vFarbverlauf[V(vDeniro[[1]])$nomtot == "2"] <- "violetred1"
vFarbverlauf[V(vDeniro[[1]])$nomtot == "3"] <- "violetred3"
vFarbverlauf[V(vDeniro[[1]])$nomtot == "4"] <- "violetred"
vFarbverlauf[V(vDeniro[[1]])$nomtot == "5"] <- "violetred4"
coords <- layout_with_kk(vDeniro[[1]])*0.3
plot(vDeniro[[1]], edge.arrow.size=0.1, vertex.color=vFarbverlauf, layout = coords, rescale = FALSE, ylim=c(-1.8,1.3),xlim=c(-1.8,1.5), asp = 0, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", main="Deniro EGO-Netzwerk")

#Egonetzwerk Ben Stiller
vStiller <- subgraph <- make_ego_graph (razzie, order=1, c("Ben Stiller"))
#Visualisierung Stiller
vFarbverlauf <- vcount(vStiller)
vFarbverlauf[V(vStiller[[1]])$nomtot == "1"] <- "pink"
vFarbverlauf[V(vStiller[[1]])$nomtot == "2"] <- "violetred1"
vFarbverlauf[V(vStiller[[1]])$nomtot == "3"] <- "violetred3"
vFarbverlauf[V(vStiller[[1]])$nomtot == "4"] <- "violetred"
vFarbverlauf[V(vStiller[[1]])$nomtot == "5"] <- "violetred4"
coords <- layout_with_kk(vStiller[[1]])*0.3
plot(vStiller[[1]], edge.arrow.size=0.1, vertex.color=vFarbverlauf, layout = coords, rescale = FALSE, ylim=c(-1.8,1.3),xlim=c(-1.8,1.5), asp = 0, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", main="Stiller EGO-Netzwerk")

#Egonetzwerk Sarah Jessica Parker
vParker <- subgraph <- make_ego_graph (razzie, order=1, c("Sarah Jessica Parker"))
#Visualisierung Parker
vFarbverlauf <- vcount(vParker)
vFarbverlauf[V(vParker[[1]])$nomtot == "1"] <- "pink"
vFarbverlauf[V(vParker[[1]])$nomtot == "2"] <- "violetred1"
vFarbverlauf[V(vParker[[1]])$nomtot == "3"] <- "violetred3"
vFarbverlauf[V(vParker[[1]])$nomtot == "4"] <- "violetred"
vFarbverlauf[V(vParker[[1]])$nomtot == "5"] <- "violetred4"
coords <- layout_with_kk(vParker[[1]])*0.3
plot(vParker[[1]], edge.arrow.size=0.1, vertex.color=vFarbverlauf, layout = coords, rescale = FALSE, ylim=c(-1.8,1.3),xlim=c(-1.8,1.5), asp = 0, vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black", main="Parker EGO-Netzwerk")

############# Weitere
#Egonetzwerk Will Ferrell
vFerrell <- subgraph <- make_ego_graph (razzie, order=1, c("Will Ferrell"))
plot(vFerrell[[1]], vertex.color="grey70", edge.color = "pink3", vertex.label.color="black", vertex.frame.color="transparent", edge.arrow.size=0.1, layout=layout_with_kk, main="Ferrell EGO-Netzwerk")

#Egonetzwerk Kevin James
vJames <- subgraph <- make_ego_graph (razzie, order=1, c("Kevin James"))
plot(vJames[[1]], vertex.color="grey70", edge.color = "pink3", vertex.label.color="black", vertex.frame.color="transparent", edge.arrow.size=0.1, layout=layout_with_kk, main="James EGO-Netzwerk")

#Egonetzwerk Kristen Stewart
vStewart <- subgraph <- make_ego_graph (razzie, order=1, c("Kristen Stewart"))
plot(vStewart[[1]], vertex.color="grey70", edge.color = "pink3", vertex.label.color="black", vertex.frame.color="transparent", edge.arrow.size=0.1, layout=layout_with_kk, main="Stewart EGO-Netzwerk")

