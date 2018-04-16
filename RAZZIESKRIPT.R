# Netzwerk der Razzie Nominierten (und Gewinner) von 2010 bis 2018
## Gruppenmitglieder: Celine Eckl (cs039), Jacqueline Fritsch (jf079), Jennifer Kögel (jk180), Jana Stäbener (js324), Cosima Staneker (cs238)
## Im nachfolgenden Skript werden die einzelnen Arbeitsschritte durch Kommentare genau erklärt und definiert, damit sie nachvollzogen werden können.

## Basics
# Zunächst müssen die Basis-Pakete installiert und geladen werden. Diese Schritte können bei Bedarf überprungen werden.
install.packages("igraph")
library(igraph)

# Die Edge- und Nodelist werden direkt aus dem GitHub Verzeichnis ausgelesen.
el <- read.csv("https://raw.githubusercontent.com/SabethHdM/Razzie_HdM/master/Edgelist_Razzies.csv", header=T, as.is=T, sep = ",")
nl <- read.csv("https://raw.githubusercontent.com/SabethHdM/Razzie_HdM/master/Nodelist_Razzies.csv", header=T, as.is=T, sep = ",")

# Es wird fix geprüft, ob die Daten korrekt eingelesen wurden.
head(el)
head(nl)

# Die Edgelist wird in eine Matrix umgewandelt.
mel <- as.matrix(el)

# Das finale, erste igraph Objekt wird erstellt. Dabei wird die Nodelist und ihre Daten miteinbezogen.
razzie <- graph_from_data_frame(d=mel, vertices=nl, directed=T)

# Das igraph Objekt wird aufgerufen
razzie

# Eine erste, einfache Visualisierung erfolgt.
plot(razzie)

## Netzwerkmaße
# Als nächstes berechnen wir ein paar basale Netzwerkmaßen.

# Besteht das Netzwerk nur aus einer Komponente?
is_connected(razzie)

# Wie viele Komponenten gibt es und wie viele Knoten befinden sich in ihnen?
components(razzie)

# Wie ist der Durchmesser des Netzwerkes (= maximale Pfaddistanz)?
diameter(razzie)

# Wie ist die Dichte des Netzwerks?
edge_density(razzie)

# Wie ist die durchschnittliche Pfaddistanz?
mean_distance(razzie)

## Bessere Visualisierungen
# Die Visualisierung des Gesamtnetzwerks wird optisch optimiert. Dabei werden die Knoten danach gefärbt, wie oft sie zwischen 2010 und 2018 nominiert worden sind.
col=rev(heat.colors(5))
V(razzie)$color <- col[V(razzie)$nomtot]
coords <- layout_with_kk(razzie)*0.3
plot(razzie, edge.arrow.size=0.1, layout = coords, rescale = FALSE, main="Netzwerk Razzie Schauspieler 2010-2018 nach Häufigkeit der Nominierung", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

##Cluster
# Nun wird das Gesamtnetzwerk auf Cluster hin untersucht.
razziecluster <- cluster_walktrap(razzie)
modularity(razziecluster)
membership(razziecluster)

#Die Cluster werden visualisiert.
plot(razziecluster, razzie, edge.arrow.size=0.1, layout = coords, rescale = FALSE, main="Netzwerk Razzie Schauspieler 2010-2018 Cluster", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")
# Sandhus Vorschlag: plot(razzie, layout = layout_with_fr, edge.arrow.size=.1, vertex.label=NA, vertex.size=2)

## Teilnetzwerke
# Im folgenden werden Teilnetzwerke der jeweiligen Bezugsjahre erstellt. Dabei werden die Knoten so eingefärbt, dass die Nominierten und Gewinner hervorgehoben werden.

## Das Teilnetzwerk 2009
razzie2009 <- subgraph.edges(razzie, E(razzie)[time == "2009"])
razzie2009

# Visualisierung des Teilnetzwerks 2009
vcol2009 <- rep("grey70", vcount(razzie2009))
vcol2009[V(razzie2009)$win9 == "1"] <- "violetred3"
vcol2009[V(razzie2009)$nom9 == "1"] <- "pink2"
coords2009 <- layout_with_kk(razzie2009)*0.3
plot(razzie2009, vertex.color=vcol2009, edge.arrow.size=0.1, layout=coords2009, main="Teilnetzwerk 2009", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Das Teilnetzwerk 2010
razzie2010 <- subgraph.edges(razzie, E(razzie)[time == "2010"])
razzie2010

# Visualisierung des Teilnetzwerks 2010
vcol2010 <- rep("grey70", vcount(razzie2010))
vcol2010[V(razzie2010)$win10 == "1"] <- "violetred3"
vcol2010[V(razzie2010)$nom10 == "1"] <- "pink2"
coords2010 <- layout_with_kk(razzie2010)*0.3
plot(razzie2010, vertex.color=vcol2010, edge.arrow.size=0.1, layout=coords2010, main="Teilnetzwerk 2010", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Das Teilnetzwerk 2011
razzie2011 <- subgraph.edges(razzie, E(razzie)[time == "2011"])
razzie2011

# Visualisierung des Teilnetzwerks 2011
vcol2011 <- rep("grey70", vcount(razzie2011))
vcol2011[V(razzie2011)$win11 == "1"] <- "violetred3"
vcol2011[V(razzie2011)$nom11 == "1"] <- "pink2"
coords2011 <- layout_with_kk(razzie2011)*0.3
plot(razzie2011, vertex.color=vcol2011, edge.arrow.size=0.1, layout=coords2011, main="Teilnetzwerk 2011", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Das Teilnetzwerk 2012
razzie2012 <- subgraph.edges(razzie, E(razzie)[time == "2012"])
razzie2012

# Visualisierung des Teilnetzwerks 2012
vcol2012 <- rep("grey70", vcount(razzie2012))
vcol2012[V(razzie2012)$win12 == "1"] <- "violetred3"
vcol2012[V(razzie2012)$nom12 == "1"] <- "pink2"
coords2012 <- layout_with_kk(razzie2012)*0.3
plot(razzie2012, vertex.color=vcol2012, edge.arrow.size=0.1, layout=coords2012, main="Teilnetzwerk 2012", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Das Teilnetzwerk 2013
razzie2013 <- subgraph.edges(razzie, E(razzie)[time == "2013"])
razzie2013

# Visualisierung des Teilnetzwerks 2013
vcol2013 <- rep("grey70", vcount(razzie2013))
vcol2013[V(razzie2013)$win13 == "1"] <- "violetred3"
vcol2013[V(razzie2013)$nom13 == "1"] <- "pink2"
coords2013 <- layout_with_kk(razzie2013)*0.3
plot(razzie2013, vertex.color=vcol2013, edge.arrow.size=0.1, layout=coords2013, main="Teilnetzwerk 2013", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Das Teilnetzwerk 2014
razzie2014 <- subgraph.edges(razzie, E(razzie)[time == "2014"])
razzie2014

# Visualisierung des Teilnetzwerks 2014
vcol2014 <- rep("grey70", vcount(razzie2014))
vcol2014[V(razzie2014)$win14 == "1"] <- "violetred3"
vcol2014[V(razzie2014)$nom14 == "1"] <- "pink2"
coords2014 <- layout_with_kk(razzie2014)*0.3
plot(razzie2014, vertex.color=vcol2014, edge.arrow.size=0.1, layout=coords2014, main="Teilnetzwerk 2014", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## DasTeilnetzwerk 2015
razzie2015 <- subgraph.edges(razzie, E(razzie)[time == "2015"])
razzie2015

# Visualisierung des Teilnetzwerks 2015
vcol2015 <- rep("grey70", vcount(razzie2015))
vcol2015[V(razzie2015)$win15 == "1"] <- "violetred3"
vcol2015[V(razzie2015)$nom15 == "1"] <- "pink2"
coords2015 <- layout_with_kk(razzie2015)*0.3
plot(razzie2015, vertex.color=vcol2015, edge.arrow.size=0.1, layout=coords2015, main="Teilnetzwerk 2015", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Das Teilnetzwerk 2016
razzie2016 <- subgraph.edges(razzie, E(razzie)[time == "2016"])
razzie2016

# Visualisierung des Teilnetzwerks 2016
vcol2016 <- rep("grey70", vcount(razzie2016))
vcol2016[V(razzie2016)$win16 == "1"] <- "violetred3"
vcol2016[V(razzie2016)$nom16 == "1"] <- "pink2"
coords2016 <- layout_with_kk(razzie2016)*0.3
plot(razzie2016, vertex.color=vcol2016, edge.arrow.size=0.1, layout=coords2016, main="Teilnetzwerk 2016", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

## Das Teilnetzwerk 2017
razzie2017 <- subgraph.edges(razzie, E(razzie)[time == "2017"])
razzie2017

# Visualisierung des Teilnetzwerks 2017
vcol2017 <- rep("grey70", vcount(razzie2017))
vcol2017[V(razzie2017)$win17 == "1"] <- "violetred3"
vcol2017[V(razzie2017)$nom17 == "1"] <- "pink2"
coords2017 <- layout_with_kk(razzie2017)*0.3
plot(razzie2017, vertex.color=vcol2017, edge.arrow.size=0.1, layout=coords2017, main="Teilnetzwerk 2017", vertex.frame.color = "transparent", vertex.label.family = "Helvetica", vertex.label.color = "black")

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
