## Importer les librairies nécessaires
library(ggplot2)   ## package pour réaliser des sorties graphiques
library(gstat)     ## package pour réaliser des opérations géo-statistiques
library(raster)    ## package pour manipuler des données raster

#Choisir le working directory
#Mettre le fichier "pts_controle_lambert.txt" dans le working directory
#ex setwd("D:/Utilisateur/Données/Points")


############# Etape : 1 Représenter l'erreur géométrique et sa direction  ##################

A = read.table("pts_controle_lambert.txt", header = TRUE) # création d'un objet A contenant les 100 points de calage
## l'argument header permet de spécifier l'existence des noms de colonnes


# Identifier et stocker les variables d'intérêt
X_ref = A$Xref    ## la colonne du tableau A nommé Xref est stockée dans un objet novel obet X_ref
X_ini = A$Xinit   ## //
Y_ref = A$Yref    ## //
Y_ini = A$Yinit   ## //

# Formater la table des données (dans l'optique d'utiliser ggplot)
AA = data.frame(c(X_ini,X_ref),c(Y_ini,Y_ref),c(rep(1,100),rep(2,100)),c(seq(1,100),seq(1,100)))  ## assembler les informations et créer une nouvelle table avec un ID pour chaque point et classer les points : 1 = référence (OSO) 2 : cible (Etat aor)
colnames(AA) = c("X","Y","Class","ID") ## renommer les colonnes de la table AA
# AA = liste des coordonnées ref et ini à la suite l'une de l'autre

# Calculer le biais
biaisXY = sqrt(((X_ref-X_ini)^2)+((Y_ref-Y_ini)^2))   ## Calcul du biais planimétrique et stockage dans la variable biaisXY
AA$biaisXY = c(biaisXY,biaisXY)    ## Permet de raccrocher la variable biais XY sous la forme d'une nouvelle colonne dans la table AA

mean(biaisXY) # 101.1467
sd(biaisXY) # 47.22801


b = ggplot(data = AA[AA$Class==2,], aes(X,Y))+
  geom_point(aes(size = biaisXY), colour = "#009E73",alpha = 0.2)+
  scale_size_continuous(name = "Biais en XY, en mètres")+
  labs(x = "Coordonnées X (en Lambert 93)", 
       y = "Coordonnées Y (en Lambert 93)",
       title = "Représentation spatiale de l'erreur géométrique", 
       subtitle = "Valeurs de biais entre les points de la cible et la référence ", 
       caption = "Geometrie: Minute d'Etat Major, IGN; Data: Geoportail")

#1. la fonction ggplot() prend en entrée les données à représenter (les abscisses et les ordonnées)
#2. la fonction geom_point() permet de représenter les données sous forme de points
##2.1. l'argument size permet de faire évoluer la taille du cercle en fonction du biaisXY. 
##2.2. l'agument color permet de spécifier la couleur des points
##2.3. l'argument alpha permet de jouer sur la transparence
#5. la fonction scale_size_continuous() permet de spécifier les informations relatives à la légende
#6. la fonction labs permet de gérnérer ls titres des axes, du graphique, la source, etc...

b   ## visualiser l'objet b construit précédemment


# Calculer le biais seuleument en X et Y puis ajouter une colonne à la table AA

AA$biaisX = c((X_ref-X_ini),(X_ref-X_ini))
AA$biaisY = c((Y_ref-Y_ini),(Y_ref-Y_ini))

mean(AA$biaisX) #  -26.1526
mean(AA$biaisY) #  28.04535
min(AA$biaisXY) # 8.141356
max(AA$biaisXY) # 283.7841


# A partir des informations du biais en X et en Y, classifier la direction du biais (Nord-Ouest, Sud-Ouest, Nord-Est, Sud-Est)

AA$directionXY = rep(0,200) # Ajouter une colonne à la table AA comprenant des 0

id_NO = which(AA$biaisX<0 & AA$biaisY>0)   ## la fonction which() permet de chercher la position des lignes correspondant à la condition fixée
AA$directionXY[id_NO] = "NO"               ## on se sert alors du résultat renvoyé par which() et stockée dans id_NO pour mettre à jour la colonne directionXY
# NO = direction Nord-Ouest 
id_NE = which(AA$biaisX>0 & AA$biaisY>0)   ## NE = direction Nord-Est
AA$directionXY[id_NE] = "NE"               
id_SO = which(AA$biaisX<0 & AA$biaisY<0)   ## SO = Sud-Ouest
AA$directionXY[id_SO] = "SO"              
id_SE = which(AA$biaisX>0 & AA$biaisY<0)    ## SE = Sud-Est
AA$directionXY[id_SE] = "SE"              

# créer le graphique vecteur pour afficher les directions du biais graphiquement

a = ggplot(data = AA, aes(X,Y, group = ID))+
  geom_point(aes(fill=as.factor(Class)), pch=21, alpha = 0.08)+
  geom_path(aes(color=as.factor(directionXY)),arrow = arrow(type = "open",length=unit(0.30,"cm")))+
  scale_fill_manual(values=c("red","blue"),name = "Spatial point sets", labels = c("Points initiaux","Points de référence")) + 
  scale_colour_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73"),name = "Direction in XY")+
  labs(x = "Coordonnées X (en Lambert 93)", 
       y = "Coordonnées Y (en Lambert 93)",
       title = "Représentation spatiale de l'erreur géométrique", 
       subtitle = "Valeurs de biais entre les points ", 
       caption = "Geometrie: Minute d'Etat Major, IGN; Data: Geoportail")


a # afficher le graphique vecteur


# combiner le graphique des cercles proportionnels et vecteurs

c = ggplot(data = AA, aes(X,Y, group = ID))+
  geom_point(aes(fill=as.factor(Class),size = biaisXY), pch=21, alpha = 0.08)+
  geom_path(aes(color=as.factor(directionXY)),arrow = arrow(type = "open",length=unit(0.30,"cm")))+  scale_fill_manual(values=c("red","blue"),name = "Points de calage", labels = c("Points initiaux","Points de référence")) +   scale_colour_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73"),name = "Direction du biais planimetrique")+
  labs(x = "Coordonnées X (en Lambert 93)", 
       y = "Coordonnées Y (en Lambert 93)",
       title = "Représentation spatiale de l'erreur géométrique", 
       subtitle = "Valeurs de biais entre les points ", 
       caption = "Geometrie: Minute d'Etat Major, IGN; Data: Geoportail")
c



############# Etape 2 : Interpolation et représentation graphique de l'erreur calculée #########

## Construire une table et l'emprise de la zone d'interpolation
V = data.frame(X_ini,Y_ini,biaisXY)     ## Assembler les coordonnées X, Y initiales et le biais XY calculé précédemment, puis stocker le tout dans l'objet V
coordinates(V) <- ~ X_ini+ Y_ini        ## Définir les coordonnées de l'objet V avec les colonnes X_ini et Y_ini de cette même table. L'objet V devient alors automatiquement un objet spatial contenant l'information du biais

# Créer une grille à partir de l'objet V
grd <- as.data.frame(spsample(V, "regular", n=50000))   ## permet de créer une grille régulière de 50000 cellules sur la base de l'emprise de V
names(grd) <- c("X", "Y")     ## renommer les lignes et colonnes de l'objet grd
coordinates(grd) <- c("X", "Y")     ## définir les coordonnées de la grille à partir des lignes/colonnes de l'objet grd
gridded(grd) <- TRUE  # crée un "SpatialPixel object" et définit la grille comme un objet spatial
fullgrid(grd) <- TRUE  # crée un "SpatialGrid object" et définit l'objet spatial comme une grille avec des caractéristiques spatiales

## Interpolation avec la méthode de pondération par l'inverse (fonctions du package gstat)

P.idw <- gstat::idw(biaisXY~1,V, newdata=grd, idp=2.0)   ## Calcule l'interpolation à partir des cellules renseignées vers les cellules vides.
                                                          # inverse distance weighted interpolation
r <- raster(P.idw)                                 ## Permet de projeter les résultats de l'interpolation vers un raster et le stocker dans l'obet r

plot(r,col = rev(heat.colors(255)))                      ## Permet de visualiser le raster

writeRaster(r, "result_interpolation.tif")              ## Permet d'exporter le résultat sous format .tif
