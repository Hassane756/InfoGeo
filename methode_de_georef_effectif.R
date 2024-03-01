## Importer les librairies nécessaires
library(splines) ## package pour obtenir les fonctions splines
library(ggplot2) ## package pour réaliser des sorties graphiques
library(gridExtra) ## package pour travailler avec des graphiques sous forme de grilles

# Choisir le working directory
# setwd("D:/Utilisateur/Données/Points")




####### Etape 1: Géoréférencer et prédire de nouvelles coordonnées ######

## Importer la table de points

A = read.table("pts_controle_lambert.txt", header = TRUE) # Aoter les points de controle à l'obet A

# Identifier et stocker les vecteurs d'intérêt
X_ref = A$Xref
X_ini = A$Xinit
Y_ref = A$Yref
Y_ini = A$Yinit

## Predire la coordonnée X à partir des coordonnées cible et référence
# Déclarer les données de calibration et de prédiction
calibX = data.frame(X_ini,X_ref)
testX = data.frame(X_ini)
predX = data.frame("X_ini" = testX$X_ini)              ## Identifier le vecteur à prédire

# Pour la méthode polynomiale d'ordre 1
myFitP1X <- lm(X_ref ~ poly(X_ini,1), calibX)            ## Calibrer la fonction de régression (polynome d'ordre 1 ici)
P_X_p1 = predict(myFitP1X, predX, type = "response")        ## Prédire les coordonnées initiales avec l'équation polynomiale calibrée 

# Pour la méthode polynomiale d'ordre 2
myFitP2X <- lm(X_ref ~ poly(X_ini,2), calibX)            ## Calibrer la fonction de régression (polynome d'ordre 2 ici)
P_X_p2 = predict(myFitP2X, predX, type = "response")        ## Prédire les coordonnées initiales avec l'équation polynomiale calibrée 

# Pour la méthode spline
myFitSpX <- lm(X_ref ~ ns(X_ini, df = 7),data = calibX)## Calibrer la fonction de régression (splines ici) 
P_X_sp = predict(myFitSpX,predX, type = "response")    ## Prédire les coordonnées initiales avec l'équation des splines cubiques -- voir ?predict()



## Predire la coordonnée Y ##

# Déclarer les données de calibration et de prédiction
calibY = data.frame(Y_ini,Y_ref)
testY = data.frame(Y_ini)
predY = data.frame("Y_ini" = testY$Y_ini)

# Pour un polynome d'ordre 1
myFitP1Y <- lm(Y_ref ~ poly(Y_ini,1), calibY)
P_Y_p1 = predict(myFitP1Y, predY, type = "response")

# Pour un polynome d'ordre 2
myFitP2Y <- lm(Y_ref ~ poly(Y_ini,2), calibY)
P_Y_p2 = predict(myFitP2Y, predY, type = "response")


# Pour la méthode spline
myFitSpY <- lm(Y_ref ~ ns(Y_ini, df = 7),data = calibY)
P_Y_sp = predict(myFitSpY,predY, type = "response")

####### Etape 2: Apprendre à mesurer les écarts entre les coordonnées transformés (prédites) et les coordonnées de référence #######

## Calculer l'erreur quadratique planimétrique
resPol1_XY = sqrt(((P_X_p1-X_ref)^2)+((P_Y_p1-Y_ref)^2))          ## Pour la prédiction avec le polynome d'ordre 1

resPol2_XY = sqrt(((P_X_p2-X_ref)^2)+((P_Y_p2-Y_ref)^2))          ## Pour la prédiction avec le polynome d'ordre 2

resSp_XY = sqrt(((P_X_sp-X_ref)^2)+((P_Y_sp-Y_ref)^2))     ## Pour la prédiction avec les splines cubiques

## Calculer l'erreur moyenne quadratique planimétrique
mean(resPol1_XY)
mean(resPol2_XY)
mean(resSp_XY)
# methode splines cubiques produit une erreur moins importante de 5 pts


####### Etape 3 : Apprendre à évaluer la transformation géométrique avec la méthode de validation croisée ######

## Déclarer des listes vides
resPol1_XY = list()  # pour les résultats polynomiaux d'ordre 1
resPol2_XY = list()  # pour les résultats polynomiaux d'ordre 1
resSp_XY = list()   # pour les résultats des splines cubiques


t = length(X_ref)
for (n in 1:t){
  #########################################################
  ### Formater les jeux de données de calibration et de prédiction
  dX = data.frame(X_ref,X_ini)
  dY = data.frame(Y_ref,Y_ini)
  ## Pour les coordonnées X
  calibX = dX[-n,]
  testX = dX[n,]
  ## Pour les coordonnées Y
  calibY = dY[-n,]
  testY = dY[n,]
  
  ########################################################
  ### Prédire les coordonnées à l'aide des modèles de transformation
  
  ## Predire la coordonnée X
  # Pour un polynome d'ordre 1
  myFitP1X <- lm(X_ref ~ poly(X_ini,1), calibX)            
  predX = data.frame("X_ini" = testX$X_ini)
  P_X_p1 = predict(myFitP1X, predX, type = "response")
  
  # Pour la méthode polynomiale d'ordre 2
  myFitP2X <- lm(X_ref ~ poly(X_ini,2), calibX)           
  P_X_p2 = predict(myFitP2X, predX, type = "response")        
  
  # Pour la méthode spline
  myFitSpX <- lm(X_ref ~ ns(X_ini, df = 7),data = calibX)
  P_X_sp = predict(myFitSpX,predX, type = "response")
  
  ## Predire la coordonnée Y
  # Pour un polynome d'ordre 1
  myFitP1Y <- lm(Y_ref ~ poly(Y_ini,1), calibY)
  predY = data.frame("Y_ini" = testY$Y_ini)
  P_Y_p1 = predict(myFitP1Y, predY, type = "response")
  
  # Pour un polynome d'ordre 2
  myFitP2Y <- lm(Y_ref ~ poly(Y_ini,2), calibY)
  P_Y_p2 = predict(myFitP2Y, predY, type = "response")
  
  # Pour la méthode spline
  myFitSpY <- lm(Y_ref ~ ns(Y_ini, df = 7),data = calibY)
  P_Y_sp = predict(myFitSpY,predY, type = "response")
  
  #######################################################
  ### Calculer l'erreur et stocker les résultats
  resPol1_XY[[n]] = sqrt(((P_X_p1-testX$X_ref)^2)+((P_Y_p1-testY$Y_ref)^2))  
  resPol2_XY[[n]] = sqrt(((P_X_p2-testX$X_ref)^2)+((P_Y_p2-testY$Y_ref)^2))    
  resSp_XY[[n]] = sqrt(((P_X_sp-testX$X_ref)^2)+((P_Y_sp-testY$Y_ref)^2))
}

P_Pol1_XY = do.call(rbind,resPol1_XY)
mean(P_Pol1_XY)    ## Permet de calculer la moyenne des erreurs quadratiques pour la méthode polynomiale 1
sd(P_Pol1_XY)      ## Permet de calculer l'écart type des erreurs quadratiques pour la méthode polynomiale 1

P_Pol2_XY = do.call(rbind,resPol2_XY)
mean(P_Pol2_XY)    ## la moyenne des erreurs quadratiques pour la méthode polynomiale 2
sd(P_Pol2_XY)      ## calculer l'écart type des erreurs quadratiques pour la méthode polynomiale

P_Sp_XY = do.call(rbind,resSp_XY)
mean(P_Sp_XY)     ## Même chose pour les splines cubiques
sd(P_Sp_XY)       ## Même chose pour les splines cubiques

mean_error = data.frame(c(mean(P_Pol1_XY), sd(P_Pol1_XY)), c(mean(P_Pol2_XY), sd(P_Pol2_XY)), c(mean(P_Sp_XY), sd(P_Sp_XY)))


####### Etape 4 : Apprendre à faire varier l’effectif des points de controle #########

### Déclarer des listes vides pour accueillir les résultats finaux
list_Pol1_XY = list()
list_Pol2_XY = list()
list_Sp_XY = list()

#resume = as.data.frame(matrix(ncol = 4, nrow = 5))


# tirage de 100 #

for (i in 1:100){
  
  ## Séléctionner aléatoirement un nombre de points
  t = 100                      ### ajuster l'effectif à tester (10, 20 30 70 ou 100)
  id = sample(1:nrow(A),t)      ### la fonction sample() permet de sélectionner aléatoirement N nombre parmi une taille de vecteurs
  B = A[id,]
  
  ## Identifier et stocker les vecteurs d'intérêt
  X_ref = B$Xref
  X_ini = B$Xinit
  Y_ref = B$Yref
  Y_ini = B$Yinit
  
  ## Visualiser les points retenus
  #plot(X_ref,Y_ref)
  
  ########################################################
  ### Déclarer des vecteurs vides pour accueillir les résultats
  resPol1_XY = list() 
  resPol2_XY = list()  
  resSp_XY = list()  
  
  for (n in 1:t){
    #########################################################
    ### Formater les jeux de données de calibration et de prédiction
    dX = data.frame(X_ref,X_ini)
    dY = data.frame(Y_ref,Y_ini)
    ## Pour les coordonnées X
    calibX = dX[-n,]
    testX = dX[n,]
    ## Pour les coordonnées Y
    calibY = dY[-n,]
    testY = dY[n,]
    
    ########################################################
    ### Prédire les coordonnées à l'aide des modèles de transformation
    
    ## Predire la coordonnée X
    # Pour un polynome d'ordre 1
    myFitP1X <- lm(X_ref ~ poly(X_ini,1), calibX)            
    predX = data.frame("X_ini" = testX$X_ini)
    P_X_p1 = predict(myFitP1X, predX, type = "response")
    
    # Pour la méthode polynomiale d'ordre 2
    myFitP2X <- lm(X_ref ~ poly(X_ini,2), calibX)           
    P_X_p2 = predict(myFitP2X, predX, type = "response")        
    
    # Pour la méthode splines
    myFitSpX <- lm(X_ref ~ ns(X_ini, df = 3),data = calibX)
    P_X_sp = predict(myFitSpX,predX, type = "response")
    
    ## Predire la coordonnée Y
    # Pour un polynome d'ordre 1
    myFitP1Y <- lm(Y_ref ~ poly(Y_ini,1), calibY)
    predY = data.frame("Y_ini" = testY$Y_ini)
    P_Y_p1 = predict(myFitP1Y, predY, type = "response")
    
    # Pour un polynome d'ordre 2
    myFitP2Y <- lm(Y_ref ~ poly(Y_ini,2), calibY)
    P_Y_p2 = predict(myFitP2Y, predY, type = "response")
    
    
    # Pour la méthode spline
    myFitSpY <- lm(Y_ref ~ ns(Y_ini, df = 3),data = calibY)
    P_Y_sp = predict(myFitSpY,predY, type = "response")
    
    #######################################################
    ### Calculer l'erreur et stocker les résultats
    resPol1_XY[[n]] = sqrt(((P_X_p1-testX$X_ref)^2)+((P_Y_p1-testY$Y_ref)^2))  
    resPol2_XY[[n]] = sqrt(((P_X_p2-testX$X_ref)^2)+((P_Y_p2-testY$Y_ref)^2))    
    resSp_XY[[n]] = sqrt(((P_X_sp-testX$X_ref)^2)+((P_Y_sp-testY$Y_ref)^2))
  }
  
  erreur_Pol1_XY = as.matrix(do.call(rbind,resPol1_XY))
  erreur_Pol2_XY = as.matrix(do.call(rbind,resPol2_XY))
  erreur_Sp_XY = as.matrix(do.call(rbind,resSp_XY))
  
  list_Pol1_XY[[i]] = mean(erreur_Pol1_XY)
  list_Pol2_XY[[i]] = mean(erreur_Pol2_XY)
  list_Sp_XY[[i]] = mean(erreur_Sp_XY)
}

# Lancer ces lignes pour chaque effectif
err_pol1 = mean(do.call(rbind,list_Pol1_XY))
sd_pol1 = sd(do.call(rbind,list_Pol1_XY))

err_pol2 = mean(do.call(rbind,list_Pol2_XY))
sd_pol2 = sd(do.call(rbind,list_Pol2_XY))

err_sp = mean(do.call(rbind,list_Sp_XY))
sd_sp = sd(do.call(rbind,list_Sp_XY))

# lancer ces lignes en fonction de l'effectif

data_mean = rbind(c(err_pol1, err_pol2, err_sp)) # eff 10
data_sd = rbind(c(sd_pol1, sd_pol2, sd_sp)) # eff 10

data_mean = rbind(data_mean, (c(err_pol1, err_pol2, err_sp))) # eff 20
data_sd = rbind(data_sd, (c(sd_pol1, sd_pol2, sd_sp))) # eff 20

data_mean = rbind(data_mean, (c(err_pol1, err_pol2, err_sp))) # eff 30
data_sd = rbind(data_sd, (c(sd_pol1, sd_pol2, sd_sp))) # eff 30

data_mean = rbind(data_mean, (c(err_pol1, err_pol2, err_sp))) # eff 70
data_sd = rbind(data_sd, (c(sd_pol1, sd_pol2, sd_sp))) # eff 70

data_mean = rbind(data_mean, (c(err_pol1, err_pol2, err_sp))) # eff 100
data_sd = rbind(data_sd, (c(sd_pol1, sd_pol2, sd_sp))) # eff 100

data_mean = as.data.frame(data_mean)
data_sd = as.data.frame(data_sd)


# changer le nom des colonnes et lignes
rownames(data_mean)= c("eff10","eff20","eff30","eff70","eff100")
rownames(data_sd)= c("eff10","eff20","eff30","eff70","eff100")
colnames(data_mean)= c("err_pol1","err_pol2", "err_sp")
colnames(data_sd)= c("sd_pol1","sd_pol2","sd_sp")

write.csv(data_mean, "data_mean.csv") # stocker les resltats d'erreur
write.csv(data_sd, "data_sd.csv") # stocker les résultats ecart-type


### Représenter les résultats statistiques
## Reporter les résultats pour la méthode polynomiale

M_Pol1_XY = data_mean$err_pol1  ## résultats d'erreur moyenne pour la méthode polynomiale de degré 1, dans l'ordre pour l'effectif 10, 20, ...

S_Pol1_XY = data_sd$sd_pol1 ## résultats d'écart-type pour la méthode polynomiale de degré 1,, dans l'ordre pour l'effectif 10, 20, ...

M_Pol2_XY = data_mean$err_pol2 ## résultats d'erreur moyenne pour la méthode polynomiale de degré 2, dans l'ordre pour l'effectif 10, 20, ...

S_Pol2_XY = data_sd$sd_pol2 ## résultats d'écart-type pour la méthode polynomiale de degré 2, dans l'ordre pour l'effectif 10, 20, ...

## Reporter les résultats pour la méthode Spline
M_Sp_XY = data_mean$err_sp ## résultats d'erreur moyenne pour la méthode splines, dans l'ordre pour l'effectif 10, 20, ..

S_Sp_XY = data_sd$sd_sp ## résultats d'écart-type pour la méthode splines, dans l'ordre pour l'effectif 10, 20, ...

## Assembler les données pour créer une table manipulable dans ggplot
M = data.frame(c(M_Pol1_XY,M_Pol2_XY,M_Sp_XY),c(S_Pol1_XY,S_Pol2_XY,S_Sp_XY),c(10,20,30,70,100,10,20,30,70,100,10,20,30,70,100),c(rep("Polynomiale 1",5),rep("Polynomiale 2",5),rep("Splines",5)))
colnames(M) = c("Mean","Sd","Number","Method")

## Réaliser un graphique en barre avec une barre d'erreur
p1 = ggplot(M,aes(as.factor(Number),Mean, fill = Method))+geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd),width=.2,
                position=position_dodge(.9))+
  xlab("Effectif de points de contrôle") + ylab("Erreur quadratique moyenne (en mètres)")

## La même chose mais en retirant la premier effectif pour rendre le graphique plus visualisable
p2 = ggplot(M[c(2:5,7:10,12:15),],aes(as.factor(Number),Mean, fill = Method))+geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd),width=.2,
                position=position_dodge(.9))+
  xlab("Effectif de points de contrôle") + ylab("Erreur quadratique moyenne (en mètres)")

p3 = grid.arrange(p1,p2) # afficher les résultats côte à côte

