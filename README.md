# Compte rendu SY01


## Exercice 1, Simulation d'une loi discrète

### Question 1
Pour exprimer X en fonction de Ui, nous allons utiliser le fait que Ui suit une loi uniforme continue sur [0,1]. X est donc égal à la somme de 1 à n, de chaque variable Ui dans la fonction indicatrice qui vaut 1 si la condition Ui <= p est vraie et qui vaut 0 sinon. X compte donc le nombre de Ui pour lesquels Ui <= p, ce qui correspond à une distribution binomiale.
```
X = Σ I(Ui <= p), somme de i=1 à n, avec I la fonction indicatrice qui vaut 1 si la condition est respectée.
```

### Question 2
Créons la fonction ```bin(p,n)``` de paramètre p et n, qui permet de simuler une série de ```n``` essais binomiaux, où chaque essai a une probabilité de succès ```p```, et elle renvoie le nombre total de succès observés.
```
bin <- function(p, n) {
  sum(runif(n) < p)
}
```

### Question 3
La fonction ```histobin(s,p,n)``` génère un histogramme des échantillons de la loi binomiale avec les paramètres spécifiés et superpose notre résultat avec la courbe de la densité de probabilité théorique de la même distribution. Cela nous permet de visualiser à quel point les échantillons générés correspondent à la distribution théorique.

La ligne ```curve(dbinom(x, n, p), add = TRUE)``` ajoute la courbe de densité de probabilité théorique à l'histogramme pour faciliter la comparaison entre notre distribution et la distribution théorique.
```
histobin <- function(s, p, n) {
  echantillons <- replicate(s, bin(p, n))
  hist(echantillons, probability = TRUE, main = paste("Histogramme représentant la distribution binomiale avec n =", n, "et p =", p))
  curve(dbinom(x, n, p), add = TRUE)
}
```

La ligne ```histobin(100, 0.75, 1000)``` renvoie l'histogramme suivant : 

![image](https://github.com/dev-err418/sy01/assets/59390256/9cf278b2-001f-4b6e-a499-c0410fe0d88f)



### Question 4
Il suffit juste de modifier quelques lignes de nos fonctions pour faire la même simulation avec la loi géométrique.
Commençons par réécrire notre fonction ```bin(p,n)``` et à l'adapter pour une fonction géométrique. La loi géométrique n'a que deux résultats possibles le succès ou l'échec avec une probabilité de succès constante. Il nous suffit donc de compter le nombre d'echecs nécessaires pour obtenir le premier succès.
```
geom <- function(p) {
  k <- 0
  while (runif(1) >= p) {
    k <- k + 1
  }
  return(k)
}
```
Notre fonction ici compte le nombre d'échecs avec la condition ```runif(1) >= p```, tant que cette condition est vraie, on incrémente la variable ```k```. Si l'on obtient un succès (donc on sort de la boucle car la condition ```runif(1) >= p``` n'est pas respectée), on renvoie la varible k.

Nous allons maintenant réécrire notre fonction ```histobin(s,p,n)``` pour qu'elle fonctionne avec notre nouvelle fonction ```geom(p)```.
```
histogeom <- function(s, p) {
  echantillons <- replicate(s, geom(p))
  hist(echantillons, probability = TRUE, main = paste("Histogramme représentant la distribution géométrique avec p =", p))
  x <- seq(0, max(echantillons) + 1)
  y <- dgeom(x, p)
  lines(x, y, col = "blue", type = "h", lwd = 2)
  legend("topright", legend = c("Observé", "Théorique"), fill = c("lightblue", "blue"))
}
```
Cette fonction marche de la même manière que ```histobin(s,p,n)``` : elle génèrere un histogramme et supperpose la courbe de densité de probabilité théorique de la loi géométrique avec la probabilité p spécifiée.

La ligne ```histogeom(10000, 0.3)``` renvoie l'histogramme suivant : 

![image](https://github.com/dev-err418/sy01/assets/59390256/0ef7c915-4a63-4165-be07-fbef7ea65d8a)


### Question 5

Créons la fonction ```uniforme_discret(n)``` qui va simuler la loi uniforme sur [1, n].
```
uniforme_discret <- function(n) {
  floor(runif(1) * n) + 1
}
```
On utilise la fonction ```floor(a)``` pour eviter d'avoir des décimaux (ex: ```floor(3.4) = 3```). Le ```+1``` à la fin permet que l'on soit dans l'intervalle [1, n], car dans le cas contraire on serait dans l'intervalle [0, n-1] (car la fonction floor fait des troncatures).

Si nous voulons faire une illustration graphique, nous pouvons utiliser les deux lignes suivantes : 
```
echantillons <- replicate(10000, uniforme_discret(n))
hist(echantillons, breaks = 10, main = "Histogramme de la loi uniforme discrète")
```

Simulation pour la ligne ```hist(echantillons, breaks = 10, main = "Loi uniforme discrète n=10")```, (n=10).

![image](https://github.com/dev-err418/sy01/assets/59390256/d13964b4-571b-44fc-b0a9-268300c51564)


Simulation pour la ligne ```hist(echantillons, breaks = 10, main = "Loi uniforme discrète n=25")```, (n=25).
![image](https://github.com/dev-err418/sy01/assets/59390256/022b47d1-021e-46d0-815c-c5321b640fe1)


## Exercice 3, Marche aléatoires

### Question 11

```A0``` vaut 0 car nous commençons à la position ```x=0``` et nous n'avons effectué aucun mouvement. La loi de probabilité de ```A0``` est constante (probabilité certaine) car elle vaut toujours 0.
Voici la fonction ```simu_pas(n)``` qui permet de simuler la position de notre points après ```n``` pas. 

```
simu_pas = function(n) {
    pas = sample(c(-1, 1), n, replace = TRUE)
    trajectoire = cumsum(pas)
    for (i in 1:n) { #i varie de 1 à n
      cat("Après", i, "pas :", trajectoire[i], "\n")
    }
  }
```
```simu_pas(n)``` nous renvoie ```2```. Donc sur cette simulation, la position après 10 pas est de ```x=2```.

### Question 12

Voici le code que nous allons utiliser pour simuler ```A1, A2, A3, A4 et A5```. Nous allons faire 100000 simulations pour verifier nos lois de probabilité.

```
simul_pas = function(n){
    sum(sample(c(-1, 1), n, replace = TRUE))
}

table(A1) / 100000
table(A2) / 100000
table(A3) / 100000
table(A4) / 100000
table(A5) / 100000

A1<-replicate(100000,simul_pas(1))
A2<-replicate(100000,simul_pas(2))
A3<-replicate(100000,simul_pas(3))
A4<-replicate(100000,simul_pas(4))
A5<-replicate(100000,simul_pas(5))
```

Voici le résultat pour nos valeurs de Ai :
```
A1
     -1       1 
0.50021 0.49979 
A2
     -2       0       2 
0.24930 0.49838 0.25232 
A3
     -3      -1       1       3 
0.12285 0.37701 0.37567 0.12447 
A4
     -4      -2       0       2       4 
0.06283 0.24967 0.37483 0.25209 0.06058 
A5
     -5      -3      -1       1       3       5 
0.03144 0.15682 0.31430 0.31231 0.15386 0.03127 
```

Exemple de graphique pour la répartion de A5 :

![image](https://github.com/dev-err418/sy01/assets/59390256/64d21d5a-3faa-4d7e-b186-43d639dd639e)

### Question 13

Nous allons maintenant calculer la positions moyenne après avoir fait ```N``` pas. La position moyenne est 0. 

```Moyenne(An) = Moyenne(A1 + A2 + ... + An) = Moyenne(A1) + Moyenne(A2) + ... + Moyenne(An) = 0 + 0 + ... + 0 = 0```

Utilisons le code suivant pour faire une simulation.

```
simulation_marche = function(N, p) {
    pas_bis = sample(c(-1, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    return(sum(pas_bis))
}
 
N = 30
p = 0.5
 
simulations = replicate(100000, simulation_marche(N, p))
position_moy = mean(simulations)
cat("Position moyenne après", N, "pas:", position_moy)
hist(simulations,col="red", main = "Distribution des positions finales après N pas", xlab = "Position finale", ylab = "Fréquence") 
```

Le graphique de répartition renvoyé par le code du-dessus pour 100000 simulations de 30 pas est le suivant :

![image](https://github.com/dev-err418/sy01/assets/59390256/4b49f519-3847-4c6e-9635-d56106c523b0)


### Question 14

```An``` suit une loie normale symétrique, centrée à 0.
Nous pouvons l'illustrer avec le graphique suivant ```(pour N=30, p=0.5 et le nombre de simulation = 100000)``` :

![image](https://github.com/dev-err418/sy01/assets/59390256/cc05f8fa-16bc-496a-bc5a-ab7129354966)

Avec les batons bleus qui correspondent à nos simulation et la courbe rouge qui correspond à une distribution normale.
Voici le code utilisé : 
```
simulation_marche = function(N, p) {
    pas_bis = sample(c(-1, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    return(sum(pas_bis))
}

N = 30
p = 0.5

simulations = replicate(100000, simulation_marche(N, p))
position_moy = mean(simulations)
cat("Position moyenne après", N, "pas:", position_moy)

hist(simulations, col="blue", main="Distribution des positions finales après N pas", xlab="Position finale", ylab="Fréquence", prob=TRUE)

mu = mean(simulations)
sigma = sd(simulations)
x = seq(min(simulations), max(simulations), length=100)
y = dnorm(x, mean=mu, sd=sigma)
curve(dnorm(x, mean=mu, sd=sigma), col="red", lwd=2, add=TRUE)
```





