# Compte rendu SY01


## Exercice 1, Simulation d'une loi discrète

### Question 1
Pour exprimer X en fonction de Ui, nous allons utiliser le fait que Ui suit une loie uniforme continue sur [0,1]. X est donc égal à la somme de 1 à n, de chaque variable Ui dans la fonction indicatrice qui vaut 1 si la condition Ui <= p est vraie et qui vaut 0 sinon. X compte donc le nombre de Ui pour lesquels Ui <= p, ce qui correspond à une distribution binomiale.
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




