# Moteur-3D-Caml
Juste un petit challenge pour le fun de coder un moteur 3D en Caml

## Fonctionnement :
On dispose de la fonction qui colorie des rectangles en Caml, donc pour chaque triangle on regarde la projection sur un écran devant nous pour afficher les objets 3D sur l'écran 2D

##Comprendre le code :
La fonction game gère globalement l'affichage, il récupère d'abord l'entrée du clavier (et donc ne fais rien si aucune touche n'est pressée) et déplace le personnage virtuel en conséquence. Voici les commandes sur un clavier azerty :
+ qd : se déplacer de gauche à droite
+ zs : se déplacer de haut en bas
+ ik : se déplacer vers l'avant ou vers l'arrière
+ ol : tourner son regard vers la gauche ou vers la droite (ça marche pas comme ça devrais ^^)
+ pm : tourner son regarde vers le haut ou le bas (ça marche pas comme ça devrais ^^)
+ n : enclencher un saut (je trouvais ça cool <3)

##Todo :
Le programme affiche tout, pas que les objets devant, donc il met un temps fou ^^
Donc faudrait faire toutes les gestions de collisions pour l'affichage.
Si quelqu'un veut plus d'explication je serais ravi de les fournir :)
