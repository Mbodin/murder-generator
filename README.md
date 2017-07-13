# Prelude

To English-speaker: the following is in French, I will translate it when the project gets mature.


# murder-generator

Ce programme est un générateur de soirées enquête aléatoires (et très basiques).
À noter que ce que j’appelle « soirée enquête » est plus communément appellé « huit clos » par la communauté rôliste en général.
Il ne fait en effet que rarement référence à un meurtrier qu’il faudrait retrouver dans ces soirées enquête.

## Prise en main

:construction:

## Objectifs

Les soirées enquête générées ont plusieurs objectifs :
* Aider à la rédaction de soirées enquête. En effet, les scénarios générés n’ont pas pour objectif d’être trop complexes. En particulier, aucun élément sortant vraiment de l’ordinaire des soirées enquêtes (vous ne sortirez pas de la lecture en disant « c’est génial ! ») ne sera présent. Cependant, les relations entre les différents personnages ont pour but d’être équilibrées et intéressantes.
* Avoir des soirées enquêtes personnalisées. Souvent, les joueurs de soirées enquête ont des requêtes très précises sur leur personnage. À tel point que souvent, une soirée enquête toute en main n’est pas suffisante pour satisfaire leur besoin de complexité. Ce programme est paramétré par les envies des différents joueurs, permettant par exemple de donner un personnage plus simple à jouer pour des débutants, ou assuré de mourir rapidement pour quelqu’un qui ne sera pas disponible sur toute la durée de la soirée enquête.
* Permettre d’improviser rapidement une soirée enquête unique entre amis. En bonus, les soirées enquête générées étant aléatoires, il n’y a pas de raison d’avoir peur de se gâcher une soirée enquête connue, par exemple.

## Raisons

Ce logiciel a plusieurs raisons d’existences :
* Écrire une soirée enquête est long. Je n’en ai personnellement plus le temps d’écrire des soirées comme [ce que j’ai pu écrire avant](https://github.com/Mbodin/murders). Mais j’ai toujours des idées, et je trouve dommage que personne ne puisse en profiter. Ce programme me permet (à moi, mais à n’importe qui d’autre, d’ailleurs) d’écrire l’élément de scénario que j’ai en tête sans avoir à écrire un scénario entier : cet élément de scénario sera alors choisi au hasard au fil des générations.
* Écrire une soirée enquête est difficile. En particulier, en tant qu’auteur, j’ai souvent tendance à me focaliser sur l’univers, sur tout ce qui me fait rêver et rire. Souvent, le scénario a tendance à se concentrer sur une ou deux personnes en particulier. Très souvent, il y a deux ou trois personnages de ma soirée enquête qui ne sont que des remplissages : on les ajoute parce que l’on veut un certain nombre de joueur, ou pour brouiller les pistes, mais ces personnages se retrouvent relativement isolés. Le problème, c’est que les relations sont capitales dans une soirée enquête, et je ne suis jamais très content de laisser un joueur à part. Ce générateur essaye exactement d’optimiser ce problème : faire en sorte que tous les joueurs aient des relations intéressantes et équilibrés.

## Fonctionnement

Le programme est associé à une bibliothèque d‘éléments scénaristiques basiques.
Ces éléments scénaristiques changent l’état du scénario, et ajoute des relations entre les personnages.
Le programme va choisir au hasard un élément scénaristique et observer ses conséquences sur les relations des personnages.
Si cela donne des relations intéressantes, le programme va modifier le scénario en ajoutant cet élément.
Le programme continue jusqu’à ce que les relations entre personnages soient suffisantes.

Les relations sont évaluées par deux caractéristiques : la complexité et la difficulté.
La complexité peut être vue comme le nombre de phrase nécessaires à la description de la relation.
La difficulté est associée à l’aide que cette relation donne au personnage.
La complexité et la difficulté d’un personnage est la somme des complexité et difficulté de ses relations avec les autres joueurs.
Si un personnage est associé à une grande complexité, sa fiche de personnage sera probablement longue.
Il est cependant possible que sa fiche de personnage soit simple, mais qu’il découvre en cours de partie que la relation en question est beaucoup plus simple d’apparence (par exemple, dans le cas où la relation est asymétrique — l’un a une très forte idée de qui est le second, mais le second en sait très peu sur le premier —, la complexité de la relation sera la même pour les deux personnages impliqués… sans que les joueurs soient au courant !).
Si un personnage est associé à une grande difficulté, il va probablement mourir durant la partie.
La difficulté est aussi reliée à la quantité de frein qu’un joueur aura pour accomplir quoique ce soit.
Par exemple un personnage avec beaucoup de personnages prêts à l’aider (mais pour des raisons différentes) va avoir une grande complexité (le joueur va devoir mémoriser et gérer toutes ces relations en même temps), mais une faible difficulté (quelle que soit sa situation, il va probablement avoir assez de ressources pour s’en sortir sain et sauf).
Au contraire, un personnage dont très peu de personnages s’intéresse à lui, mais qui est la cible d’un ou plusieurs assassins sera associé à une faible complexité (la fiche de personnage sera facile à lire, et il sera facile pour lui de se présenter aux autres personnages sans avoir à réfléchir à qui il parle exactement), mais une grande difficulté.
Il est possible de spécifier une complexité et difficulté objectif pour chaque personnage en fonction des envies des joueurs.

Un scénario se compose de plusieurs parties :
* Chaque personnage est associé à une histoire, qui décrit comment le personnage est arrivé dans cette situation ou cette façon de penser.
* Chaque personnage est associé à un ensemble de caractéristiques, qui va aider au choix des éléments de scénarios.
* Chaque relation entre personnage est associé à un ensemble de caractéristiques (qui va aussi guider le choix des éléments de scénarios).
* Un ensemble d’objets, définis par leurs caractéristiques et leurs relations avec les personnages. Cette notion d’objet est très vague. On pourra par exemple imaginer une épée magique recherchée par un joueur et détenu par un autre, mais aussi un lieu, une pensée, etc.

Prenons un exemple assez stéréotypé, mais qui fixe les idées.
* On demande une soirée enquête à deux joueurs. C’est un cas rare, mais c’est pour l’exemple.
* Un élément de scénario attribut des genres à des personnages (c’est une caractéristique), il va par exemple dire que le premier joueur est une femme, et l’autre, un homme. Leur histoire va être mise à jour pour leur donner une date de naissance.
* Un autre élément de scénario attribut au second joueur le statut de militaire (c’est une caractéristique). Son histoire va être mise à jour pour indiquer qu’il a participé à une école militaire, qui lui a valu ce titre.
* Un élément de scénario s’active alors (il considère un militaire, il n’avait donc pas de possibilité de s’appliquer avant), et ajoute la caractéristique « chevalier » à ce même joueur. Son histoire va être mise à jour pour indiquer qu’il a été nommé chevalier à l’issue de sa formation militaire.
* Un élément de scénario ajoute la caractéristique « famille royale » au premier joueur. Lors de la génération des fiches de personnages, cela sera probablement traduit en « reine » ou « princesse ».
* Un élément de scénario ajoute une relation entre les deux joueurs : l’un est amoureux de l’autre. Nous avons alors une relation dite de confiance entre les deux joueurs, qui est vue comme une complexité moyenne et une faible difficulté. L’histoire de l’amoureux va être mise à jour pour indiquer quand et pourquoi le personnage est tombé amoureux.
* Un élément de scénario va s’activer par la caractéristique « famille royale » du joueur féminin pour l’associer à un château (un « objet » avec les caractéristiques « lieu » et « château »). L’objet est associé avec le joueur par une relation d’habitation.
Le programme continue ainsi jusqu’à ce que les demandes de complexité et de difficulté des deux joueurs soient respectées.

# License

GPL

:construction: Les scénarios générés devraient être sans licence (comme un compilateur) : vérifier cela…

