# Slitherlink - Application Shiny

Projet universitaire HAX815X — R Programming  
Université de Montpellier — 2025/2026

## Description

Application Shiny interactive pour le jeu logique slitherlink, avec un moteur de génération de puzzles en C++ via Rcpp.

Slitherlink est un casse-tête où l'on doit tracer une boucle fermée unique sur une grille de points, en respectant les contraintes numériques dans les cases.

## Fonctionnalités

- Génération aléatoire de puzzles via un algorithme C++ (croissance de région + extraction du contour)
- 3 niveaux de difficulté : Facile (5x5), Moyen (7x7), Difficile (10x10)
- Interface cliquable pour tracer les arêtes de la boucle
- Chronomètre en temps réel
- Bouton d'indice pour aider le joueur (l'indice donné est un trait)
- Personnalisation des couleurs (traits, chiffres, fond)
- Leaderboard persistant avec sauvegarde des meilleurs temps

## Structure du projet


slitherlink2526/
├── app.R                        # Application Shiny principale
├── R/
│   ├── generate.R               # Wrapper R pour le generateur C++
│   └── utils.R                  # Fonctions utilitaires (victoire, indice)
├── src/
│   └── generateur_boucles.cpp   # Generateur de boucles en C++
├── leaderboard.csv              # Sauvegarde des scores
├── .gitignore
└── README.md


## Prérequis

- R >= 4.0
- Packages R : `shiny`, `Rcpp`, `colourpicker`
- Un compilateur C++ (g++ sous Linux, Rtools sous Windows, ou Xcode Command Line Tools / clang++ sous macOS)

## Installation des dépendances

```r
install.packages(c("shiny", "Rcpp", "colourpicker"))
```

## Lancer l'application

```r
setwd(setwd("C:/Users/nom/Desktop/slitherlink2526"))
shiny::runApp()
```

## Auteurs

- El Mahzoum Akram
- Benameur Karam