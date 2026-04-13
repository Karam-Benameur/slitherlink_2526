library(Rcpp)
sourceCpp("src/generateur_boucles.cpp")

# Génère un puzzle Slitherlink jouable
# 1) On génère une boucle fermée valide via le C++
# 2) On récupère les indices (nb de cotés de chaque case sur la boucle)
# 3) On masque un % d'indices selon la difficulté pour créer le défi

generate_puzzle=function(rows,cols,difficulty="facile"){
 
   # Appel au générateur C++ qui renvoie la boucle et les indices
  result=generateLoop(rows,cols)
  
  # Plus la difficulté augmente, plus on masque d'indices
  mask_rate=switch(difficulty,"facile"= 0.3,"moyen"= 0.5,"difficile" = 0.7,0.3)
  
  clues=result$clues
  n_cells=rows*cols
  n_mask=round(n_cells*mask_rate)
  # On choisit aléatoirement quelles cases masquer
  mask_indices=sample(n_cells,n_mask)
  
  visible=clues
  visible[mask_indices]=NA   # NA=case sans indice visible pour le joueur
  
  list(h_edges=result$h_edges,v_edges=result$v_edges,clues=clues,visible=visible)}