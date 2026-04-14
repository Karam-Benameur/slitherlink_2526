# Vérifie si le joueur a gagné en comparant ses traits avec la solution
# On compare les arêtes horizontales et verticales du joueur avec celles de la boucle
check_victory=function(puzzle,h_player,v_player){
  all(puzzle$h_edges==h_player)&&all(puzzle$v_edges==v_player)
}

# Donne un indice au joueur : cherché une arête à ajouter ou à retirer
# On parcourt toutes les arêtes et on repère les différences avec la solution
get_hint=function(puzzle,h_player,v_player){
  candidates=list()
  # Arêtes horizontales manquantes : la solution dit 1 mais le joueur n'a rien tracé
  for(i in 1:nrow(puzzle$h_edges)){
    for(j in 1:ncol(puzzle$h_edges)){
      if(puzzle$h_edges[i,j]==1&&h_player[i,j]==0)
        candidates[[length(candidates)+1]]=list(type="h",i=i,j=j,action="add")
    }
  }
  # Arêtes verticales manquantes
  for(i in 1:nrow(puzzle$v_edges)){
    for(j in 1:ncol(puzzle$v_edges)){
      if(puzzle$v_edges[i,j]==1&&v_player[i,j]==0)
        candidates[[length(candidates)+1]]=list(type="v",i=i,j=j,action="add")
    }
  }
  # Arêtes horizontales en trop : le joueur a tracé mais la solution dit 0
  for(i in 1:nrow(h_player)){
    for(j in 1:ncol(h_player)){
      if(puzzle$h_edges[i,j]==0&&h_player[i,j]==1)
        candidates[[length(candidates)+1]]=list(type="h",i=i,j=j,action="remove")
    }
  }
  # Arêtes verticales en trop
  for(i in 1:nrow(v_player)){
    for(j in 1:ncol(v_player)){
      if(puzzle$v_edges[i,j]==0&&v_player[i,j]==1)
        candidates[[length(candidates)+1]]=list(type="v",i=i,j=j,action="remove")
    }
  }
  # On choisit un indice au hasard parmi toutes les erreurs trouvées
  if(length(candidates)>0) return(candidates[[sample(length(candidates),1)]])
  # Si aucune erreur, le puzzle est déjà complet
  return(NULL)
}