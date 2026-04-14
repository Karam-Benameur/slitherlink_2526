check_victory=function(puzzle,h_player,v_player){
  all(puzzle$h_edges==h_player)&&all(puzzle$v_edges==v_player)
}

get_hint=function(puzzle,h_player,v_player){
  candidates=list()
  for(i in 1:nrow(puzzle$h_edges)){
    for(j in 1:ncol(puzzle$h_edges)){
      if(puzzle$h_edges[i,j]==1&&h_player[i,j]==0)
        candidates[[length(candidates)+1]]=list(type="h",i=i,j=j,action="add")
    }
  }
  for(i in 1:nrow(puzzle$v_edges)){
    for(j in 1:ncol(puzzle$v_edges)){
      if(puzzle$v_edges[i,j]==1&&v_player[i,j]==0)
        candidates[[length(candidates)+1]]=list(type="v",i=i,j=j,action="add")
    }
  }
  for(i in 1:nrow(h_player)){
    for(j in 1:ncol(h_player)){
      if(puzzle$h_edges[i,j]==0&&h_player[i,j]==1)
        candidates[[length(candidates)+1]]=list(type="h",i=i,j=j,action="remove")
    }
  }
  for(i in 1:nrow(v_player)){
    for(j in 1:ncol(v_player)){
      if(puzzle$v_edges[i,j]==0&&v_player[i,j]==1)
        candidates[[length(candidates)+1]]=list(type="v",i=i,j=j,action="remove")
    }
  }
  if(length(candidates)>0) return(candidates[[sample(length(candidates),1)]])
  return(NULL)
}