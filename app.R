library(shiny)
library(Rcpp)
library(colourpicker) # Ajout d'une librairie

source("R/generate.R") # On importe notre script annexe qui contient la logique de création de la grille
source("R/utils.R") # On importe le script annexe qui s'occupe de la logique de création des indices

# Chemin vers le fichier CSV qui stocke les scores
LEADERBOARD_FILE="leaderboard.csv"

# Charge le leaderboard depuis le CSV et le trie par temps croissant
load_leaderboard=function(){
  if(file.exists(LEADERBOARD_FILE)){
    df=read.csv(LEADERBOARD_FILE,stringsAsFactors=FALSE)
    if(nrow(df)==0) return(df)
    return(df[order(df$temps_sec),])
  }
  data.frame(joueur=character(),difficulte=character(),
             temps_sec=numeric(),date=character(),stringsAsFactors=FALSE)
}

# Sauvegarde un nouveau score dans le CSV
save_score=function(joueur,difficulte,temps_sec){
  new_row=data.frame(joueur=joueur,difficulte=difficulte,
                     temps_sec=round(temps_sec,1),
                     date=format(Sys.time(),"%Y-%m-%d %H:%M"),stringsAsFactors=FALSE)
  if(file.exists(LEADERBOARD_FILE)){
    df=read.csv(LEADERBOARD_FILE,stringsAsFactors=FALSE)
    df=rbind(df,new_row)
  }else{
    df=new_row
  }
  write.csv(df,LEADERBOARD_FILE,row.names=FALSE)
}

# ==========================================
# ui : la fonction qui gère l'interface utilisateur
# ==========================================

ui=fluidPage(
  titlePanel("Slitherlink"),
  sidebarLayout(
    sidebarPanel(width=3,
                 selectInput("difficulty","Difficulté",
                             choices=c("Facile (5x5)"="facile","Moyen (7x7)"="moyen","Difficile (10x10)"="difficile")),
                 actionButton("new_game","Nouvelle Partie",style="width:100%; font-weight:bold;"),
                 hr(),
                 actionButton("hint_btn","Indice",style="width:100%;"),
                 hr(),
                 h4("Chronomètre"),
                 textOutput("timer_display"),
                 tags$style("#timer_display {font-size:24px;font-weight:bold;text-align:center;}"),
                 hr(),
                 h4("Personnalisation"),
                 colourInput("col_lines","Couleur des traits",value="#0000FF"),
                 colourInput("col_numbers","Couleur des chiffres",value="#000000"),
                 colourInput("col_bg","Couleur du fond",value="#FFFFFF"),
                 hr(),
                 helpText("Cliquez sur les arêtes entre les points pour tracer la boucle.")
    ),
    mainPanel(width=9,
              # Onglets pour séparer le jeu et le leaderboard
              tabsetPanel(
                tabPanel("Jeu",
                         # On capture les clics de souris
                         plotOutput("grid",click="grid_click",height="600px",width="600px")
                ),
                # Onglet leaderboard avec tableau des meilleurs scores
                tabPanel("Leaderboard",
                         br(),
                         actionButton("refresh_lb","Rafraîchir"),
                         br(),br(),
                         tableOutput("leaderboard_table")
                )
              )
    )
  )
)

# ==========================================
# serveur : la fonction qui gère le backend
# ==========================================

server=function(input,output,session){
  
  # game est notre "mémoire", toute modification d'une variable ici forcera l'UI à se mettre à jour
  game=reactiveValues(puzzle=NULL,rows=5,cols=5,h_player=NULL,v_player=NULL,
                      start_time=NULL,game_won=FALSE,end_time=NULL)
  
  # Fonction pour initialiser une nouvelle grille vierge
  start_game=function(difficulty="facile"){
    dims=switch(difficulty,"facile"=c(5,5),"moyen"=c(7,7),"difficile"=c(10,10))
    game$rows=dims[1]
    game$cols=dims[2]
    # Appel à notre fonction externe (dans generate.R) pour créer le niveau
    game$puzzle=generate_puzzle(dims[1],dims[2],difficulty)
    # Matrices qui stockent les traits du joueur. 0 = vide, 1 = trait tracé
    # h_player gère les lignes horizontales (il y a une ligne de plus que de cases)
    game$h_player=matrix(0,nrow=dims[1]+1,ncol=dims[2])
    # v_player gère les lignes verticales (il y a une colonne de plus que de cases)
    game$v_player=matrix(0,nrow=dims[1],ncol=dims[2]+1)
    game$start_time=Sys.time()
    game$game_won=FALSE
    game$end_time=NULL
  }
  
  # Au lancement de l'app, on force la création d'une partie facile par défaut
  observe({if(is.null(game$puzzle)) start_game("facile")})
  
  # Si l'utilisateur clique sur le bouton "Nouvelle partie", on relance start_game()
  observeEvent(input$new_game,{start_game(input$difficulty)})
  
  output$timer_display=renderText({
    invalidateLater(1000,session)
    if(is.null(game$start_time)) return("00:00")
    if(game$game_won) elapsed=as.numeric(difftime(game$end_time,game$start_time,units="secs"))
    else elapsed=as.numeric(difftime(Sys.time(),game$start_time,units="secs"))
    mins=floor(elapsed/60)
    secs=floor(elapsed%%60)
    sprintf("%02d:%02d",mins,secs)
  })
  
  # Fonction appelée quand le joueur gagne
  # Affiche le temps et propose de sauvegarder le score dans le leaderboard
  handle_victory=function(){
    game$game_won=TRUE
    game$end_time=Sys.time()
    elapsed=round(as.numeric(difftime(game$end_time,game$start_time,units="secs")),1)
    showModal(modalDialog(
      title="Bravo ! (j'aurais fait mieux mais bon)",
      paste0("Puzzle résolu en ",floor(elapsed/60)," min ",floor(elapsed%%60)," sec !"),
      textInput("winner_name","Votre nom :",value="Joueur"),
      footer=tagList(
        actionButton("save_score_btn","Sauvegarder le score"),
        modalButton("Fermer")
      )
    ))
  }
  
  # Quand le joueur clique "Sauvegarder", on écrit dans le CSV
  observeEvent(input$save_score_btn,{
    elapsed=round(as.numeric(difftime(game$end_time,game$start_time,units="secs")),1)
    save_score(input$winner_name,input$difficulty,elapsed)
    removeModal()
  })
  
  # Gestion du bouton indice
  # On cherche une arête incorrecte ou manquante et on la corrige pour aider le joueur
  observeEvent(input$hint_btn,{
    if(is.null(game$puzzle)||game$game_won) return()
    hint=get_hint(game$puzzle,game$h_player,game$v_player)
    if(is.null(hint)){
      showModal(modalDialog(title="Aucun indice","Votre solution est déjà complète !",easyClose=TRUE))
      return()
    }
    if(hint$action=="add"){
      if(hint$type=="h") game$h_player[hint$i,hint$j]=1
      else game$v_player[hint$i,hint$j]=1
    }else{
      if(hint$type=="h") game$h_player[hint$i,hint$j]=0
      else game$v_player[hint$i,hint$j]=0
    }
    # On vérifie si l'indice a permis de terminer le puzzle
    if(check_victory(game$puzzle,game$h_player,game$v_player)) handle_victory()
  })
  
  # ==========================================
  # GESTION DES CLICS SUR LA GRILLE
  # ==========================================
  observeEvent(input$grid_click,{
    if(is.null(game$puzzle)||game$game_won) return()
    
    # Récupération des coordonnées exactes du clic de souris
    click_x=input$grid_click$x
    click_y=input$grid_click$y
    rows=game$rows
    cols=game$cols
    
    # Variables pour trouver l'arête la plus proche du clic
    best_dist=Inf
    best_type=""
    best_i=0
    best_j=0
    
    # On teste la distance avec toutes les arêtes HORIZONTALES
    for(i in 0:rows){
      for(j in 0:(cols-1)){
        mx=j+0.5   # Coordonnée X du milieu de l'arête
        my=rows-i  # Coordonnée Y du milieu de l'arête
        # Calcul de distance (Pythagore)
        d=sqrt((click_x-mx)^2+(click_y-my)^2)
        if(d<best_dist){best_dist=d;best_type="h";best_i=i+1;best_j=j+1}
      }
    }
    
    # On teste la distance avec toutes les arêtes VERTICALES
    for(i in 0:(rows-1)){
      for(j in 0:cols){
        mx=j
        my=rows-i-0.5
        d=sqrt((click_x-mx)^2+(click_y-my)^2)
        if(d<best_dist){best_dist=d;best_type="v";best_i=i+1;best_j=j+1}
      }
    }
    
    # Si le clic est suffisamment proche d'une arête (tolérance de 0.4)
    if(best_dist<0.4){
      # permet de basculer de 0 à 1 (tracer) ou de 1 à 0 (effacer) après un clic suffisamment proche
      if(best_type=="h") game$h_player[best_i,best_j]=1-game$h_player[best_i,best_j]
      else game$v_player[best_i,best_j]=1-game$v_player[best_i,best_j]
      # Après chaque clic on vérifie si le joueur a terminé le puzzle
      if(check_victory(game$puzzle,game$h_player,game$v_player)) handle_victory()
    }
  })
  
  # ==========================================
  # DESSIN DE LA GRILLE
  # ==========================================
  output$grid=renderPlot({
    if(is.null(game$puzzle)) return()
    rows=game$rows
    cols=game$cols
    # Marges réduites + couleur de fond choisie par le joueur
    par(mar=c(1,1,1,1),bg=input$col_bg)
    # Initialisation de la toile vierge
    plot(NULL,xlim=c(-0.5,cols+0.5),ylim=c(-0.5,rows+0.5),asp=1,xlab="",ylab="",axes=FALSE)
    rect(-0.5,-0.5,cols+0.5,rows+0.5,col=input$col_bg,border=NA)
    # Dessin des traits horizontaux du joueur (couleur personnalisable)
    for(i in 1:(rows+1)){
      for(j in 1:cols){
        if(game$h_player[i,j]==1)
          segments(j-1,rows-(i-1),j,rows-(i-1),lwd=3,col=input$col_lines)
      }
    }
    # Dessin des traits verticaux du joueur (couleur personnalisable)
    for(i in 1:rows){
      for(j in 1:(cols+1)){
        if(game$v_player[i,j]==1)
          segments(j-1,rows-(i-1),j-1,rows-i,lwd=3,col=input$col_lines)
      }
    }
    # Dessin de tous les points noirs (la grille de base)
    for(i in 0:rows){
      for(j in 0:cols) points(j,rows-i,pch=19,cex=1.2,col="black")
    }
    # Affichage des numéros indices au centre des cases (couleur personnalisable)
    visible=game$puzzle$visible
    for(i in 1:rows){
      for(j in 1:cols){
        if(!is.na(visible[i,j]))
          text(j-0.5,rows-i+0.5,visible[i,j],cex=1.5,col=input$col_numbers,font=2)
      }
    }
  })
  
  # Affichage du tableau des scores, se rafraichit au clic ou après une sauvegarde
  output$leaderboard_table=renderTable({
    input$refresh_lb
    input$save_score_btn
    df=load_leaderboard()
    if(nrow(df)==0) return(data.frame(Message="Aucun score enregistré"))
    df
  })
}

shinyApp(ui=ui,server=server)