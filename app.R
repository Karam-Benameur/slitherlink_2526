library(shiny)
library(Rcpp)
# On importe notre script annexe qui contient la logique de création de la grille
source("R/generate.R")

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
                 helpText("Cliquez sur les arêtes entre les points pour tracer la boucle.")
    ),
    mainPanel(width=9,
              # On capture les clics de souris
              plotOutput("grid",click="grid_click",height="600px",width="600px")
    )
  )
)

# ==========================================
# serveur : la fonction qui gère le backend
# ==========================================

server=function(input,output,session){
  
  # game est notre "mémoire", toute modification d'une variable ici forcera l'UI à se mettre à jour
  game=reactiveValues(puzzle=NULL,rows=5,cols=5,h_player=NULL,v_player=NULL)
  
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
  }
  
  # Au lancement de l'app, on force la création d'une partie facile par défaut
  observe({if(is.null(game$puzzle)) start_game("facile")})
  
  # Si l'utilisateur clique sur le bouton "Nouvelle partie", on relance start_game()
  observeEvent(input$new_game,{start_game(input$difficulty)})
  
  # ==========================================
  # GESTION DES CLICS SUR LA GRILLE
  # ==========================================
  observeEvent(input$grid_click,{
    if(is.null(game$puzzle)) return()
    
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
    }
  })
  
  # ==========================================
  # DESSIN DE LA GRILLE 
  # ==========================================
  output$grid=renderPlot({
    if(is.null(game$puzzle)) return()
    rows=game$rows
    cols=game$cols
    
    # Marges réduites
    par(mar=c(1,1,1,1))
    
    # Initialisation de la toile vierge 
    plot(NULL,xlim=c(-0.5,cols+0.5),ylim=c(-0.5,rows+0.5),asp=1,xlab="",ylab="",axes=FALSE)
    
    # Dessin des traits horizontaux du joueur
    for(i in 1:(rows+1)){
      for(j in 1:cols){
        if(game$h_player[i,j]==1)
          segments(j-1,rows-(i-1),j,rows-(i-1),lwd=3,col="blue")
      }
    }
    
    # Dessin des traits verticaux du joueur
    for(i in 1:rows){
      for(j in 1:(cols+1)){
        if(game$v_player[i,j]==1)
          segments(j-1,rows-(i-1),j-1,rows-i,lwd=3,col="blue")
      }
    }
    
    # Dessin de tous les points noirs (la grille de base)
    for(i in 0:rows){
      for(j in 0:cols) points(j,rows-i,pch=19,cex=1.2,col="black")
    }
    
    # Affichage des numéros indices au centre des cases
    visible=game$puzzle$visible
    for(i in 1:rows){
      for(j in 1:cols){
        if(!is.na(visible[i,j]))
          text(j-0.5,rows-i+0.5,visible[i,j],cex=1.5,col="black",font=2)
      }
    }
  })
}

shinyApp(ui=ui,server=server)