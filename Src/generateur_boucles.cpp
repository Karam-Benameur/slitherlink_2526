#include <Rcpp.h>
#include <vector>
#include <queue>
using namespace Rcpp;
using namespace std;

// Vérifie que la zone extérieure (hors boucle) reste d'un seul tenant
// On utilise un BFS depuis une bordure virtuelle autour de la grille 
// BFS=breadth first search (parcours en largeur), un algorithme classique pour explorer un graphe ou une grille case par case, en partant d'un point et en visitant tous ses voisins, puis les voisins des voisins, etc
// Si on ne peut pas atteindre toutes les cases extérieures, c'est qu'il y a un trou et la boucle n'est pas valide
bool outsideConnected(vector<vector<bool>>& inside,int rows,int cols){
  // Grille étendue avec bordure virtuelle (toujours extérieure)
  vector<vector<bool>> visited(rows+2,vector<bool>(cols+2,false));
  queue<pair<int,int>> q;
  q.push(make_pair(0,0));
  visited[0][0]=true;
  int dx[]={0,0,1,-1};
  int dy[]={1,-1,0,0};
  // Parcours en largeur (BFS) de toutes les cases extérieures
  while(!q.empty()){
    int x=q.front().first;
    int y=q.front().second;
    q.pop();
    for(int d=0;d<4;d++){
      int nx=x+dx[d];
      int ny=y+dy[d];
      if(nx<0||nx>=rows+2||ny<0||ny>=cols+2) continue;
      if(visited[nx][ny]) continue;
      // La bordure virtuelle est toujours extérieure
      bool isOutside;
      if(nx==0||nx==rows+1||ny==0||ny==cols+1){
        isOutside=true;
      }else{
        isOutside=!inside[nx-1][ny-1];
      }
      if(isOutside){
        visited[nx][ny]=true;
        q.push(make_pair(nx,ny));
      }
    }
  }
  // Si une case extérieure n'a pas ete visitée, il y a un trou
  for(int i=0;i<rows;i++){
    for(int j=0;j<cols;j++){
      if(!inside[i][j]&&!visited[i+1][j+1]) return false;
    }
  }
  return true;
}

// Génère une boucle slitherlink valide
// Principe: on fait "pousser" une region intérieure depuis le centre
// La boucle=le contour de cette region (frontière intérieur/extérieur)
// [[Rcpp::export]]
List generateLoop(int rows,int cols){
  // Grille de booléens : true=cellule intérieure a la boucle
  vector<vector<bool>> inside(rows,vector<bool>(cols,false));
  // On démarre la croissance depuis le centre
  int startR=rows/2;
  int startC=cols/2;
  inside[startR][startC]=true;
  vector<pair<int,int>> insideCells;
  insideCells.push_back(make_pair(startR,startC));
  int dx[]={0,0,1,-1};
  int dy[]={1,-1,0,0};
  // On vise entre 35% et 55% de cellules intérieures
  int targetSize=(int)(rows*cols*R::runif(0.35,0.55));
  if(targetSize<4) targetSize=4;
  int attempts=0;
  int maxAttempts=rows*cols*20;
  // Croissance aléatoire : on ajoute des voisins un par un
  while((int)insideCells.size()<targetSize&&attempts<maxAttempts){
    attempts++;
    // Choisir une cellule intérieure au hasard
    int idx=(int)floor(R::runif(0,insideCells.size()));
    int r=insideCells[idx].first;
    int c=insideCells[idx].second;
    // Choisir une direction au hasard (haut/bas/gauche/droite)
    int d=(int)floor(R::runif(0,4));
    int nr=r+dx[d];
    int nc=c+dy[d];
    if(nr<0||nr>=rows||nc<0||nc>=cols) continue;
    if(inside[nr][nc]) continue;
    // On teste l'ajout : si ca crée un trou on annule
    inside[nr][nc]=true;
    if(outsideConnected(inside,rows,cols)){
      insideCells.push_back(make_pair(nr,nc));
    }else{
      inside[nr][nc]=false;
    }
  }
  // Extraction des arêtes horizontales (frontière haut/bas entre 2 cases)
  IntegerMatrix h_edges(rows+1,cols);
  for(int i=0;i<=rows;i++){
    for(int j=0;j<cols;j++){
      bool above=(i>0)?inside[i-1][j]:false;
      bool below=(i<rows)?inside[i][j]:false;
      h_edges(i,j)=(above!=below)?1:0; // arête si changement int/ext
    }
  }
  // Extraction des arêtes verticales (frontière gauche/droite entre 2 cases)
  IntegerMatrix v_edges(rows,cols+1);
  for(int i=0;i<rows;i++){
    for(int j=0;j<=cols;j++){
      bool leftCell=(j>0)?inside[i][j-1]:false;
      bool rightCell=(j<cols)?inside[i][j]:false;
      v_edges(i,j)=(leftCell!=rightCell)?1:0;
    }
  }
  // Calcul des indices : nb de côtés de chaque case qui font partie de la boucle
  IntegerMatrix clues(rows,cols);
  for(int i=0;i<rows;i++){
    for(int j=0;j<cols;j++){
      clues(i,j)=h_edges(i,j)+h_edges(i+1,j)+v_edges(i,j)+v_edges(i,j+1);
    }
  }
  return List::create(Named("h_edges")=h_edges,Named("v_edges")=v_edges,Named("clues")=clues);
}