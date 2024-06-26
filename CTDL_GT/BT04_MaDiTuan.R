theKnightsTour <- function(startKnight = "a1", M=8,N=8){
  # Chon vi tri xuat phat
  col <- as.numeric(charToRaw(tolower(substr(startKnight,1,1)))) - 96
  row <- as.numeric(substr(startKnight,2,nchar(startKnight)))
  if(row > M | col > N){stop("Your knight is not on the chess board =/")}
  startKnight <- c(row,col)
  
  # T???o b�n c??? tr???ng
  chess <- matrix(0, nrow = M, ncol = N)
  # Gi� tr??? � hi???n t???i
  getValue <- function(position){chess[position[1], position[2]]}
  # �???t gi� tr???
  setValue <- function(position, x){chess[position[1], position[2]] <<- x}
  
  # C�c b�???c qu�n m?? c� th??? di chuy???n
  knightMoves <- cbind(c(-2,-1), c(-1,-2), c(1,-2), c(2,-1),c(2,1), c(1,2), c(-1,2), c(-2,1))
 
  # 1: B�n c??? ph???i tr???ng
  # 2: C�c n�???c �i ph???i ??? tr�n b�n c??? 
  # 1 <= move: is x v� y ph??? l???n h�n 1
  # move <= c(M,N): n�???c �i ph???i n???m trong b???ng
  valid <- function (move) {all(1 <= move & move <= c(M, N)) && (getValue(move) == 0)}

  # B�???c h???p l???
  possibleMoves <- function (position){
    moves <- position + knightMoves
    cbind(moves[, apply(moves, 2, valid)])
  }
  
  candidates <- function (position) {
    moves <- possibleMoves(position)
    wcosts <- apply(moves, 2, function (position) { ncol(possibleMoves(position)) })
    moves <- cbind(moves[, order(wcosts)])
    moves 
  }
  
  # D�ng �??? quy �??? �i h???t to�n b??? b�n c???
  knightTour = function (position, moveN) {
    # Ho�n th�nh chuy???n �i tu???n
    if (moveN > (M * N)) {
      board <- chess
      board <- as.data.frame(board)
      sortBoard <- rev(order(as.numeric(rownames(board))))
      board <- board[sortBoard,]
      colnames(board) <- letters[1:ncol(board)]
      board <<- board; 
      print(board)
      opt <- options(show.error.messages=FALSE) 
      on.exit(options(opt))
      stop() 
    }
    
    moves = candidates(position) 
    if (ncol(moves) == 0) {return()}
    # Ti???p t???c di chuy???n v� ho�n th�nh chuy???n tham quan
    apply(moves, 2, function (position) {
      setValue(position, moveN)
      knightTour(position, moveN + 1)
      setValue(position, 0)
    })
  }
  setValue(startKnight, 1); knightTour(startKnight, 2)
  print("Kh�ng th??? th???c thi m?? �i tu???n.")
}

theKnightsTour("a1", 6,6)

