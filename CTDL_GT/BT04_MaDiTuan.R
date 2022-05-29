theKnightsTour <- function(startKnight = "a1", M=8,N=8){
  # Chon vi tri xuat phat
  col <- as.numeric(charToRaw(tolower(substr(startKnight,1,1)))) - 96
  row <- as.numeric(substr(startKnight,2,nchar(startKnight)))
  if(row > M | col > N){stop("Your knight is not on the chess board =/")}
  startKnight <- c(row,col)
  
  # T???o bàn c??? tr???ng
  chess <- matrix(0, nrow = M, ncol = N)
  # Giá tr??? ô hi???n t???i
  getValue <- function(position){chess[position[1], position[2]]}
  # Ð???t giá tr???
  setValue <- function(position, x){chess[position[1], position[2]] <<- x}
  
  # Các bý???c quân m?? có th??? di chuy???n
  knightMoves <- cbind(c(-2,-1), c(-1,-2), c(1,-2), c(2,-1),c(2,1), c(1,2), c(-1,2), c(-2,1))
 
  # 1: Bàn c??? ph???i tr???ng
  # 2: Các ný???c ði ph???i ??? trên bàn c??? 
  # 1 <= move: is x và y ph??? l???n hõn 1
  # move <= c(M,N): ný???c ði ph???i n???m trong b???ng
  valid <- function (move) {all(1 <= move & move <= c(M, N)) && (getValue(move) == 0)}

  # Bý???c h???p l???
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
  
  # Dùng ð??? quy ð??? ði h???t toàn b??? bàn c???
  knightTour = function (position, moveN) {
    # Hoàn thành chuy???n ði tu???n
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
    # Ti???p t???c di chuy???n và hoàn thành chuy???n tham quan
    apply(moves, 2, function (position) {
      setValue(position, moveN)
      knightTour(position, moveN + 1)
      setValue(position, 0)
    })
  }
  setValue(startKnight, 1); knightTour(startKnight, 2)
  print("Không th??? th???c thi m?? ði tu???n.")
}

theKnightsTour("a1", 6,6)

