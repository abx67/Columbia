drawBoard = function(board) {
  R1 = board$R1
  R2 = board$R2
  R3 = board$R3
  R4 = board$R4
  R5 = board$R5
  R = board$R
  nums = board$nums
  
  mar.orig = par()$mar
  par(mar = c(0, 0, 0, 0))
  plot(c(), c(), axes = FALSE, xlim = c(-R - 15, R + 15), 
       ylim = c(-R - 15, R + 15))
  t = seq(0, 2 * pi, length = 5000)
  x = cos(t)
  y = sin(t)
  points(R * x, R * y, type = "l")
  points(R5 * x, R5 * y, type = "l")
  points(R4 * x, R4 * y, type = "l")
  points(R3 * x, R3 * y, type = "l")
  points(R2 * x, R2 * y, type = "l")
  points(R1 * x, R1 * y, type = "l")
  t0 = pi/2 + 2 * pi/40
  points(c(R2 * cos(t0), R * cos(t0)), c(R2 * sin(t0), 
                                         R * sin(t0)), type = "l")
  for (i in 1:19) {
    t1 = t0 - i * 2 * pi/20
    points(c(R2 * cos(t1), R * cos(t1)), c(R2 * sin(t1), 
                                           R * sin(t1)), type = "l")
  }
  
  r = R + 10
  for (i in 1:20) {
    t1 = pi/2 - (i - 1) * 2 * pi/20
    text(r * cos(t1), r * sin(t1), nums[i])
  }
  
  par(mar=mar.orig)
  invisible()
}


board = list(
  R1 = 6.35, # center to double bullseye ring
  R2 = 15.9, # center to single bullseye ring
  R3 = 99, # center to inner triple ring
  R4 = 107, # center to outer triple ring
  R5 = 162, # center to inner double ring
  R = 170, # center to outer double ring
  nums = c(20,1,18,4,13,6,10,15,2,17,3,19,
           7,16,8,11,14,9,12,5)) # numbers in order
drawBoard(board)


# Simulate 100 throws with the x and y
# location modeled by N(0, 50^2).
throws <- 100
std.dev <- 50
x <- rnorm(throws, sd = std.dev)
y <- rnorm(throws, sd = std.dev)
drawBoard(board)
points(x, y, pch = 20, col = "red")
# Score the throws and add the values to the plot
scores <- scorePositions(x, y, board)
text(x, y + 8, scores, cex = .75)



