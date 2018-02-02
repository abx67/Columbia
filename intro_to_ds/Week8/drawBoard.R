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