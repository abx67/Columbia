scorePositions = function(x, y, board) {
  R1 = board$R1
  R2 = board$R2
  R3 = board$R3
  R4 = board$R4
  R5 = board$R5
  R = board$R
  nums = board$nums
  
  n = length(x)
  rad = sqrt(x^2 + y^2)
  raw.angles = atan2(x,y)
  slice = 2*pi/20
  tilted.angles = (raw.angles + slice/2) %% (2*pi)
  scores = nums[floor(tilted.angles/slice) + 1]
  
  # Bullseyes
  scores[rad <= R1] = 50
  scores[R1 < rad & rad <= R2] = 25
  
  # Triples
  scores[R3 < rad & rad <= R4] = 3*scores[R3 < rad & rad <= R4]
  
  # Doubles
  scores[R5 < rad & rad <= R] = 2*scores[R5 < rad & rad <= R]
  
  # Zeros (off the board)
  scores[R < rad] = 0
  
  return(scores)
}