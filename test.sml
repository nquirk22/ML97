fun flip (nil) = nil
  | flip ([x]) = [x]
  | flip (x :: y :: zs) = y :: x :: flip(zs);

fun deleteIth (x::ys, 0) = ys
  | deleteIth ( = 

