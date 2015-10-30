# Notation:
#
# firstderv: first derivative  dx/dh
# secondderv: second derivative d^2x/(dh)^2
# forward: derivative is based on values in the positive axial direction
# central: derivative is based on symetric values in the positive / negative axial direction
# backward: derivative is based on values in the negative axial direction
# order: the order of the error as a function of h.  For example, order4 is O(h^4) in error
#
# parameters:
#
# Let x be the quantity of take the derivative of
# Let h be the quantity taking the derivative with respect to
# x: instant it which the derivative is being taken (x_i)
# xm: x_(i-1) (read x minus)
# xmm: x_(i-2) (read x minus minus)
# xp: x_(i+1) (read x plus)
# xpp: x_(i+2) (read x plus plus)
# h: step size along the x axis
#

firstderv_forward_order1 <- function(x, xp, h = 1) {
   (xp - x) / h
}

firstderv_central_order2 <- function(xm, xp, h = 1 ) {
   (xp - xm) / (2*h)
}

firstderv_backward_order1 <- function(xm, x, h = 1){
   (x - xm) / h
}

firstderv_forward_order2 <- function(x, xp, xpp, h = 1){
   (-xpp + 4 * xp - 3 * x ) / (2*h)
}

firstderv_central_order4 <- function(xmm, xm, xp, xpp, h = 1){
   (-xpp + 8 * xp - 8 * xm + xmm) / (12 * h)
}

firstderv_backward_order2 <- function(xmm, xm, x, h = 1){
   (x - 2 * xm + xmm) / (2 * h)
}

secondderv_forward_order1 <- function(x, xp, xpp, h = 1) {
   (xpp - 2 * xp + x) / (h * h)
}

secondderv_central_order2 <- function(xm, x, xp, h = 1){
   (xp - 2*x + xm) / (h * h)
}

secondderv_backward_order1 <- function(xmm, xm, x, h = 1){
   (x - 2 * xm + xmm) / (h * h)
}

secondderv_forward_order2 <- function(x, xp, xpp, xpp, xppp, h = 1){
   (-xppp + 4 * xpp - 5 * xp + 2 * x) / (h * h)
}

secondderv_central_order4 <- function(xmm, xm, x, xp, xpp, h = 1){
   (-xpp + 16 * xp - 30 * x + 16 * xm - xmm) / (12 * h * h)
}

secondderv_backward_order2 <- function(xmmm, xmm, xm, x, h = 1){
   (2 * x - 5 * xm + 4 * xmm - xmmm) / (h * h)
}
