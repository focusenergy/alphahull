inahull <-
function (ahull.obj, p) 
{
    compl <- ahull.obj$complement

    ## TODO it might be better to store half-planes as simply (a,b,c) where ax+by+c>0.
    halfp <- compl[compl[, "r"] < 0, ]
    sig = halfp[,3]
    a = halfp[,1]
    b = halfp[,2]

    in.halfp <-
        (p[1] > a & sig == -3) |             # x > a;       1x + 0y - a > 0
        (p[1] < a & sig == -4) |             # x < a;      -1x + 0y + a > 0
        (p[2] > a + b * p[1] & sig == -1) |  # y > a + bx; -bx + 1y - a > 0
        (p[2] < a + b * p[1] & sig == -2)    # y < a + bx;  bx - 1y + a > 0
    if (any(in.halfp))
        return(FALSE)

    ball <- compl[compl[, "r"] > 0, ]
    r = ball[,3]
    c1 = ball[,1]
    c2 = ball[,2]
    d <- sqrt((p[1] - c1)^2 + (p[2] - c2)^2)
    if (any(d < r))
        return(FALSE)

    return(TRUE)
}
