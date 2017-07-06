QU2PA = function(q, u, dq = c(), du = c()) 
{
    f = 180 / pi
    n = min(length(q), length(u))
    if (length(dq) > 0 & length(du) > 0)
    {
       n = min(n, length(dq), length(du))
    }

    q = q[1:n]
    u = u[1:n]
    dq = dq[1:n]
    du = du[1:n]

    p = sqrt(q ^ 2 + u ^ 2)
    a = 0.5 * atan(u/ q) * f

    if (length(dq) > 0 & length(du) > 0)
    {
        dp = sqrt((q^2 * dq^2 + u^2 * du^2)) / p

        da = 0.5 * f * sqrt(q^2 * du^2 + u^2 * dq^2) / p ^ 2

        return(list("p" = p, "a" = a, "dp" = dp,"da" = da))
    }
    else
        return(list("p" = p, "a" = a, "dp" = NA, "da" = NA))
}


PA2QU = function(p, a, dp = c(), da = c())
{
    f = pi / 180
    n = min(length(p), length(a))
    if (length(dp) > 0 & length(da) > 0)
    {
        n = min(n, length(dp), length(da))
    }

    p = p[1:n]
    a = a[1:n]
    dp = dp[1:n]
    da = da[1:n]

    q = p * cos(2 * a * f)
    u = p * sin(2 * a * f)

    if (length(dp) > 0 & length(da) > 0)
    {
        dq = sqrt(cos(2 * a * f) ^ 2 * dp ^ 2 + p ^ 2 * 4 * da ^ 2 * f^2* sin(2 * a * f)^2)

        du = sqrt(sin(2 * a * f) ^ 2 * dp ^ 2 + p ^ 2 * 4 * da ^ 2 * f ^ 2 * cos(2 * a * f) ^ 2)

        return(list("q" = q, "u" = u, "dq" = dq, "du" = du))
    } else
        return(list("q" = q, "u" = u, "dq" = NA, "du" = NA))
}
