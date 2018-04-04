#   MIT License
#
#   Copyright(c) 2017-2018 Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com]
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files(the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission
#   notice shall be included in all
#   copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
#   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
#   THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
