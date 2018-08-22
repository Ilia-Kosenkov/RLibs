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

#' @rawNamespace export(Const.c)
makeActiveBinding("Const.c", function() 2.99792458e10, env = environment())

#' @rawNamespace export(Const.h)
makeActiveBinding("Const.h", function() 6.6260755e-27, env = environment())

#' @rawNamespace export(Const.h_bar)
makeActiveBinding("Const.h_bar", function() 1.05457266e-27,
                env = environment())

#' @rawNamespace export(Const.G)
makeActiveBinding("Const.G", function() 6.67259e-8, env = environment())

#' @rawNamespace export(Const.K)
makeActiveBinding("Const.K", function() 1.380658e-16, env = environment())

#' @rawNamespace export(Const.m_e)
makeActiveBinding("Const.m_e", function() 9.10938356e-28, env = environment())

#' @rawNamespace export(Const.m_H)
makeActiveBinding("Const.m_H", function() 1.6737236e-24, env = environment())


#' @rawNamespace export(Const.m_Sun)
makeActiveBinding("Const.m_Sun", function() 1.98855e33, env = environment())

#' @rawNamespace export(Const.L_Sun)
makeActiveBinding("Const.L_Sun", function() 3.828e33, env = environment())

#' @rawNamespace export(Const.R_Sun)
makeActiveBinding("Const.R_Sun", function() 6.95700e10, env = environment())


#' @rawNamespace export(Const.Sigm_SB)
makeActiveBinding("Const.Sigm_SB", function() 5.67036713e-5,
                env = environment())

#' @rawNamespace export(Const.Sigm_Th)
makeActiveBinding("Const.Sigm_Th", function() 6.6524587158e-25,
                env = environment())

#' @rawNamespace export(Const.g)
makeActiveBinding("Const.g", function() 9.80665e2, env = environment())

#' @rawNamespace export(Const.R_gas)
makeActiveBinding("Const.R_gas", function() 8.314459848e7, env = environment())

# Unit conversions

#' @rawNamespace export(Const.Jy2CGS)
makeActiveBinding("Const.Jy2CGS", function() 1e-23, env = environment())
