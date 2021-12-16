#
# Copyright (C) 2013-2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' @export
.v <- function(x, ...) { x }

#' @export
.unv <- function(x, ...) { x }

#' @export
interactionSymbol <- "\u2009\u273B\u2009"
base::Encoding(interactionSymbol) <- "UTF-8"

#' @export
gsubInteractionSymbol <- function(x, ...) {
  UseMethod("gsubInteractionSymbol", x)
}

#' @export
gsubInteractionSymbol.list <- function(x, how = "replace", ...) {
  rapply(x, gsubInteractionSymbol, classes = "character", how = how, ...)
}

#' @export
gsubInteractionSymbol.character <- function(x, ...) {
  # assumes the input consists of encoded column names, which never contain ':'
  gsub(":", interactionSymbol, x, fixed = TRUE, ...)
}
