#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


## DSC
DSC_registry <- registry(registry_class="DSC_registry",
  entry_class="DSC_")

DSC_registry$set_field("name", type = "character",
  is_key = TRUE, index_FUN = match_regexp, ignore.case = TRUE)
DSC_registry$set_field("description", type = "character",
  is_key = FALSE)
DSC_registry$set_field("DSC_Micro", type = "logical",
  is_key = TRUE)
DSC_registry$set_field("DSC_Macro", type = "logical",
  is_key = TRUE)
