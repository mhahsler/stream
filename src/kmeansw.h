/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

//

#ifndef KMEANSW_H
#define KMEANSW_H

extern "C" {
void kmnsw ( double *a, int *mR, int *nR, double *c, double *wh, int *kR, int *ic1, int *nc,
             int *iterR, double *wss, int *ifault );
}

void optra ( double a[], int m, int n, double c[], double wh[], double whc[], int k, int ic1[],
             int ic2[], int nc[], int ncp[], double d[],
             int itran[], int live[], int *indx, int *ifault );

void qtran ( double a[], int m, int n, double c[], double wh[], double whc[], int k, int ic1[],
             int ic2[], int nc[], int ncp[], double d[],
             int itran[], int *indx, int *ifault );

double r8_huge ( void );

#endif
