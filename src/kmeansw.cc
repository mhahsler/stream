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

#include <cstdlib>
#include <iomanip>
#include <cmath>
#include <ctime>
#include "kmeansw.h"

using namespace std;

# define epsilon 1.0E-30 //NEW


extern "C"  {
//****************************************************************************80
//MODIF function name: kmnsw
//NEW   weight of each point variable: wh
void kmnsw ( double *a, int *m_p, int *n_p, double *c, double *wh, int *k_p,
                 int *ic1, int *nc, int *iter_p, double *wss, int *ifault )
//****************************************************************************80
//
//     Purpose:
//
//MODIF  KMNSW carries out the K-means algorithm considering weights.
//
//     Discussion:
//
//       This routine attempts to divide M points in N-dimensional space into
//       K clusters so that the within cluster sum of squares is minimized
//       considering weights for each point.
//
//     Modified:
//
//MODIF  25 November 2009
//
//     Author:
//
//       FORTRAN77 original by John Hartigan, Manchek Wong.
//       C++ version by John Burkardt.
//NEW    C++ version modified considering weights by Camilo Torres.
//
//     Reference:
//
//       John Hartigan, Manchek Wong,
//       Algorithm AS 136:
//       A K-Means Clustering Algorithm,
//       Applied Statistics,
//       Volume 28, Number 1, 1979, pages 100-108.
//
//     Parameters:
//
//       Input, double A(M,N), the points.
//
//       Input, int M, the number of points.
//
//       Input, int N, the number of spatial dimensions.
//
//       Input/output, double C(K,N), the cluster centers.
//
//NEW    Input, double WH(M), the weight of each point.
//
//       Input, int K, the number of clusters.
//
//       Output, int IC1(M), the cluster to which each point
//       is assigned.
//
//       Output, int NC(K), the number of points in each cluster.
//
//       Input, int ITER, the maximum number of iterations allowed.
//
//       Output, double WSS(K), the within-cluster sum of squares
//       of each cluster.
//
//       Output, int *IFAULT, error indicator.
//       0, No error was detected.
//       1, At least one cluster is empty or with a weight lower than epsilon
//          after the initial assignment.
//          A better set of initial cluster centers is needed.
//       2, The allowed maximum number off iterations was exceeded.
//       3, K is less than or equal to 1, or greater than or equal to M.
//
//NEW    Output, double WHC(K), the weight of each cluster.
//
{
  //DEL  double aa;
  double da;
  double db;
  double dc;
  double dt[2];
  int i;
  int ii;
  int ij;
  int il;
  int indx;
  int j;
  int l;
  double temp;
  double *d;
  int *ic2;
  int *itran;
  int *live;
  int *ncp;
  double *whc; //NEW
  int m=*m_p, n=*n_p, k=*k_p, iter=*iter_p; //NEW

  *ifault = 0;

  if ( k <= 1 || m <= k )
  {
    *ifault = 3;
    return;
  }
  d = new double[m];
  ic2 = new int[m];
  itran = new int[k];
  live = new int[k];
  ncp = new int[k];
  whc = new double[k]; //NEW
//
//  For each point I, find its two closest centers, IC1(I) and
//  IC2(I).  Assign the point to IC1(I).
//
  for ( i = 1; i <= m; i++ )
  {
    ic1[i-1] = 1;
    ic2[i-1] = 2;

    for ( il = 1; il <= 2; il++ )
    {
      dt[il-1] = 0.0;
      for ( j = 1; j <= n; j++ )
      {
        da = a[i-1+(j-1)*m] - c[il-1+(j-1)*k];
        dt[il-1] = dt[il-1] + da * da;
      }
    }

    if ( dt[1] < dt[0] )
    {
      ic1[i-1] = 2;
      ic2[i-1] = 1;
      temp = dt[0];
      dt[0] = dt[1];
      dt[1] = temp;
    }

    for ( l = 3; l <= k; l++ )
    {
      db = 0.0;
      for ( j = 1; j <= n; j++ )
      {
        dc = a[i-1+(j-1)*m] - c[l-1+(j-1)*k];
        db = db + dc * dc;
      }

      if ( db < dt[1] )
      {
        if ( dt[0] <= db )
        {
          dt[1] = db;
          ic2[i-1] = l;
        }
        else
        {
          dt[1] = dt[0];
          ic2[i-1] = ic1[i-1];
          dt[0] = db;
          ic1[i-1] = l;
        }
      }
    }
  }
//
//  Update cluster centers to be the average of points contained within them.
//
  for ( l = 1; l <= k; l++ )
  {
    nc[l-1] = 0;
    whc[l-1] = 0.0; //NEW
    for ( j = 1; j <= n; j++ )
    {
      c[l-1+(j-1)*k] = 0.0;
    }
  }

  for ( i = 1; i <= m; i++ )
  {
    l = ic1[i-1];
    nc[l-1] = nc[l-1] + 1;
    whc[l-1] = whc[l-1] + wh[i-1]; //NEW
    for ( j = 1; j <= n; j++ )
    {
      c[l-1+(j-1)*k] = c[l-1+(j-1)*k] + a[i-1+(j-1)*m] * wh[i-1]; //MODIF
    }
  }
//
//  Check to see if there is any empty cluster at this stage.
//
  *ifault = 1;

  for ( l = 1; l <= k; l++ )
  {
    if ( whc[l-1] < epsilon ) //MODIF
    {
      return;
    }
  }

  *ifault = 0;

  for ( l = 1; l <= k; l++ )
  {
    //DEL    aa = ( double ) ( nc[l-1] );
     for ( j = 1; j <= n; j++ )
    {
        c[l-1+(j-1)*k] = c[l-1+(j-1)*k] / whc[l-1]; //MODIF
    }
//
//MODIF//  Initialize ITRAN and NCP.
//
//DEL//  AN1(L) = NC(L) / (NC(L) - 1)
//DEL//  AN2(L) = NC(L) / (NC(L) + 1)
//  ITRAN(L) = 1 if cluster L is updated in the quick-transfer stage,
//           = 0 otherwise
//
//  In the optimal-transfer stage, NCP(L) stores the step at which
//  cluster L is last updated.
//
//  In the quick-transfer stage, NCP(L) stores the step at which
//  cluster L is last updated plus M.
//
//DEL    an2[l-1] = aa / ( aa + 1.0 );
//DEL    if ( 1.0 < aa )
//DEL    {
//DEL      an1[l-1] = aa / ( aa - 1.0 );
//DEL    }
//DEL    else
//DEL    {
//DEL      an1[l-1] = r8_huge ( );
//DEL    }
    itran[l-1] = 1;
    ncp[l-1] = -1;
  }

  indx = 0;
  *ifault = 2;

  for ( ij = 1; ij <= iter; ij++ )
  {
//
//  In this stage, there is only one pass through the data.   Each
//  point is re-allocated, if necessary, to the cluster that will
//  induce the maximum reduction in within-cluster sum of squares.
//
    optra ( a, m, n, c, wh, whc, k, ic1, ic2, nc, ncp, d, itran, live, &indx, ifault );
//
//  Stop if no transfer took place in the last M optimal transfer steps.
//
    if ( indx == m )
    { //NEW
      *ifault = 0;
      break;
    }
//
//  Each point is tested in turn to see if it should be re-allocated
//  to the cluster to which it is most likely to be transferred,
//  IC2(I), from its present cluster, IC1(I).   Loop through the
//  data until no further change is to take place.
//
    qtran ( a, m, n, c, wh, whc, k, ic1, ic2, nc, ncp, d, itran, &indx, ifault );
//
//  If there are only two clusters, there is no need to re-enter the
//  optimal transfer stage.
//
    if ( k == 2 )
    {
      *ifault = 0;
      break;
    }
//
//  NCP has to be set to 0 before entering OPTRA.
//
    for ( l = 1; l <= k; l++ )
    {
      ncp[l-1] = 0;
    }

  }
//
//  Compute the within-cluster sum of squares for each cluster.
//
  for ( l = 1; l <= k; l++ )
  {
    wss[l-1] = 0.0;
    for ( j = 1; j <= n; j++ )
    {
      c[l-1+(j-1)*k] = 0.0;
    }
  }

  for ( i = 1; i <= m; i++ )
  {
    ii = ic1[i-1];
    for ( j = 1; j <= n; j++ )
    {
      c[ii-1+(j-1)*k] = c[ii-1+(j-1)*k] + a[i-1+(j-1)*m] * wh[i-1]; //MODIF
    }
  }

  for ( j = 1; j <= n; j++ )
  {

    for ( l = 1; l <= k; l++ )
    {
      if(whc[l-1] > epsilon) //NEW
        c[l-1+(j-1)*k] = c[l-1+(j-1)*k] / whc[l-1]; //MODIF
      else //NEW*
        c[l-1+(j-1)*k] = c[l-1+(j-1)*k] * r8_huge(); //NEW*
    }

    for ( i = 1; i <= m; i++ )
    {
      ii = ic1[i-1];
      da = a[i-1+(j-1)*m] - c[ii-1+(j-1)*k];
      wss[ii-1] = wss[ii-1] + da * da * wh[i-1]; //MODIF
    }
  }
  
  delete [] d;
  delete [] ic2;
  delete [] itran;
  delete [] live;
  delete [] ncp;
  delete [] whc; //NEW

  return;
}

} // extern "C"

//****************************************************************************80
//DEL   variables: an1, an2
void optra ( double a[], int m, int n, double c[], double wh[], double whc[],
             int k, int ic1[], int ic2[], int nc[], int ncp[],
             double d[], int itran[], int live[], int *indx, int *ifault)
//****************************************************************************80
//
//     Purpose:
//
//       OPTRA carries out the optimal transfer stage.
//
//     Discussion:
//
//       This is the optimal transfer stage.
//
//       Each point is re-allocated, if necessary, to the cluster that
//       will induce a maximum reduction in the within-cluster sum of
//       squares.
//
//     Modified:
//
//MODIF  20 November 2009
//
//     Author:
//
//       FORTRAN77 original by John Hartigan, Manchek Wong.
//       C++ version by John Burkardt.
//NEW    C++ version modified considering weights by Camilo Torres.
//
//     Reference:
//
//       John Hartigan, Manchek Wong,
//       Algorithm AS 136:
//       A K-Means Clustering Algorithm,
//       Applied Statistics,
//       Volume 28, Number 1, 1979, pages 100-108.
//
//     Parameters:
//
//       Input, double A(M,N), the points.
//
//       Input, int M, the number of points.
//
//       Input, int N, the number of spatial dimensions.
//
//       Input/output, double C(K,N), the cluster centers.
//
//NEW    Input, double WH(M), the weight of each point.
//
//NEW    Output, double WHC(K), the weight of each cluster.
//
//       Input, int K, the number of clusters.
//
//       Input/output, int IC1(M), the cluster to which each
//       point is assigned.
//
//       Input/output, int IC2(M), used to store the cluster
//       which each point is most likely to be transferred to at each step.
//
//       Input/output, int NC(K), the number of points in
//       each cluster.
//
//       Input/output, int NCP(K).
//
//       Input/output, double D(M).
//
//       Input/output, int ITRAN(K).
//
//       Input/output, int LIVE(K).
//
//       Input/output, int *INDX, the number of steps since a
//       transfer took place.
//
{
//DEL  double al1;
//DEL  double al2;
//DEL  double alt;
//DEL  double alw;
  double walt; //NEW
  double walw; //NEW
  double aux; //NEW
  double da;
  double db;
  double dc;
  double dd;
  double de;
  double df;
  int i;
  int j;
  int l;
  int l1;
  int l2;
  int ll;
  double r2;
  double rr;
//
//  If cluster L is updated in the last quick-transfer stage, it
//  belongs to the live set throughout this stage.   Otherwise, at
//  each step, it is not in the live set if it has not been updated
//  in the last M optimal transfer steps.
//
  for ( l = 1; l <= k; l++ )
  {
    if ( itran[l-1] == 1)
    {
      live[l-1] = m + 1;
    }
  }

  for ( i = 1; i <= m; i++ )
  {
    *indx = *indx + 1;
    l1 = ic1[i-1];
    l2 = ic2[i-1];
    ll = l2;
//
//  If point I is the only member of cluster L1, no transfer.
//
    if ( 1 < nc[l1-1]  )
    {
//
//  If L1 has not yet been updated in this stage, no need to
//  re-compute D(I).
//
      if ( ncp[l1-1] != 0 )
      {
        de = 0.0;
        for ( j = 1; j <= n; j++ )
        {
          df = a[i-1+(j-1)*m] - c[l1-1+(j-1)*k];
          de = de + df * df;
        }

        walw = whc[l1-1] - wh[i-1]; //NEW
        if(walw > epsilon) //NEW*
          d[i-1] = (de * whc[l1-1]) / walw; //MODIF
        else //NEW*
          d[i-1] = (de * whc[l1-1]) * r8_huge(); //NEW*

      }
//
//  Find the cluster with minimum R2.
//
      da = 0.0;
      for ( j = 1; j <= n; j++ )
      {
        db = a[i-1+(j-1)*m] - c[l2-1+(j-1)*k];
        da = da + db * db;
      }

      walt = whc[l2-1] + wh[i-1]; //NEW
      if(walt > epsilon) //NEW*
        r2 = (da * whc[l2-1]) / walt; //MODIF
      else //NEW*
        r2 = (da * whc[l2-1]) * r8_huge(); //NEW*

      for ( l = 1; l <= k; l++ )
      {
//
//  If LIVE(L1) <= I, then L1 is not in the live set.   If this is
//  true, we only need to consider clusters that are in the live set
//  for possible transfer of point I. Otherwise, we need to consider
//  all possible clusters.
//
        if ( ( i < live[l1-1] || i < live[l-1] ) && l != l1 && l != ll ) //MODIF i < live[l1-1] || i < live[l2-1]
        {
//DEL          rr = r2 / an2[l-1];

          dc = 0.0;
          for ( j = 1; j <= n; j++ )
          {
            dd = a[i-1+(j-1)*m] - c[l-1+(j-1)*k];
            dc = dc + dd * dd;
          }

          walt = whc[l-1] + wh[i-1]; //NEW
          if(walt > epsilon) //NEW*
            rr = (dc * whc[l-1]) / walt; //NEW
          else //NEW*
            rr = (dc * whc[l-1]) * r8_huge(); //NEW*

          if ( rr < r2 )
          {
            r2 = rr; //MODIF
            l2 = l;
          }
        }
      }
//
//  If no transfer is necessary, L2 is the new IC2(I).
//
      if ( d[i-1] <= r2 )
      {
        ic2[i-1] = l2;
      }
//
//  Update cluster centers, LIVE, NCP, for clusters L1 and
//  L2, and update IC1(I) and IC2(I).
//
      else
      {
        *indx = 0;
        live[l1-1] = m + i;
        live[l2-1] = m + i;
        ncp[l1-1] = i;
        ncp[l2-1] = i;
//DEL        al1 = ( double ) ( nc[l1-1] );
//DEL        alw = al1 - 1.0;
//DEL        al2 = ( double ) ( nc[l2-1] );
//DEL        alt = al2 + 1.0;
        walw = whc[l1-1] - wh[i-1]; //NEW
        walt = whc[l2-1] + wh[i-1]; //NEW

        for ( j = 1; j <= n; j++ )
        {
          aux = a[i-1+(j-1)*m] * wh[i-1]; //NEW
          if(walw > epsilon) //NEW*
            c[l1-1+(j-1)*k] = ( c[l1-1+(j-1)*k] * whc[l1-1] - aux ) / walw; //MODIF
          else //NEW*
            c[l1-1+(j-1)*k] = ( c[l1-1+(j-1)*k] * whc[l1-1] - aux ) * r8_huge(); //NEW*

          if(walt > epsilon)  //NEW*
            c[l2-1+(j-1)*k] = ( c[l2-1+(j-1)*k] * whc[l2-1] + aux ) / walt; //MODIF
          else //NEW*
            c[l2-1+(j-1)*k] = ( c[l2-1+(j-1)*k] * whc[l2-1] + aux ) * r8_huge(); //NEW*
        }

        nc[l1-1] = nc[l1-1] - 1;
        nc[l2-1] = nc[l2-1] + 1;
//DEL        an2[l1-1] = alw / al1;
//DEL        if ( 1.0 < alw )
//DEL        {
//DEL          an1[l1-1] = alw / ( alw - 1.0 );
//DEL        }
//DEL        else
//DEL        {
//DEL          an1[l1-1] = r8_huge ( );
//DEL        }
//DEL        an1[l2-1] = alt / al2;
//DEL        an2[l2-1] = alt / ( alt + 1.0 );
        whc[l1-1] = walw; //NEW
        whc[l2-1] = walt; //NEW
        ic1[i-1] = l2;
        ic2[i-1] = l1;
      }
    }

    if ( *indx == m )
    {
      return;
    }
  }
//
//  ITRAN(L) = 0 before entering QTRAN.   Also, LIVE(L) has to be
//  decreased by M before re-entering OPTRA.
//
  for ( l = 1; l <= k; l++ )
  {
    itran[l-1] = 0;
    live[l-1] = live[l-1] - m;
  }
  return;
}

//****************************************************************************80
//DEL   variables: an1, an2
void qtran ( double a[], int m, int n, double c[], double wh[], double whc[],
             int k, int ic1[], int ic2[], int nc[], int ncp[],
             double d[], int itran[], int *indx, int *ifault )
//****************************************************************************80
//
//     Purpose:
//
//       QTRAN carries out the quick transfer stage.
//
//     Discussion:
//
//       This is the quick transfer stage.
//
//       IC1(I) is the cluster which point I belongs to.
//       IC2(I) is the cluster which point I is most likely to be
//       transferred to.
//
//       For each point I, IC1(I) and IC2(I) are switched, if necessary, to
//       reduce within-cluster sum of squares.  The cluster centers are
//       updated after each step.
//
//     Modified:
//
//MODIF  20 November 2009
//
//     Author:
//
//       FORTRAN77 original by John Hartigan, Manchek Wong.
//       C++ version by John Burkardt.
//NEW    C++ version modified considering weights by Camilo Torres.
//
//     Reference:
//
//       John Hartigan, Manchek Wong,
//       Algorithm AS 136:
//       A K-Means Clustering Algorithm,
//       Applied Statistics,
//       Volume 28, Number 1, 1979, pages 100-108.
//
//     Parameters:
//
//       Input, double A(M,N), the points.
//
//       Input, int M, the number of points.
//
//       Input, int N, the number of spatial dimensions.
//
//       Input/output, double C(K,N), the cluster centers.
//
//NEW    Input, double WH(M), the weight of each point.
//
//NEW    Output, double WHC(K), the weight of each cluster.
//
//       Input, int K, the number of clusters.
//
//       Input/output, int IC1(M), the cluster to which each
//       point is assigned.
//
//       Input/output, int IC2(M), used to store the cluster
//       which each point is most likely to be transferred to at each step.
//
//       Input/output, int NC(K), the number of points in
//       each cluster.
//
//       Input/output, int NCP(K).
//
//       Input/output, double D(M).
//
//       Input/output, int ITRAN(K).
//
//       Input/output, int INDX, counts the number of steps
//       since the last transfer.
//
{
//DEL  double al1;
//DEL  double al2;
//DEL  double alt;
//DEL  double alw;
  double walt; //NEW
  double walw; //NEW
  double aux; //NEW
  double da;
  double db;
  double dd;
  double de;
  int i;
  int icoun;
  int istep;
  int j;
  int l1;
  int l2;
  double r2;
//
//  In the optimal transfer stage, NCP(L) indicates the step at which
//  cluster L is last updated.   In the quick transfer stage, NCP(L)
//  is equal to the step at which cluster L is last updated plus M.
//
  icoun = 0;
  istep = 0;

  for ( ; ; )
  {
    for ( i = 1; i <= m; i++ )
    {
      icoun = icoun + 1;
      istep = istep + 1;
      l1 = ic1[i-1];
      l2 = ic2[i-1];
//
//  If point I is the only member of cluster L1, no transfer.
//
      if ( 1 != nc[l1-1] )
      {
//
//  If NCP(L1) < ISTEP, no need to re-compute distance from point I to
//  cluster L1.   Note that if cluster L1 is last updated exactly M
//  steps ago, we still need to compute the distance from point I to
//  cluster L1.
//
        if ( istep <= ncp[l1-1] )
        {
          da = 0.0;
          for ( j = 1; j <= n; j++ )
          {
            db = a[i-1+(j-1)*m] - c[l1-1+(j-1)*k];
            da = da + db * db;
          }
          walw = whc[l1-1] - wh[i-1]; //NEW
	  if(walw > epsilon) //NEW*
            d[i-1] = (da * whc[l1-1]) / walw; //MODIF
          else //NEW*
            d[i-1] = (da * whc[l1-1]) * r8_huge(); //NEW*
        }
//
//  If NCP(L1) <= ISTEP and NCP(L2) <= ISTEP, there will be no transfer of
//  point I at this step.
//
        if ( istep < ncp[l1-1] || istep < ncp[l2-1] )
        {
//DEL          r2 = d[i-1] / an2[l2-1];

          dd = 0.0;
          for ( j = 1; j <= n; j++ )
          {
            de = a[i-1+(j-1)*m] - c[l2-1+(j-1)*k];
            dd = dd + de * de;
          }

          walt = whc[l2-1] + wh[i-1]; //NEW
          if(walt > epsilon) //NEW*
            r2 = (dd * whc[l2-1]) / walt; //MODIF
          else //NEW*
            r2 = (dd * whc[l2-1]) * r8_huge(); //NEW*
//
//  Update cluster centers, NCP, NC, ITRAN, AN1 and AN2 for clusters
//  L1 and L2.   Also update IC1(I) and IC2(I).   Note that if any
//  updating occurs in this stage, INDX is set back to 0.
//
          if ( r2 < d[i-1] )
          {
            icoun = 0;
            *indx = 0;
            itran[l1-1] = 1;
            itran[l2-1] = 1;
            ncp[l1-1] = istep + m;
            ncp[l2-1] = istep + m;
//DEL            al1 = ( double ) ( nc[l1-1] );
//DEL            alw = al1 - 1.0;
//DEL            al2 = ( double ) ( nc[l2-1] );
//DEL            alt = al2 + 1.0;
            walw = whc[l1-1] - wh[i-1]; //NEW
            walt = whc[l2-1] + wh[i-1]; //NEW

            for ( j = 1; j <= n; j++ )
            {
              aux = a[i-1+(j-1)*m] * wh[i-1]; //NEW
	      if(walw > epsilon) //NEW*
                c[l1-1+(j-1)*k] = ( c[l1-1+(j-1)*k] * whc[l1-1] - aux ) / walw; //MODIF
              else //NEW*
                c[l1-1+(j-1)*k] = ( c[l1-1+(j-1)*k] * whc[l1-1] - aux ) * r8_huge(); //NEW*
              if(walt > epsilon) //NEW*
                c[l2-1+(j-1)*k] = ( c[l2-1+(j-1)*k] * whc[l2-1] + aux ) / walt; //MODIF
              else //NEW*
                c[l2-1+(j-1)*k] = ( c[l2-1+(j-1)*k] * whc[l2-1] + aux ) * r8_huge(); //NEW*
            }
            nc[l1-1] = nc[l1-1] - 1;
            nc[l2-1] = nc[l2-1] + 1;
//DEL            an2[l1-1] = alw / al1;
//DEL            if ( 1.0 < alw )
//DEL            {
//DEL              an1[l1-1] = alw / ( alw - 1.0 );
//DEL            }
//DEL            else
//DEL            {
//DEL              an1[l1-1] = r8_huge ( );
//DEL            }
//DEL            an1[l2-1] = alt / al2;
//DEL            an2[l2-1] = alt / ( alt + 1.0 );
            whc[l1-1] = walw; //NEW
            whc[l2-1] = walt; //NEW
            ic1[i-1] = l2;
            ic2[i-1] = l1;
          }
        }
      }
//
//  If no re-allocation took place in the last M steps, return.
//
      if ( icoun == m )
      {
        return;
      }
    }
  }
}
//****************************************************************************80

double r8_huge ( void )

//****************************************************************************80
//
//  Purpose:
//
//    R8_HUGE returns a "huge" R8.
//
//  Discussion:
//
//    The value returned by this function is NOT required to be the
//    maximum representable R8.  This value varies from machine to machine,
//    from compiler to compiler, and may cause problems when being printed.
//    We simply want a "very large" but non-infinite number.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 October 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, double R8_HUGE, a "huge" R8 value.
//
{
  double value;

  value = 1.0E+30;

  return value;
}
