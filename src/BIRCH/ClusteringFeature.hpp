//
// Created by Dede on 08.09.2016.
//

#ifndef CFTREE_CLUSTERINGFEATURE_H
#define CFTREE_CLUSTERINGFEATURE_H

#include<iostream>
#include <vector>
#include <algorithm>
#include <Rcpp.h>
#include "Util.hpp"

namespace CF{
    class ClusteringFeature {
    public:
        ClusteringFeature(ClusteringFeature *feature);
        ClusteringFeature(Rcpp::NumericVector v);
        ClusteringFeature(long n, Rcpp::NumericVector ls, double ss);
        ClusteringFeature(short dim);
    public:

        bool canAbsorb(CF::ClusteringFeature *newCF,bool diameter, double treshold);

        void add(CF::ClusteringFeature *feature);

        void remove(CF::ClusteringFeature *feature);

        void clearCF();

        long getN();

        void setN(long n);

        Rcpp::NumericVector &getLs();

        void setLs(const Rcpp::NumericVector &ls);

        double getSs();

        void setSs(double ss);

        Rcpp::NumericVector getCentroid();

        double getRadius();

        double getDiameter();
        double getD1(CF::ClusteringFeature *c2);

        double getD0(CF::ClusteringFeature *c2);


        double getD2();

        double getD3();

        double getD4();

        double getInterClusterMetric(CF::ClusteringFeature *cf);

    private:
      int interClusterMetric;
      Rcpp::NumericVector ls;
      double ss;
      long n;

    };
}


#endif //CFTREE_CLUSTERINGFEATURE_H
