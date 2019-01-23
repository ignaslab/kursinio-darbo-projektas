#include <Rcpp.h>
using namespace Rcpp;
#include <iostream>
#include <fstream>
#include <vector>
#include <iomanip>
#include <cmath>

double interpolate(NumericVector xData, NumericVector yData, double x)
{
  int size = xData.size();

  int i = 0;
  if (x >= xData[size - 2])
  {
    i = size - 2;
  }
  else
  {
    while (x > xData[i+1]) i++;
  }
  double xL = xData[i], yL = yData[i], xR = xData[i+1], yR = yData[i+1];

  if (x < xL) yR = yL;
  if (x > xR) yL = yR;

  double dydx = (yR - yL) / (xR - xL);

  return yL + dydx * (x - xL);
}
//[[Rcpp::export]]
NumericVector solveDiffusionEq(double L, double n, int t, DataFrame boundaryCondition, DataFrame diffusion){
  double m;
  double del_x = L/n;
  double del_t;
  double R;
  double minD;
  NumericVector x, u, p, diffCof;
  NumericVector bC_t = boundaryCondition["t"];
  NumericVector bC_f0 = boundaryCondition["f0"];
  NumericVector bC_f1 = boundaryCondition["f1"];
  NumericVector dif_x = diffusion["x"];
  NumericVector dif_d = diffusion["d"];


  for(int j=0; j<n; j++){
    x.push_back(j*del_x);
    u.push_back(4*x[j]*(1-x[j]));
    diffCof.push_back(interpolate(dif_x, dif_d, x[j]));
  }

  minD = min(diffCof);
  del_t = 0.5*(pow(del_x,2)/minD);
  m = t / del_t;
  R = max(diffCof)*(del_t/pow(del_x,2));

    for (int j=0; j<n; j++){
      p.push_back(x[j]);
      p.push_back(0);
      p.push_back(u[j]);
    }
  for(int i=0; i<m; i++){

    u[0]=interpolate(bC_t, bC_f0, del_t*i);
    for(int j=1; j<(n-1); j++){
      R = diffCof[j]*(del_t/pow(del_x,2));
      u[j]= u[j] + R*(u[j+1]-2*u[j]+u[j-1]);
    }
    u[n-1]=interpolate(bC_t, bC_f1, del_t*i);

    for (int j=0; j<n; j++){
      p.push_back(x[j]);
      p.push_back((i+1)*del_t);
      p.push_back(u[j]);
  }
}
  return p;
}
