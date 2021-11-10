// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

//Create the companion form of the transition matrix B
// [[Rcpp::export]]
arma::mat Comp_Form(arma::mat B){
  uword r = B.n_rows;
  uword c = B.n_cols;
  mat A   = join_vert(B, join_horiz(eye<mat>(c-r,c-r), zeros<mat>(c-r,r)));
  return(A);
}

// Get initial variance for the Kalman Filter
// [[Rcpp::export]]
arma::mat Long_Run_Var(arma::mat A,
                       arma::mat Q,
                       arma::uword m,
                       arma::uword p){
  uword sA = A.n_cols;
  uword pp = sA/m;
  uword mp = m*p;
  double mp2 = mp*mp;
  mat B(A(span(0,mp-1), span(0,mp-1))); mat BB; mat b;
  mat XX(eye<mat>(mp2, mp2) - kron(B,B));
  vec vQ(reshape(Q(span(0,m*p-1),span(0,m*p-1)), mp2, 1));
  mat P(reshape(solve(XX, vQ), mp, mp));
  //P = P(span(0,m-1),span(0,m-1));
  mat PP(sA,sA,fill::zeros);
  for(uword j = 0; j<pp; j++){
    BB = B;
    for(uword k=j+1; k<pp; k++){
      b = BB*P;
      PP(span(m*j,m*j+m-1), span(m*k,m*k+m-1)) = b(span(0,m-1),span(0,m-1));
      BB = B*BB;
    }
  }
  PP = PP + trans(PP) + kron(eye<mat>(pp,pp), P(span(0,m-1), span(0,m-1)));
  return(PP);
}

// [[Rcpp::export]]
arma::uvec finite_cols(arma::mat X){
  uword T = X.n_cols;
  vec x;
  uvec rtrn = regspace<uvec>(0,T-1);
  for(uword j=T; j!=0; j--){
    x = X.col(j-1);
    if(!x.is_finite()){
      rtrn.shed_row(j-1);
    }
  }
  return(rtrn);
}

// [[Rcpp::export]]
arma::uvec count_finite(arma::mat X){
  uword T = X.n_cols;
  vec x;
  uvec id;
  uvec rtrn(T, fill::zeros);
  for(uword j=0; j<T; j++){
    x = X.col(j);
    id = find_finite(x);
    rtrn(j) = id.n_elem;
  }
  return(rtrn);
}

// [[Rcpp::export]]
arma::uvec any_obs_cols(arma::mat X){
  uword T = X.n_cols;
  uvec x;
  uvec rtrn = regspace<uvec>(0,T-1);
  for(uword j=T; j!=0; j--){
    x = find_finite(X.col(j-1));
    if(x.n_elem==0){
      rtrn.shed_row(j-1);
    }
  }
  return(rtrn);
}

//Stack times series data in VAR format
// [[Rcpp::export]]
arma:: mat Stack_Obs(arma::mat nn, arma::uword p, arma::uword r = 0){
  uword rr = nn.n_rows;
  uword mn = nn.n_cols;
  if(r == 0){
    r = rr-p+1;
  }
  if(rr-p+1 != r){
    stop("Length of input nn and length of data r do not agree.");
  }
  mat N(r,mn*p, fill::zeros);
  uword indx = 0;
  for(uword j = 1; j<=p; j++){
    N.cols(indx,indx+mn-1) = nn.rows(p-j,rr-j);
    indx = indx+mn;
  }
  return(N);
}

// [[Rcpp::export]]
arma::uword which_date_leq(Rcpp::Date date,
                           std::vector<Date> Dvec){
  uword j;
  for(j = 0; j < Dvec.size(); j++){
    if(Dvec[j] > date){
      break;
    }
  }
  return(j); //R indexing
}

// [[Rcpp::export]]
arma::uword which_date_geq(Rcpp::Date date,
                           std::vector<Date> Dvec){
  uword j;
  for(j = Dvec.size(); j>0; j--){
    if(Dvec[j-1] < date){
      break;
    }
  }
  return(j+1); //R indexing
}

// [[Rcpp::export]]
arma::uword which_date_closest(Rcpp::Date date,
                               std::vector<Date> Dvec){
  uword j;
  for(j = 0; j < Dvec.size(); j++){
    if(Dvec[j] > date){
      break;
    }
  }
  if(j > 1 && j < Dvec.size()){
    int d_less = date - Dvec[j-1];
    int d_more = Dvec[j] - date;
    if(d_less > d_more){
      j++;
    }
  }
  return(j); //R indexing
}

// [[Rcpp::export]]
arma::uvec which_date_closest_ordered(std::vector<Date> FromVec,    //for each date in this vector
                                      std::vector<Date> IndVec){    //find the index of the closest date in this vector
  int d_more, d_less;
  uword j = 0;
  uword k = 1;
  uvec idx(FromVec.size(), fill::ones);
  idx = idx*IndVec.size();
  while(j<FromVec.size()){
    while(k<IndVec.size()){
      if(FromVec[j] < IndVec[k]){
        d_less = FromVec[j] - IndVec[k-1];
        d_more = IndVec[k] - FromVec[j];
        if(d_more<d_less){
          idx(j) = k + 1; //R indexing
          k++;
          break;
        }else{
          idx(j) = k; //R indexing
          break;
        }
      }else{
        k++;
      }
    }
    j++;
  }
  return(idx);
}

//return the last day of the year
// [[Rcpp::export]]
arma::uvec Day(std::vector<Date> dte){
  uvec d(dte.size());
  Rcpp::Date tmp;
  uword dd;
  for(uword j=0; j<dte.size(); j++){
    tmp  = dte[j];
    dd = tmp.getDay();
    d(j) = dd;
  }
  return(d);
}

// [[Rcpp::export]]
Rcpp::Date replace_day(Rcpp::Date date,
                        int new_day){
  Date d(date.getYear(),date.getMonth(),new_day);
  return(d);
}


// [[Rcpp::export]]
int MonthDays(double year,
              double month){
  int days;
  if((month == 1) || (month == 3) || (month == 5) || (month == 7) || (month == 8) || (month == 10) || (month == 12)){
    days = 31;
  }
  else if((month == 4) || (month == 6) || (month == 9) || (month == 11) ){
    days = 30;
  }
  else if(round((year-1940)/4) == ((year-1940)/4) ){
    days = 29;
  }
  else{
    days = 28;
  }
  return(days);
}

// // [[Rcpp::export]]
// arma::uvec MonthDaysVec(arma::uvec year,
//                         arma::uvec month){
//   if(year.n_elem !=  month.n_elem{
//     Rcpp::stop("Year and month vectors must be the same size")
//   }
//   uvec days(year.n_elem);
//
//   return(days);
// }

// Get year and month for shift
ivec month_13(int val){
  int yr = val/12;
  int mnth = val%12;
  if(mnth <= 0){
    mnth += 12;
    yr -= 1;
  } 
  ivec rtrn = {yr, mnth};
  return(rtrn);
}

//return last day for the given month
// [[Rcpp::export]]
std::vector<Date> End_of_Month(std::vector<Date> date, int shift = 0){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int mnth;
  int yr;
  ivec yrm;
  if(date.size()>0){
    for(uword j=0; j<date.size(); j++){
      tmp  = date[j];
      yrm = month_13(tmp.getMonth() + shift);
      yr = tmp.getYear() + yrm(0);
      mnth = yrm(1);
      d[j] = Date(yr, mnth, MonthDays(yr, mnth));
    }
  }
  return(d);
}

// [[Rcpp::export]]
std::vector<Date> End_next_Month(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int year, month, days;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    month = tmp.getMonth()+1;
    year = tmp.getYear();
    if(month==13){
      month = 1;
      year = year + 1;
    }
    days = MonthDays(year, month);
    d[j] = Date(year, month, days);
  }
  return(d);
}

// [[Rcpp::export]]
std::vector<Date> End_previous_Month(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int year, month, days;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    month = tmp.getMonth()-1;
    year = tmp.getYear();
    if(month==0){
      month = 12;
      year = year - 1;
    }
    days = MonthDays(year, month);
    d[j] = Date(year, month, days);
  }
  return(d);
}

//return last day for the given quarter
// [[Rcpp::export]]
std::vector<Date> End_of_Quarter(std::vector<Date> date, int shift = 0){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int mnth;
  int yr;
  ivec yrm;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    yrm = month_13(tmp.getMonth() + 3*shift);
    yr = tmp.getYear() + yrm(0);
    mnth = yrm(1);
    if(mnth == 1 || mnth == 2){
      mnth = 3;
    }else if(mnth == 4 || mnth == 5){
      mnth = 6;
    }else if(mnth == 7 || mnth == 8){
      mnth = 9;
    }else if(mnth == 10 || mnth == 11){
      mnth = 12;
    }
    d[j] = Date(yr, mnth, MonthDays(tmp.getYear(), mnth));
  }
  return(d);
}

//return first day for previous quarter
// [[Rcpp::export]]
std::vector<Date> First_previous_Quarter(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int yr;
  int mnth;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    mnth = tmp.getMonth();
    yr = tmp.getYear();
    if(mnth == 1 || mnth == 2 || mnth == 3){
      mnth = 10;
      yr = yr-1;
    }else if(mnth == 4 || mnth == 5 || mnth == 6){
      mnth = 1;
    }else if(mnth == 7 || mnth == 8 || mnth == 9){
      mnth = 4;
    }else if(mnth == 10 || mnth == 11 || mnth == 12){
      mnth = 7;
    }
    d[j] = Date(yr, mnth, 1);
  }
  return(d);
}

//return last day for previous quarter
// [[Rcpp::export]]
std::vector<Date> End_previous_Quarter(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int yr;
  int mnth;
  int days;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    mnth = tmp.getMonth();
    yr = tmp.getYear();
    if(mnth == 1 || mnth == 2 || mnth == 3){
      mnth = 12;
      yr = yr-1;
    }else if(mnth == 4 || mnth == 5 || mnth == 6){
      mnth = 3;
    }else if(mnth == 7 || mnth == 8 || mnth == 9){
      mnth = 6;
    }else if(mnth == 10 || mnth == 11 || mnth == 12){
      mnth = 9;
    }
    days = MonthDays(yr, mnth);
    d[j] = Date(yr, mnth, days);
  }
  return(d);
}

//return first day of the quarter
// [[Rcpp::export]]
std::vector<Date> First_Of_Quarter(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int yr;
  int mnth;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    mnth = tmp.getMonth();
    yr = tmp.getYear();
    if(mnth == 1 || mnth == 2 || mnth == 3){
      mnth = 1;
    }else if(mnth == 4 || mnth == 5 || mnth == 6){
      mnth = 4;
    }else if(mnth == 7 || mnth == 8 || mnth == 9){
      mnth = 7;
    }else if(mnth == 10 || mnth == 11 || mnth == 12){
      mnth = 10;
    }
    d[j] = Date(yr, mnth, 1);
  }
  return(d);
}

//return the last day of the year
// [[Rcpp::export]]
std::vector<Date> End_Of_Year(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int yr;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    yr = tmp.getYear();
    d[j] = Date(yr, 12, 31);
  }
  return(d);
}

// [[Rcpp::export]]
arma::umat NumDum(arma::vec x){
  vec unq = unique(x);
  uword k = unq.n_elem;
  umat rtrn(x.n_elem, k, fill::zeros);
  umat idx;
  uvec z(x.n_elem);
  for(uword j = 0; j<k; j++){
    z.zeros();
    z.elem(find(x == unq(j))).ones();
    rtrn.col(j) = z;
  }
  return(rtrn);
}

// [[Rcpp::export]]
arma::vec Fill_Forward(arma::vec x){
  for(uword j=1; j<x.n_elem; j++){
    if(!std::isfinite(x(j))){
      x(j) = x(j-1);
    }
  }
  return(x);
}

// [[Rcpp::export]]
arma::vec rollmean_cpp(arma::vec x, arma::uword n){
  uword T = x.n_elem;
  vec y(T);
  y.fill(datum::nan);
  vec tmp;
  for(uword j=n-1; j<x.n_elem; j++){
    tmp = x(span(j-n+1,j));
    if(tmp.is_finite()){
      y(j) = mean(tmp);
    }
  }
  return(y);
}

// [[Rcpp::export]]
arma::vec RollMax(arma::vec x, arma::uword n){
  uword T = x.n_elem;
  vec y(T);
  vec xt;
  x.replace(datum::nan, -datum::inf);
  for(uword j=0; j<n; j++){
    xt = x(span(0,j));
    y(j) = xt.max();
  }
  for(uword j=n; j<T; j++){
    xt = x(span(j-n+1,j));
    y(j) = xt.max();
  }
  return(y);
}

// [[Rcpp::export]]
arma::vec RollMin(arma::vec x, arma::uword n){
  uword T = x.n_elem;
  vec y(T);
  vec xt;
  x.replace(datum::nan, datum::inf);
  for(uword j=0; j<n; j++){
    xt = x(span(0,j));
    y(j) = xt.min();
  }
  for(uword j=n; j<T; j++){
    xt = x(span(j-n+1,j));
    y(j) = xt.min();
  }
  return(y);
}





