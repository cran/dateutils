// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Comp_Form
arma::mat Comp_Form(arma::mat B);
RcppExport SEXP _dateutils_Comp_Form(SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(Comp_Form(B));
    return rcpp_result_gen;
END_RCPP
}
// Long_Run_Var
arma::mat Long_Run_Var(arma::mat A, arma::mat Q, arma::uword m, arma::uword p);
RcppExport SEXP _dateutils_Long_Run_Var(SEXP ASEXP, SEXP QSEXP, SEXP mSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Q(QSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type m(mSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(Long_Run_Var(A, Q, m, p));
    return rcpp_result_gen;
END_RCPP
}
// finite_cols
arma::uvec finite_cols(arma::mat X);
RcppExport SEXP _dateutils_finite_cols(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(finite_cols(X));
    return rcpp_result_gen;
END_RCPP
}
// count_finite
arma::uvec count_finite(arma::mat X);
RcppExport SEXP _dateutils_count_finite(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(count_finite(X));
    return rcpp_result_gen;
END_RCPP
}
// any_obs_cols
arma::uvec any_obs_cols(arma::mat X);
RcppExport SEXP _dateutils_any_obs_cols(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(any_obs_cols(X));
    return rcpp_result_gen;
END_RCPP
}
// Stack_Obs
arma:: mat Stack_Obs(arma::mat nn, arma::uword p, arma::uword r);
RcppExport SEXP _dateutils_Stack_Obs(SEXP nnSEXP, SEXP pSEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type nn(nnSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type p(pSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(Stack_Obs(nn, p, r));
    return rcpp_result_gen;
END_RCPP
}
// which_date_leq
arma::uword which_date_leq(Rcpp::Date date, std::vector<Date> Dvec);
RcppExport SEXP _dateutils_which_date_leq(SEXP dateSEXP, SEXP DvecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::Date >::type date(dateSEXP);
    Rcpp::traits::input_parameter< std::vector<Date> >::type Dvec(DvecSEXP);
    rcpp_result_gen = Rcpp::wrap(which_date_leq(date, Dvec));
    return rcpp_result_gen;
END_RCPP
}
// which_date_geq
arma::uword which_date_geq(Rcpp::Date date, std::vector<Date> Dvec);
RcppExport SEXP _dateutils_which_date_geq(SEXP dateSEXP, SEXP DvecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::Date >::type date(dateSEXP);
    Rcpp::traits::input_parameter< std::vector<Date> >::type Dvec(DvecSEXP);
    rcpp_result_gen = Rcpp::wrap(which_date_geq(date, Dvec));
    return rcpp_result_gen;
END_RCPP
}
// which_date_closest
arma::uword which_date_closest(Rcpp::Date date, std::vector<Date> Dvec);
RcppExport SEXP _dateutils_which_date_closest(SEXP dateSEXP, SEXP DvecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::Date >::type date(dateSEXP);
    Rcpp::traits::input_parameter< std::vector<Date> >::type Dvec(DvecSEXP);
    rcpp_result_gen = Rcpp::wrap(which_date_closest(date, Dvec));
    return rcpp_result_gen;
END_RCPP
}
// which_date_closest_ordered
arma::uvec which_date_closest_ordered(std::vector<Date> FromVec, std::vector<Date> IndVec);
RcppExport SEXP _dateutils_which_date_closest_ordered(SEXP FromVecSEXP, SEXP IndVecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type FromVec(FromVecSEXP);
    Rcpp::traits::input_parameter< std::vector<Date> >::type IndVec(IndVecSEXP);
    rcpp_result_gen = Rcpp::wrap(which_date_closest_ordered(FromVec, IndVec));
    return rcpp_result_gen;
END_RCPP
}
// Day
arma::uvec Day(std::vector<Date> dte);
RcppExport SEXP _dateutils_Day(SEXP dteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type dte(dteSEXP);
    rcpp_result_gen = Rcpp::wrap(Day(dte));
    return rcpp_result_gen;
END_RCPP
}
// replace_day
Rcpp::Date replace_day(Rcpp::Date date, int new_day);
RcppExport SEXP _dateutils_replace_day(SEXP dateSEXP, SEXP new_daySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::Date >::type date(dateSEXP);
    Rcpp::traits::input_parameter< int >::type new_day(new_daySEXP);
    rcpp_result_gen = Rcpp::wrap(replace_day(date, new_day));
    return rcpp_result_gen;
END_RCPP
}
// MonthDays
int MonthDays(double year, double month);
RcppExport SEXP _dateutils_MonthDays(SEXP yearSEXP, SEXP monthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type year(yearSEXP);
    Rcpp::traits::input_parameter< double >::type month(monthSEXP);
    rcpp_result_gen = Rcpp::wrap(MonthDays(year, month));
    return rcpp_result_gen;
END_RCPP
}
// End_of_Month
std::vector<Date> End_of_Month(std::vector<Date> date, int shift);
RcppExport SEXP _dateutils_End_of_Month(SEXP dateSEXP, SEXP shiftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type date(dateSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    rcpp_result_gen = Rcpp::wrap(End_of_Month(date, shift));
    return rcpp_result_gen;
END_RCPP
}
// End_next_Month
std::vector<Date> End_next_Month(std::vector<Date> date);
RcppExport SEXP _dateutils_End_next_Month(SEXP dateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type date(dateSEXP);
    rcpp_result_gen = Rcpp::wrap(End_next_Month(date));
    return rcpp_result_gen;
END_RCPP
}
// End_previous_Month
std::vector<Date> End_previous_Month(std::vector<Date> date);
RcppExport SEXP _dateutils_End_previous_Month(SEXP dateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type date(dateSEXP);
    rcpp_result_gen = Rcpp::wrap(End_previous_Month(date));
    return rcpp_result_gen;
END_RCPP
}
// End_of_Quarter
std::vector<Date> End_of_Quarter(std::vector<Date> date, int shift);
RcppExport SEXP _dateutils_End_of_Quarter(SEXP dateSEXP, SEXP shiftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type date(dateSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    rcpp_result_gen = Rcpp::wrap(End_of_Quarter(date, shift));
    return rcpp_result_gen;
END_RCPP
}
// First_previous_Quarter
std::vector<Date> First_previous_Quarter(std::vector<Date> date);
RcppExport SEXP _dateutils_First_previous_Quarter(SEXP dateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type date(dateSEXP);
    rcpp_result_gen = Rcpp::wrap(First_previous_Quarter(date));
    return rcpp_result_gen;
END_RCPP
}
// End_previous_Quarter
std::vector<Date> End_previous_Quarter(std::vector<Date> date);
RcppExport SEXP _dateutils_End_previous_Quarter(SEXP dateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type date(dateSEXP);
    rcpp_result_gen = Rcpp::wrap(End_previous_Quarter(date));
    return rcpp_result_gen;
END_RCPP
}
// First_Of_Quarter
std::vector<Date> First_Of_Quarter(std::vector<Date> date);
RcppExport SEXP _dateutils_First_Of_Quarter(SEXP dateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type date(dateSEXP);
    rcpp_result_gen = Rcpp::wrap(First_Of_Quarter(date));
    return rcpp_result_gen;
END_RCPP
}
// End_Of_Year
std::vector<Date> End_Of_Year(std::vector<Date> date);
RcppExport SEXP _dateutils_End_Of_Year(SEXP dateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Date> >::type date(dateSEXP);
    rcpp_result_gen = Rcpp::wrap(End_Of_Year(date));
    return rcpp_result_gen;
END_RCPP
}
// NumDum
arma::umat NumDum(arma::vec x);
RcppExport SEXP _dateutils_NumDum(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(NumDum(x));
    return rcpp_result_gen;
END_RCPP
}
// Fill_Forward
arma::vec Fill_Forward(arma::vec x);
RcppExport SEXP _dateutils_Fill_Forward(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(Fill_Forward(x));
    return rcpp_result_gen;
END_RCPP
}
// rollmean_cpp
arma::vec rollmean_cpp(arma::vec x, arma::uword n);
RcppExport SEXP _dateutils_rollmean_cpp(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(rollmean_cpp(x, n));
    return rcpp_result_gen;
END_RCPP
}
// RollMax
arma::vec RollMax(arma::vec x, arma::uword n);
RcppExport SEXP _dateutils_RollMax(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(RollMax(x, n));
    return rcpp_result_gen;
END_RCPP
}
// RollMin
arma::vec RollMin(arma::vec x, arma::uword n);
RcppExport SEXP _dateutils_RollMin(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(RollMin(x, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dateutils_Comp_Form", (DL_FUNC) &_dateutils_Comp_Form, 1},
    {"_dateutils_Long_Run_Var", (DL_FUNC) &_dateutils_Long_Run_Var, 4},
    {"_dateutils_finite_cols", (DL_FUNC) &_dateutils_finite_cols, 1},
    {"_dateutils_count_finite", (DL_FUNC) &_dateutils_count_finite, 1},
    {"_dateutils_any_obs_cols", (DL_FUNC) &_dateutils_any_obs_cols, 1},
    {"_dateutils_Stack_Obs", (DL_FUNC) &_dateutils_Stack_Obs, 3},
    {"_dateutils_which_date_leq", (DL_FUNC) &_dateutils_which_date_leq, 2},
    {"_dateutils_which_date_geq", (DL_FUNC) &_dateutils_which_date_geq, 2},
    {"_dateutils_which_date_closest", (DL_FUNC) &_dateutils_which_date_closest, 2},
    {"_dateutils_which_date_closest_ordered", (DL_FUNC) &_dateutils_which_date_closest_ordered, 2},
    {"_dateutils_Day", (DL_FUNC) &_dateutils_Day, 1},
    {"_dateutils_replace_day", (DL_FUNC) &_dateutils_replace_day, 2},
    {"_dateutils_MonthDays", (DL_FUNC) &_dateutils_MonthDays, 2},
    {"_dateutils_End_of_Month", (DL_FUNC) &_dateutils_End_of_Month, 2},
    {"_dateutils_End_next_Month", (DL_FUNC) &_dateutils_End_next_Month, 1},
    {"_dateutils_End_previous_Month", (DL_FUNC) &_dateutils_End_previous_Month, 1},
    {"_dateutils_End_of_Quarter", (DL_FUNC) &_dateutils_End_of_Quarter, 2},
    {"_dateutils_First_previous_Quarter", (DL_FUNC) &_dateutils_First_previous_Quarter, 1},
    {"_dateutils_End_previous_Quarter", (DL_FUNC) &_dateutils_End_previous_Quarter, 1},
    {"_dateutils_First_Of_Quarter", (DL_FUNC) &_dateutils_First_Of_Quarter, 1},
    {"_dateutils_End_Of_Year", (DL_FUNC) &_dateutils_End_Of_Year, 1},
    {"_dateutils_NumDum", (DL_FUNC) &_dateutils_NumDum, 1},
    {"_dateutils_Fill_Forward", (DL_FUNC) &_dateutils_Fill_Forward, 1},
    {"_dateutils_rollmean_cpp", (DL_FUNC) &_dateutils_rollmean_cpp, 2},
    {"_dateutils_RollMax", (DL_FUNC) &_dateutils_RollMax, 2},
    {"_dateutils_RollMin", (DL_FUNC) &_dateutils_RollMin, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_dateutils(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}