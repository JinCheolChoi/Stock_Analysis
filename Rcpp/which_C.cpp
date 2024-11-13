#include <Rcpp.h>
#include <vector>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<int> which_C(std::vector<int>& vec, int singular) {
    std::vector<int> indices;
    for (size_t i = 0; i < vec.size(); ++i) {
        if (vec[i] == singular) {
            indices.push_back(i + 1); // Adding 1 to match R's indexing convention
        }
    }

    
    return indices;
}