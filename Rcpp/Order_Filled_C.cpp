#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List Order_Filled_C(List Which_Signals, int Max_Orders)
{

  // create vectors from Which_Signals
  IntegerVector Ind_ = as<IntegerVector>(Which_Signals["Ind"]);

  CharacterVector Action_ = as<CharacterVector>(Which_Signals["Action"]);

  CharacterVector Detail_ = as<CharacterVector>(Which_Signals["Detail"]);

  IntegerVector Quantity_ = as<IntegerVector>(Which_Signals["Quantity"]);

  LogicalVector Simultaneous_ = as<LogicalVector>(Which_Signals["Simultaneous"]);

  int n = Simultaneous_.size();

  int exist = 0;

  // create output vectors
  IntegerVector Net_Quantity_(n);
  std::fill(Net_Quantity_.begin(), Net_Quantity_.end(), 0);
  // Net_Quantity_[0] = Quantity_[0];

  IntegerVector Remove_(n);
  std::fill(Remove_.begin(), Remove_.end(), 0);

  int Simultaneous_Ind = 0;

  // begin orders with an open position
  // thus, indicate removal in Remove_[i] for closing positions until the first open position
  int ind = 0;
  int while_i = 0;
  while (while_i == 0)
  {
    if (Detail_[ind] == "BTC" ||
        Detail_[ind] == "STC")
    {
      Net_Quantity_[ind] = 0;
      Remove_[ind] = 1;
    }
    else
    {
      while_i++;
      // Net_Quantity_[ind] = Quantity_[ind];
      Net_Quantity_[ind] = ((Quantity_[ind] > 0) - (Quantity_[ind] < 0)) * min(abs(Quantity_[ind]), Max_Orders);
    }
    ind++;
  }

  Quantity_[0] = Net_Quantity_[0];

  for (int i = ind; i < n; ++i)
  {
    // adjust Quantity[i]
    {
      // comment the following bracket part to allow switching
      {
        if (
            (Net_Quantity_[i - 1] < 0) &&
            (Net_Quantity_[i - 1] + Quantity_[i] > 0) &&
            (Net_Quantity_[i - 1] + Quantity_[i] <= Max_Orders))
        {
          Quantity_[i] = -Net_Quantity_[i - 1];
        }
        else if (
            (Net_Quantity_[i - 1] > 0) &&
            (Net_Quantity_[i - 1] + Quantity_[i] < 0) &&
            (Net_Quantity_[i - 1] + Quantity_[i] >= -Max_Orders))
        {
          Quantity_[i] = -Net_Quantity_[i - 1];
        }
      }

      if (abs(Net_Quantity_[i - 1] + Quantity_[i]) > Max_Orders)
      {
        if (Detail_[i] == "BTC" ||
            Detail_[i] == "STC")
        {
          if (Quantity_[i] < 0)
          {
            Quantity_[i] = max(Quantity_[i], -(Max_Orders));
          }
          else if (Quantity_[i] >= 0)
          {
            Quantity_[i] = min(Quantity_[i], Max_Orders);
          }
        }
        else
        {
          if (Quantity_[i] < 0)
          {
            Quantity_[i] = max(Quantity_[i], -(Max_Orders + Net_Quantity_[i - 1]));
          }
          else if (Quantity_[i] >= 0)
          {
            Quantity_[i] = min(Quantity_[i], Max_Orders - Net_Quantity_[i - 1]);
          }
        }
      }
    }

    // if abs(Net_Quantity_[i-1])>=Max_Orders
    if (abs(Net_Quantity_[i - 1]) >= Max_Orders)
    {
      if (Detail_[i] == "BTO" ||
          Detail_[i] == "STO")
      {
        Net_Quantity_[i] = Net_Quantity_[i - 1];
        Remove_[i] = 1;

        continue;
      }

      switch (Simultaneous_[i])
      {
      case true:
        // indicate removal in Remove_[i] for orders that occur at the same time in both directions (long & short)
        // such duplicated orders are removed from the 2nd one (the very 1st one is processed by the next for statment)
        if (Simultaneous_Ind == Ind_[i])
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1];
          Remove_[i] = 1;

          continue;
        }

        // Always first try to clear the existing positions
        if ((Net_Quantity_[i - 1] > 0 && Detail_[i] == "STC") ||
            (Net_Quantity_[i - 1] < 0 && Detail_[i] == "BTC"))
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
          Simultaneous_Ind = Ind_[i]; // update Simultaneous_Ind after the 1st duplicated order is recorded

          continue;
        }

        break;

      case false:
        if ((Net_Quantity_[i - 1] > 0 && Detail_[i] == "STC") ||
            (Net_Quantity_[i - 1] < 0 && Detail_[i] == "BTC"))
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];

          continue;
        }
        break;
      }

      Net_Quantity_[i] = Net_Quantity_[i - 1];

      Remove_[i] = 1;

      continue;
    }

    // if(Net_Quantity_[i-1]>0)
    if (Net_Quantity_[i - 1] > 0)
    {
      switch (Simultaneous_[i])
      {
      case true:
        // indicate removal in Remove_[i] for orders that occur at the same time in both directions (long & short)
        // such duplicated orders are removed from the 2nd one (the very 1st one is processed by the next for statment)
        if (Simultaneous_Ind == Ind_[i])
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1];
          Remove_[i] = 1;

          continue;
        }

        // Always first try to clear the existing positions
        exist = 0;
        for (int which_i = 0; which_i < Ind_.size(); ++which_i)
        {
          if (Ind_[which_i] == Ind_[i])
          {
            if (Detail_[which_i] == "STC")
            {
              exist = 1;
            }
          }
        }

        if (exist == 1)
        {
          if (Detail_[i] == "STC")
          {
            Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
            Simultaneous_Ind = Ind_[i]; // update Simultaneous_Ind after the 1st duplicated order is recorded

            continue;
          }
        }
        else if (exist == 0)
        {
          if (Detail_[i] == "BTO")
          {
            Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
            Simultaneous_Ind = Ind_[i]; // update Simultaneous_Ind after the 1st duplicated order is recorded

            continue;
          }
        }

        break;

      case false:
        if (Detail_[i] == "BTO" ||
            Detail_[i] == "STC")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];

          continue;
        }
        break;
      }

      Net_Quantity_[i] = Net_Quantity_[i - 1];

      Remove_[i] = 1;

      continue;
    }

    // if(Net_Quantity_[i-1]<0)
    if (Net_Quantity_[i - 1] < 0)
    {
      switch (Simultaneous_[i])
      {
      case true:
        // indicate removal in Remove_[i] for orders that occur at the same time in both directions (long & short)
        // such duplicated orders are removed from the 2nd one (the very 1st one is processed by the next for statment)
        if (Simultaneous_Ind == Ind_[i])
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1];
          Remove_[i] = 1;

          continue;
        }

        // Always first try to clear the existing positions
        exist = 0;
        for (int which_i = 0; which_i < Ind_.size(); ++which_i)
        {
          if (Ind_[which_i] == Ind_[i])
          {
            if (Detail_[which_i] == "BTC")
            {
              exist = 1;
            }
          }
        }

        if (exist == 1)
        {
          if (Detail_[i] == "BTC")
          {
            Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
            Simultaneous_Ind = Ind_[i]; // update Simultaneous_Ind after the 1st duplicated order is recorded

            continue;
          }
        }
        else if (exist == 0)
        {
          if (Detail_[i] == "STO")
          {
            Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
            Simultaneous_Ind = Ind_[i]; // update Simultaneous_Ind after the 1st duplicated order is recorded

            continue;
          }
        }

        break;

      case false:
        if (Detail_[i] == "BTC" ||
            Detail_[i] == "STO")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];

          continue;
        }
        break;
      }

      Net_Quantity_[i] = Net_Quantity_[i - 1];

      Remove_[i] = 1;

      continue;
    }

    // Net_Quantity_[i-1]==0
    if (Net_Quantity_[i - 1] == 0)
    {
      switch (Simultaneous_[i])
      {
      case true:
        // indicate removal in Remove_[i] for orders that occur at the same time in both directions (long & short)
        // such duplicated orders are removed from the 2nd one (the very 1st one is processed by the next for statment)
        if (Simultaneous_Ind == Ind_[i])
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1];
          Remove_[i] = 1;

          continue;
        }

        // This part allows to force the long position entrance when there is no position filled yet while Sigs_N indicates to enter both positions at the same time
        if (Detail_[i] == "BTO")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
          Simultaneous_Ind = Ind_[i]; // update Simultaneous_Ind after the 1st duplicated order is recorded

          continue;
        }
        else if (Detail_[i] == "STO")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
          Simultaneous_Ind = Ind_[i]; // update Simultaneous_Ind after the 1st duplicated order is recorded

          continue;
        }

        break;

      case false:
        if (Detail_[i] == "BTO" ||
            Detail_[i] == "STO")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];

          continue;
        }

        break;
      }

      Net_Quantity_[i] = Net_Quantity_[i - 1];

      Remove_[i] = 1;

      continue;
    }
  }

  return List::create(
      _["Quantity"] = Quantity_,
      _["Net_Quantity"] = Net_Quantity_,
      _["Remove"] = Remove_);
}

/* // [[Rcpp::export]]
List Order_Filled(List Which_Signals, LogicalVector Simultaneous_, int Max_Orders){

  IntegerVector Ind_ = as<IntegerVector>(Which_Signals["Ind"]);

  CharacterVector Action_ = as<CharacterVector>(Which_Signals["Action"]);
  int n=Action_.size();

  CharacterVector Detail_ = as<CharacterVector>(Which_Signals["Detail"]);

  IntegerVector Quantity_ = as<IntegerVector>(Which_Signals["Quantity"]);
  //Quantity_[1]=-5;
  //Quantity_[2]=10;

  IntegerVector Net_Quantity_(n);
  //std::vector<int> Net_Quantity_ (n);
  std::fill(Net_Quantity_.begin(), Net_Quantity_.end(), 0);
  Net_Quantity_[0]=Quantity_[0];

  IntegerVector Remove_(n);
  std::fill(Remove_.begin(), Remove_.end(), 0);

  for(int Ind = 1; Ind < n; ++Ind) {
    //
    if((Net_Quantity_[Ind-1]>=0 && Detail_[Ind]=="BTC")||
       (Net_Quantity_[Ind-1]<=0 && Detail_[Ind]=="STC")){
          Net_Quantity_[Ind]=Net_Quantity_[Ind-1];
          Remove_[Ind]=1;
       }else{

        //
        if(abs(Net_Quantity_[Ind-1]+Quantity_[Ind])>Max_Orders){
          if(Quantity_[Ind]<0){
              Quantity_[Ind]=-(Max_Orders+Quantity_[Ind-1]);
              Net_Quantity_[Ind]=-Max_Orders;
        }else if(Quantity_[Ind]>=0){
          Quantity_[Ind]=Max_Orders-Quantity_[Ind-1];
          Net_Quantity_[Ind]=Max_Orders;
        }

        if((signbit(Net_Quantity_[Ind-1])==signbit(Net_Quantity_[Ind])) && (abs(Net_Quantity_[Ind-1])>=Max_Orders)){
            Quantity_[Ind]=0;
            Remove_[Ind]=1;
        }else{
            Remove_[Ind]=0;
        }
      }else{ //
        if(Simultaneous_[Ind]==TRUE){
          if(Net_Quantity_[Ind-1]<0 && Action_[Ind]=="Sell"){
              Net_Quantity_[Ind]=Net_Quantity_[Ind-1];
              Remove_[Ind]=1;
            }else if(Net_Quantity_[Ind-1]<0 && Action_[Ind]=="Buy"){
            Net_Quantity_[Ind]=Net_Quantity_[Ind-1]+Quantity_[Ind];
              Remove_[Ind]=0;
          }else if(Net_Quantity_[Ind-1]>0 && Action_[Ind]=="Buy"){
              Net_Quantity_[Ind]=Net_Quantity_[Ind-1];
              Remove_[Ind]=1;
          }else if(Net_Quantity_[Ind-1]>0 && Action_[Ind]=="Sell"){
              Net_Quantity_[Ind]=Net_Quantity_[Ind-1]+Quantity_[Ind];
              Remove_[Ind]=0;
          }else if(Net_Quantity_[Ind-1]==0){
            Net_Quantity_[Ind]=Net_Quantity_[Ind-1]+Quantity_[Ind];
            Remove_[Ind]=0;
            if(Detail_[Ind]=="STO"){
                for(int Ind_2 = 0; Ind_2 < n; ++Ind_2){
                  if(Ind_[Ind_2]==Ind_[Ind]){
                    if(Detail_[Ind_2]=="BTC"){
                        Net_Quantity_[Ind] = Net_Quantity_[Ind - 1];
                        Remove_[Ind] = 1;
                        continue;
                    }
                  }
                }
              }
          }
        }else{
          Net_Quantity_[Ind]=Net_Quantity_[Ind-1]+Quantity_[Ind];
          Remove_[Ind]=0;
        }
      }
    }
  }

  return List::create(Quantity_, Net_Quantity_, Remove_, Simultaneous_);
} */