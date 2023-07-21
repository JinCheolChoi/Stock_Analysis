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

  LogicalVector Both_Direction_ = as<LogicalVector>(Which_Signals["Both_Direction"]);

  int n = Both_Direction_.size();

  // create output vectors
  IntegerVector Net_Quantity_(n);
  std::fill(Net_Quantity_.begin(), Net_Quantity_.end(), 0);
  //Net_Quantity_[0] = Quantity_[0];

  IntegerVector Remove_(n);
  std::fill(Remove_.begin(), Remove_.end(), 0);

  int Both_Direction_Ind = 0;

  int ind = 0;
  int while_i = 0;
  while(while_i==0){
    if(Detail_[ind]=="BTC" ||
       Detail_[ind]=="STC"){
      Net_Quantity_[ind]=0;
      Remove_[ind]=1;
    }else{
      while_i++;
      Net_Quantity_[ind]=Quantity_[ind];
    }
    ind++;
  }

  for (int i = ind; i < n; ++i)
  {
    // adjust Quantity[i]
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

      switch (Both_Direction_[i])
      {
      case true:
        if (Both_Direction_Ind == Ind_[i])
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
          Both_Direction_Ind = Ind_[i];

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
      switch (Both_Direction_[i])
      {
      case true:
        if (Both_Direction_Ind == Ind_[i])
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1];
          Remove_[i] = 1;

          continue;
        }

        // Always first try to clear the existing positions
        if (Detail_[i] == "STC")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
          Both_Direction_Ind = Ind_[i];

          continue;
        }

        if (Detail_[i] == "BTO")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
          Both_Direction_Ind = Ind_[i];

          continue;
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
      switch (Both_Direction_[i])
      {
      case true:
        if (Both_Direction_Ind == Ind_[i])
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1];
          Remove_[i] = 1;

          continue;
        }

        // Always first try to clear the existing positions
        if (Detail_[i] == "BTC")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
          Both_Direction_Ind = Ind_[i];

          continue;
        }

        if (Detail_[i] == "STO")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
          Both_Direction_Ind = Ind_[i];

          continue;
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
      switch (Both_Direction_[i])
      {
      case true:
        if (Both_Direction_Ind == Ind_[i])
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1];
          Remove_[i] = 1;

          continue;
        }

        // This part allows to force the long position entrance when there is no position filled yet while Sigs_N indicates to enter both positions at the same time
        if (Detail_[i] == "BTO")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
          Both_Direction_Ind = Ind_[i];

          continue;
        }
        else if (Detail_[i] == "STO")
        {
          Net_Quantity_[i] = Net_Quantity_[i - 1] + Quantity_[i];
          Both_Direction_Ind = Ind_[i];

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
List Order_Filled(List Which_Signals, LogicalVector Both_Direction_, int Max_Orders){

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
        if(Both_Direction_[Ind]==TRUE){
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

  return List::create(Quantity_, Net_Quantity_, Remove_, Both_Direction_);
} */