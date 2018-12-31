NFL Playoff Simulation
================

This repository will simulate the 2019 NFL Playoffs 
--------------------------------------------------------------------------------



1,000,001 simulations takes awhile to run. If you lower the num_sims it will be quicker to play around with



``` r
set.seed(1234)
while (i <= num_sims) {
  
  ##### Wild Card Weekend
  # AFC
  wc.1.afc <- simulate.game(afc.3, afc.6)
  wc.2.afc <- simulate.game(afc.4, afc.5)
  
  # NFC
  wc.1.nfc <- simulate.game(nfc.3, nfc.6)
  wc.2.nfc <- simulate.game(nfc.4, nfc.5)
  
  ##### Divisional Weekend
  # AFC
  dw.1.afc <- simulate.game(afc.1, wc.2.afc)
  dw.2.afc <- simulate.game(afc.2, wc.1.afc)
  
  # NFC
  dw.1.nfc <- simulate.game(nfc.1, wc.2.nfc)
  dw.2.nfc <- simulate.game(nfc.2, wc.1.nfc)
  
  ##### Championship Weekend
  # AFC
  cw.1.afc <- simulate.game(dw.1.afc, dw.2.afc)
  
  # NFC
  cw.1.nfc <- simulate.game(dw.1.nfc, dw.2.nfc)
  
  ##### Superbowl
  champ <- simulate.game(cw.1.afc, cw.1.nfc)
  
  #print(paste0("This is the winner ",champ))
  
  results.all <- c( 
    i,  
    wc.1.afc,
    wc.2.afc,
    wc.1.nfc,
    wc.2.nfc,
    dw.1.afc,
    dw.2.afc,
    dw.1.nfc,
    dw.2.nfc,
    cw.1.afc,
    cw.1.nfc,
    champ
  )
  
  simulation.results <- c(simulation.results, results.all)
  
  i <- i + 1 
}
```

View Results
---------

``` r
# View results
kable(all.chances.df)
```

|team |Divisional |Conference |Superbowl |Champion |
|:----|:----------|:----------|:---------|:--------|
|KC   |0%         |66.36%     |44.88%    |27.13%   |
|NO   |0%         |58.41%     |32.96%    |17.74%   |
|LAR  |0%         |59.6%      |31.67%    |15.43%   |
|NE   |0%         |64.13%     |27.34%    |14.29%   |
|CHI  |65.25%     |28.13%     |13.89%    |5.45%    |
|SEA  |57.82%     |25.36%     |11.94%    |4.71%    |
|LAC  |47.66%     |18.16%     |9.34%     |4.5%     |
|BAL  |52.34%     |15.48%     |6.89%     |3.16%    |
|HOU  |56.79%     |20.73%     |6.7%      |3%       |
|IND  |43.21%     |15.14%     |4.85%     |2.12%    |
|DAL  |42.18%     |16.22%     |5.14%     |1.29%    |
|PHI  |34.75%     |12.26%     |4.4%      |1.16%    |

# The Chiefs have the best chance of winning it all!

Remember probabilities are just that... probabilities... It doesn't mean that they WILL win. But they have the best chance to win. I love the playoffs! Anything can happen! Let's see how this plays out!  

Feel free to play around with the percentages in the "playoff_games.csv" file to see how it affects the simulation.