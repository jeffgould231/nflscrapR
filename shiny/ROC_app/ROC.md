## What Is The ROC Score?

ROC Score, or *Receiver Opportunity Composite Score*, is a Contested Catch metric that weights receiver opportunity as both a share of the offense, ie target share and air yard share, and raw volume, ie targets, air yards, and red zone targets, in order to better predict fantasy points in the coming weeks. It is best used as an in-season tool, and can be particularly helpful for identifying buy-low or sell-high candidates. Players who are out-producing their ROC Score would be likely to regress in the coming weeks, while players who are under-performing their ROC Score are likely to see a bounce in fantasy production. ROC Scores are out of 100 based on the distribution of 4 game ROC Scores, but over shorter samples scores can exceed this scale. We recommend using the ROC Score over a 4-6 week sample to identify players whose receiving production is not aligned with their opportunities.The ROC Score was developed using play-by-play data available from the [nflfastR package](https://github.com/mrcaseb/nflfastR)

### What ROC Score doesn’t do

* Take injuries or missed games into account - players that miss games will have lower ROC scores. If you wish to see the ROC Score for an individual receiver in the games they played, you can filter out the missed weeks in the window above

* Differentiate between positions - Wide Receivers, Tight Ends, and Running Backs are all treated equally. But because RBs frequently have negative Air Yard targets, this can down weight their ROC score. Because of this, we only show WRs and TEs in the Over/Under Producer table below

* Look at defenses faced or upcoming

* Consider rushing attempts

* Account for trades