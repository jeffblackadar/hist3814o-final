# hist3814o-final
Files for the final project of HIST3814o.  Analysis of the Shawville Equity

## Problem with equity_mallet_topic_modeller.R

In respository see program:

equity_mallet_topic_modeller.R

When it executes: km <- kmeans(topic_df_dist, n.topics)  I get   Error in sample.int(m, k) :
  cannot take a sample larger than the population when 'replace = FALSE   I have not been able to figure this out.  I see a few other people have this error on the internet and suspect it's how the "sweep" does the sample.  This is a productive fail for me, but I wanted to see if it's fixable.
