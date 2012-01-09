# Investigation into computing text similarity.  
  
***
-
-

## Definitions

- Similarity: the degree to which humans perceive two documents to share a common subject, topic, and context.
- Accuracy: congruency between computational measurements of simiarity with human perception. Higher congruency 
  corresponds to higher accuracy.


## Aim

To determine the most accurate and efficient computational measure of similarity between English text documents, according 
to the definitions provided above. 


## Hypothesis

Given that the following are shown to be correct:

- Different measurement algorithms will yield different scores when comparing the same documents.
- Each algorithm will approach a relatively constant level of accuracy when over a set of documents.
- Outputs of algorithms that share a common computational 'mechanism' (such as term-vectors) will cluster 
  relative to those that do not have common 'mechanisms'.  

It will follow that:

- Over many comparisons, one algorithm will have a smaller average difference between its outputs and (normalized) human
  opinion than the others.
- Over many comparisons, one class of algorithm will have a smaller average difference between its outputs and (normalized)
  human opinion than other classes.

Furthermore, it should follow that algorithmic consideration of certain textual features will prove more accurate. These 
features are likely to include:

- The subset of terms identified as nouns;
- Synonyms of these nouns.
- Syntax and gramatical patterns.



## Background + Procedure

The data-set will consist of thirty document pairs (sixty documents total) of varying similarity. Each document pair will be
given a score by human reader(s) between 0 and 100, with 100 representing indentical documents and 0 representing entirely 
unrelated documents. Due to the subjectivity of human scoring, scores will be normalized according to the following psuedocode:
 
`ls` is the lowest score awarded by a individualX  
`hs` is the highest score awarded by individualX  
`score` is an arbitrary score awarded by individualX  

`range = hs - ls;
normalizedScore = ((score - ls) / range) * 100;` 



In order to test multiple approaches to measuring text similarity, a higher-order function '#similarity' was devised.
This function   


