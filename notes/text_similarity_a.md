# Investigation into computing text similarity.  
  
***
-
-

## Definitions

- Similarity: the degree to which humans perceive two documents to share a common subject, topic, and context.
- Accuracy: congruency between computational measurements of similarity with human perception. Higher congruency 
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
- Syntax and grammatical patterns.



## Background + Procedure

#### Data-set

The data-set will consist of thirty document pairs (sixty documents total) of varying similarity. Each document pair will be
given a score by human reader(s) between 0 and 100, with 100 representing identical documents and 0 representing entirely 
unrelated documents. Due to the subjectivity of human scoring, scores will be normalized according to the following pseudocode:
 
`ls` is the lowest score awarded by a individualX  
`hs` is the highest score awarded by individualX  
`score` is an arbitrary score awarded by individualX  

`range = hs - ls;
normalizedScore = ((score - ls) / range) * 100;` 


#### Part-of-speech tagging

A version of the Viterbi Algorithm will be used to tag parts of speech. This is required for algorithms that will consider 
nouns. Detailed description of the Viterbi Algorithm is not within scope. A quick summary:

Statistical, supervised machine-learning algorithm. Generally, an English word can be of different parts of speech; 'dog' can be
either a noun or verb, depending on context. By processing a large, pre-tagged corpus, the algorithm learns how likely each
word is to be of a certain part-of-speech. It also learns the likelyhood of parts-of-speech being proceeded by others. By 
traversing an unseen document sequentially, these probabilities can be combined to tag a text. 


#### Vector-based algorithms (VBAs)

These algorithms operate by converting the document into a vector derived from its individual terms. Similarity is determined by 
comparing vectors.

Typical evaluation of VBAs when given a document pair can be broken down into three stages:

1. Tally stage: For both documents, record and possibly normalize the frequency of each term.
2. Dimensionality stage: Transform the vectors to ensure they are of the same dimensionality.
3. Comparison stage: Compare the vectors to determine the level of similarity.

There are numerous methods of performing each step, each of which will produce different results. As such, a higher-order function
is used for combination of these 'sub-functions':

`-- (Haskell) type-signature of a function to coordinate composition of individual sub-functions to form a VBA <br />  
f :: String ->                                      -- document a  <br />
     String ->                                      -- document b  <br />
     (String -> Map String Float) ->                -- tallying function  <br />
     ([Map String Float] -> [Map String Float]) ->  -- dimensionality reduction function  <br />
     ([Map String Float] -> Float) ->               -- comparison function  <br />
     Float                                          -- evaluated, f is a Float, representing the similarity score`

This provides an environment in which individual components of the algorithm can be substituted for one another in a controlled fashion.
Each 'sub-function' can be assessed relative to its competitors.  

The following routines will be assessed:

Tallying

-- Simple word count 
