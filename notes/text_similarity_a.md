# Investigation into computing text similarity.  
  
***
-
-

## Definitions

- Similarity: the degree to which humans perceive two documents to share a common subject, topic, and context.
- Accuracy: congruency between computational measurements of similarity and human perception. Higher congruency 
  corresponds to higher accuracy.


## Aim

To determine the most accurate and efficient computational measure of similarity between English text documents, according 
to the definitions provided above. 


## Hypothesis

Given that the following are shown to be correct:

- Different measurement algorithms will yield different scores when comparing the same documents.
- Each algorithm will approach a relatively constant level of accuracy over a set of documents.
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
 
`ls` is the lowest score awarded by individualX  
`hs` is the highest score awarded by individualX  
`score` is an arbitrary score awarded by individualX  

```ruby
range = hs - ls
normalizedScore = ((score - ls) / range) * 100
``` 


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

1. Tally stage: For both documents, record and convert the frequency of each term into a significance score.
2. Dimensionality stage: Transform the vectors to ensure they have an equal number of dimensions.
3. Comparison stage: Compare the vectors to determine the level of similarity.

There are numerous methods of performing each step, each of which will produce different results. As such, a higher-order function
is used for combination of these 'sub-functions':

```haskell
    -- (Haskell) type-signature of a function to coordinate composition of individual sub-functions 
    -- to form a VBA. Such explicit typing may or may not be used in practice.
    f :: String ->                                      -- document a  
         String ->                                      -- document b  
         (String -> Map String Float) ->                -- tallying function  
         ([Map String Float] -> [Map String Float]) ->  -- dimensionality reduction function  
         ([Map String Float] -> Float) ->               -- comparison function  
         Float                                          -- evaluated, f is a Float, representing the similarity score
```

This provides an environment in which individual components of the algorithm can be substituted for one another in a controlled fashion.
Each 'sub-function' can be assessed relative to its competitors. 

Brief descriptions and links to the sub-functions implemented are listed below. 


**Tallying Functions**

- Total-Relative: significance = term frequency / total word count
- Inverse Total-Relative: significance = 1 - (term frequency / total word count)
- Count-Relative: significance = term frequency / highest term frequency
- Inverse Count-Relative: significance = 1 - (term frequency / highest term frequency)


**Dimension Equalization** 

- Intersection: Only scores corresponding to words present in both documents are considered. 
- Injection: Zeros are inserted into the vectors at the indicies corresponding to words exclusive to the other document.
 
**Comparison**

Rather than describe these again here I have provided links to their respective wikipedia articles, which are more thorough
than I could be.

- Dot product 
- Cosine similarity
- Mahalanobis distance
- Chebyhev distance

