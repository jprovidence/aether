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


#### Vector-based algorithms (VBAs)

**Construction**

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

Haskell implementations can be found [here](https://github.com/jprovidence/aether/blob/master/src/Text/Sim.hs), Brief descriptions of 
the sub-functions implemented are listed below.

###### Tallying Functions

- Total-Relative: 
  `significance = term frequency / total word count`
- Inverse Total-Relative: 
  `significance = 1 / (term frequency / total word count)`
- Count-Relative: 
  `significance = term frequency / highest term frequency`
- Inverse Count-Relative: 
  `significance = 1 / (term frequency / highest term frequency)`


###### Dimension Equalization

- Intersection: Only scores corresponding to words present in both documents are considered. 
- Injection: Zeros are inserted into the vectors at the indicies corresponding to words exclusive to the other document.
 
###### Comparison

Vectors are typically compared by determining the angle or distance between them. Three functions have been selected for evaluation
that use one of these approaches. Rather than describe them again here, I have provided links to their respective wikipedia articles, 
which are more thorough than I could be.

- [Euclidean distance](http://en.wikipedia.org/wiki/Euclidean_distance) 
- [Cosine similarity](http://en.wikipedia.org/wiki/Cosine_similarity)
- [Chebyhev distance](http://en.wikipedia.org/wiki/Chebyshev_distance)


**Document Preparation**

The easyiest manner in which to apply these functions is to a document string is to split the it at whitespace, tabulations, etc.
The result is an array of terms which can easily be tallied and processed. The issue of stopwords (a, the, it...) can vastly skew
results, as these words are numerous in completely unrelated documents. They must be filtered out. Additionally, the problem of edge-case 
terms (those that contain punctuation and strange capitalization) can be remedied by downcasing and filtering for punctuation.   


**Consideration of Nouns**

At this point, most 'low hanging' points of the hypothesis can be tested. Each sub-function combiniation can be applied to the prepared
documents and scoring should, to some degree, be a ranking by similarity. Relative sub-function accuracies can be determined and general 
trends discovered. However, none of the final points can be assessed. Namely, the algorithm does not consider:

- The subset of terms identified as nouns;
- Synonyms of these nouns.
- Syntax and grammatical patterns.

The program as it stands so far should serve as a reference point, against which more extensive functions can be compared. Such a 
comparison will answer whether, say, considering the set of nouns actually contributes positively to the algorithm's accuracy. 

The most basic way to introduce nouns is to take a naive approach. Using the Viterbi Algorithm [Appendix A]  to eliminate non-nouns from
the document input will cause the algorithm to consider nouns exclusively with no further modification. Alternatively, nouns can merely 
be identified in the input group and awarded additional significance with minor modification of tallying and/or comparison sub-functions. 
For ease of implementation in this case, the scores of a noun-blind and noun-exclusive run of an otherwise identical function can be 
hybridized to acheive the same effect. Of course there will be performance implications, but these can be addressed if the approach proves
most fruitful.

A more robust approach would involve knowledge of noun relations. A corpus of documents to learn from [Appendix B] is therefore necessary, 
as is an [algorithm to group/cluster these nouns](https://github.com/jprovidence/aether/blob/master/notes/noun_clustering.md). 



## Appendix A, Viterbi Algorithm

A version of the Viterbi Algorithm will be used to tag parts of speech. This is required for algorithms that will consider 
nouns. Detailed description of the Viterbi Algorithm is not within scope. A quick summary:

Statistical, supervised machine-learning algorithm. Generally, an English word can be of different parts of speech; 'dog' can be
either a noun or verb, depending on context. By processing a large, pre-tagged corpus, the algorithm learns how likely each
word is to be of a certain part-of-speech. It also learns the likelyhood of parts-of-speech being proceeded by others. By 
traversing an unseen document sequentially, these probabilities can be combined to tag a text. 


## Appendix B, Document Corpus

Blog articles provide a good base for learning noun similarities. They are relatively short and about singluar topics. Therefore, nouns
they contain will be generally relavant to one another. Furthermore, nouns which frequently appear together across many articles are 
even more likely to denote a certain topic. These nouns can be grouped into 'topic-clusters'. Knowledge of these clusters may aid an 
algorithm in determining the topic of a document and, by extension, more accurately gauge the similarity of two documents. More information 
is available [here](https://github.com/jprovidence/aether/blob/master/notes/document_corpus.md).
