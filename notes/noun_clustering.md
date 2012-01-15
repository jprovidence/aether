# Investigation into clustering of nouns.

***

## Musings

#### *A priori* reasoning and speculation

Nouns that appear in the same context are relevant to one another. The ideal context for determining relevance may be 

- same sentence
- same paragraph, or within some number of words of one another
- same document
- some combination of the above

The more frequently nouns appear in the same context, the stronger their relationship.

A data structure that catalogs all nouns in a document set, their relationships to others, and the strengths of these relationships will either
be:

- an undirected graph.
- a set of undirected graphs.

where:

- a noun is a vertex
- a relationship is an edge, with a strength attribute

The state of the data contained in this graph/graph-set can be used to cluster the nouns.

A good manner in which to cluster given this information would involve:

- randomly and evenly distributing verticies across an n-dimensional space
- assuming a 'force of attraction' between nouns that share an edge. Strength attributes  strength of this force.
- adjust the distances between verticies according to these forces until some sort of equilibrium is achieved. 

However, given that many nouns are likely to have a high number of relations, this state of homeostatis will likely provide a rather 'coarse'
view of noun-groupings (or topics). Somewhat related groups are likely to merge entirely, the entire graph may even converge on a single point. 
Therefore, the introduction of forces of replusion between unrelated nouns is necessary. 

If repulsive forces are applied universally towards all unrelated nouns in addition to the existing attraction, it is likely that homeostasis 
will never be attained. A distance threshold that determines whether or not repulsive forces should be applied may fix this issue. 

Therefore, the first iteration of a noun clustering algorithm should:

1. Randomly and evenly distribute all nouns in an n-dimensional space
2. Iteratively apply attractive forces between related verticies
3. Iteratively apply repulsive forces between unrelated verticies that are nearer than some preset.
4. Repeat step 2 & 3 until either  
   
   - vertex movement ceases  
   - becomes repetitive (i.e the same vertex moving back and forth by the x units) 
   - is negligible


A periodically updated visualization would benefit assessment of the algorithm. 

#### Implementation

###### Obtaining and storing data

Before anything else, nouns must be extracted from documents, their relationship strengths determined, and an undirected graph constructed to 
represent this. 

Identification of nouns is a simple task. The Viterbi Algorithm can be used to tag nouns with considerable accuracy. Next, relationships must 
be determined and ranked. A scoring routine must be defined. As a starting point, the following process will be used:

- All nouns in the same document are related.
- Those in the same document will have their relationship strength increased by one. 
- Those within 100 words of one another will have their relationship increased again by one.
- Those within 10 words will have their relationship increased yet again by one.

*It must be noted that scores will be skewed by the 'topic distribution' of the dataset. It is unknown whether this is desired or not, and must 
be monitored.*

The scoring routine above can be implemented by: 

- Tagging all nouns with the viterbi algorithm
- Extracting all nouns, recording their index within the document
- Applying a relationship increase of one between each noun
- Iterating the noun-set from start to finish, applying relationship increases to each noun with an index advanced fewer than 100 positions.
- Repeating the above iteration, with a 10 position advance. 

   

###### Analysing the graph

From a high-level perspective, an implementation of the above consists of three main paritions:

1. Visualization: Display positions of each vertex
2. Clustering calculations: Iterate over the dataset, adjusting distances and recording deltas
3. Homeostatis monitoring: Determine whether movement has stabilized

These sections will need to communicate as such:

Clustering    -- output -->    Visualization  
Clustering    -- output -->    Homeostasis  
Homeostais    -- output -->    Clustering  

The clustering partition, therefore, will be central. All communication will be dependant upon the state of clustering computations. Data 
will only need to be transferred when the clustering routine completes an iteration of the dataset. 

Given this separation and low level of communication, it would be easy to run each of these modules on a Haskell green thread. That way, when
compilied with the `-threaded` option, the program can take advantage of parallelism on multiple cores with concurrency as a fallback. A 
two-way channel is necessary for Clustering/Homeostasis communication. Clustering/Visualization communication will be one-way.

To abstract away communication details, the following datatypes will be useful:

- Two-Way Channel: a bundling of two `MVars`
- Cluster/Homeostatis type: a type to encapsulte all message varietie and their data content
- Cluster/Visualization type: similar to Cluster/Homeostasis, but for visualization data of course
 
Both the Visualization and Homeostatis threads will block on their input MVars until Clustering provides them with data. Visualization will
simply render this data. Homeostatis will need to respond after analysing its input. During this analysis time, Clustering could either:

- Block until a response is received
- Perform another iteration, read response upon completion

If it were to block, an unnecessary final iteration would be avoided. If it were to simply forge on, one final unnecessary iteration will be
performed. Most responses will be instructions to continue, however. It follows that performing an extra iteration will likely be less wasteful
than waiting for each analysis to complete. 





