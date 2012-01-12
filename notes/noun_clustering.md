# Investigation into clustering of nouns.

***

## Musings

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
- assuming a 'force of attraction' between nouns that share an edge. Strength attributes give the strength of this force.
- adjust the distances between verticies according to these forces until some sort of equilibrium is achieved. 

However, given that many nouns are likely to have a high number of relations, this state of homeostatis will likely provide a rather 'coarse'
view of noun-groupings (or topics). Somewhat related groups are likely to merge entirely, the entire graph may even converge on a single point. 
Therefore, the introduction of forces of repulusion between unrelated nouns is necessary. 

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

 
