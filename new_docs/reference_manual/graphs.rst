======
Graphs
======

.. function:: Graph(edges)
              Graph(vertices, edges)

  construct a graph

  .. seealso:: :func:`->`, :func:`<->`


.. function:: infix -> (vertex1, vertex2)
              infix <-> (vertex1, vertex2)

  construct an edge

.. function:: Vertices(g)

  return list of graph vertices

  .. seealso:: :func:`Edges`, :func:`Graph`

.. function:: Edges(g)

  return list of graph edges

  .. seealso:: :func:`Vertices`, :func:`Graph`

.. function:: AdjacencyMatrix(g)

  adjacency matrix

  :param g: graph

  Return `adjacency matrix <https://en.wikipedia.org/wiki/Adjacency_matrix>`_ 
  of graph ``g``.

  .. seealso:: :func:`Graph`

.. function:: DFS(g, f)
              DFS(g, v, f)

  traverse graph in depth-first order

  .. param g: graph
  .. param v: vertex
  .. param f: functor

  Traverse graph ``g`` in `depth-first 
  <https://en.wikipedia.org/wiki/Depth-first_search>`_ order, starting from
  ``v`` if provided, or from the first vertex. ``f`` is called for every
  visited vertex.

  .. seealso:: :func:`Graph`, :func:`Vertices`
