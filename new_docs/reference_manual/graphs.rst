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