Solution Approach   :
---------------------
 Create an in memory graph representation of the City Map from the string representation of the city map.Inorder to find
 if journey is possible traverse the graph between the specified nodes. The graph representation is created using
 Graph for Scala api, with the places as nodes and the edges with two labels either Foot or Both.
 Slightly altered the interface contract for methods in 'CityMapKata' trait in order to avoid side effects, the graph
 object returned from 'constructMap' is immutable and can be safely used.


 Improvements       :
 --------------------
 The approach uses basic features of Graph for Scala, even more sophisticated implementation is possible by creating custom
 edges with 'Foot' or 'Car' means as travel as a key attribute.

 RuntimeEnvironment :
 --------------------
 Java       1.6
 Scala      2.10.0
 Scalatest  1.9.2
 Graph Core 1.6.1
 Maven      3.0.4
 Scala IDE for Eclpise or IntelliJ

 Execution          :
 --------------------

 Execute Main method within Execute Object
