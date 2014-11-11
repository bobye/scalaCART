Classification and Regression Tree (CART)
====
Jianbo Ye

This is only a sample implementation of (relatively standard) CART in Scala, which includes
 - Splitting Rules (using axis aligned classifier)
 - Class Assignment
 - Pruning Trees (based on resubstitution estimates)

See more descriptions [here](https://github.com/bobye/scalaCART/wiki). To run
```bash
$ sbt test:run
$ dot -Tpng graph.dot -o graph.png
```

Reference
 - PSU [STAT557](http://sites.stat.psu.edu/~jiali/course/stat557/material.html)
 - https://www.salford-systems.com/resources/whitepapers/116-an-overview-of-the-cart-methodology

