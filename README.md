# SPARCAS

SPARCAS is a traffic simulation library that can be used by writing scala scripts as a command line based traffic simulation software

#What is implemented

At the moment only equilibrium traffic assignment is implemented. More is to come.
SPARCAS is a higher level front-end to ["BCFW"](https://github.com/frohfroh/bcfw).

#Building

SPARCAS can be build by running 'sbt assembly' . It is tested on scala 2.11 running on OpenJDK.
After that youn can write your own scripts and run them.

#How to use

An example of building a simple network, a matrix, running an assignment and getting the results is   [ FourPaths ](src/main/scala/example/FourPaths.scala).
Even if you do not know scala, it still possible to use this library by mimicking this file.

#Getting support, further information and contributing

Just open an issue to get in touch
