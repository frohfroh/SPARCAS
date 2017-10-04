

lazy val bcfw = RootProject(uri("https://github.com/frohfroh/bcfw.git"))



lazy val root =  (project in file(".")).settings(
	name := "SPARCAS",
	version := "0.1.0-SNAPSHOT",
	scalaVersion := "2.12.1" ).dependsOn(bcfw)
