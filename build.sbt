

lazy val bcfw = RootProject(uri("https://github.com/frohfroh/bcfw.git"))

lazy val root =  (project in file(".")).settings(
	name := "SPARCAS" ).dependsOn(bcfw)
