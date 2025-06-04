addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "[0.14.3,)")

// Builds docker image of our exchange svr
addSbtPlugin("com.github.sbt"        % "sbt-native-packager"     % "[1.11.1,)")

// Linter
//  addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat"           % "[1.1.0,)")

// A fast restart of our rest api svr in sbt. Does NOT require use of spray
addSbtPlugin("io.spray"                % "sbt-revolver"            % "[0.10.0,)")

// Code coverage report generation
addSbtPlugin("org.scoverage"           % "sbt-scoverage"           % "[2.3.1,)")

ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
