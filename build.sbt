val Http4sVersion = "0.23.23"
val CirceVersion = "0.14.5"
val MunitVersion = "0.7.29"
val MunitCatsEffectVersion = "1.0.7"
val Log4CatsVersion = "2.6.0"
val LogbackVersion = "1.4.11"
val ParadiseVersion = "2.1.0"
val Fs2Version = "3.8.0"
val Fs2DataJsonCirceVersion = "1.8.0"

lazy val root = (project in file("."))
  .settings(
    organization := "cn.edu.sustech",
    name := "court_reservation",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "3.3.0",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-client" % Http4sVersion,
      "org.http4s" %% "http4s-circe" % Http4sVersion,
      "org.http4s" %% "http4s-dsl" % Http4sVersion,
      "org.scalameta" %% "munit" % MunitVersion % Test,
      "org.typelevel" %% "munit-cats-effect-3" % MunitCatsEffectVersion % Test,
      "org.typelevel" %% "log4cats-slf4j" % Log4CatsVersion,
      "io.circe" %% "circe-core" % CirceVersion,
      "io.circe" %% "circe-generic" % CirceVersion,
      "io.circe" %% "circe-literal" % CirceVersion,
      "io.circe" %% "circe-parser" % CirceVersion,
      "co.fs2" %% "fs2-core" % Fs2Version,
      "co.fs2" %% "fs2-io" % Fs2Version,
      "org.gnieh" %% "fs2-data-json-circe" % Fs2DataJsonCirceVersion,
      "ch.qos.logback" % "logback-classic" % LogbackVersion
    ),
    assembly / assemblyMergeStrategy := {
      case "module-info.class" => MergeStrategy.discard
      case x => (assembly / assemblyMergeStrategy).value.apply(x)
    }
  )
