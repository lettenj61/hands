import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    organization := "com.github.lettenj61",
    scalaVersion := "2.12.7",
    version := "0.1.0-SNAPSHOT",
    scalacOptions in Compile ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      // "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-unused-import",
      "-Ywarn-unused",
      "-P:scalajs:sjsDefinedByDefault"
    ),
    name := "hands",
    description := "Pseudo event listeners and selector API for dead browsers (I mean IE)",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.6",
      "org.scalatest" %%% "scalatest" % "3.0.5" % "test"
    ),
    scalaJSLinkerConfig ~= { linkerConfig =>
      linkerConfig.withSourceMap(false)
    },
    jsEnv in Test := new JSDOMNodeJSEnv
  )
