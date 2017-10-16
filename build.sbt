import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import org.scalajs.sbtplugin.cross.CrossProject

// Convenient setting that allows writing `set scalaVersion := dotty.value` in sbt shell to switch from Scala to Dotty
val dotty = settingKey[String]("dotty version")
dotty in ThisBuild := "0.4.0-RC1"

val commonSettings = Seq(
  organization := "ch.epfl.scala",
  version := "0.6.0-SNAPSHOT",
  scalaVersion := "2.12.3",
  crossScalaVersions := scalaVersion.value :: "2.13.0-M2" :: dotty.value :: Nil,
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds"/*, "-opt:l:classpath"*/),
  scalacOptions ++= {
    if (!isDotty.value)
      Seq("-opt-warnings") // This option does not exist in Dotty
    else
      Seq()
  },
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
  fork in Test := true,
  parallelExecution in Test := false,
  homepage := Some(url("https://github.com/scala/collection-strawman")),
  licenses := Seq("BSD 3-clause" -> url("http://opensource.org/licenses/BSD-3-Clause")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/scala/collection-strawman"),
      "scm:git:git@github.com:scala/collection-strawman.git"
    )
  ),
  pomExtra :=
    <developers>
      <developer><id>ichoran</id><name>Rex Kerr</name></developer>
      <developer><id>odersky</id><name>Martin Odersky</name></developer>
      <developer><id>julienrf</id><name>Julien Richard-Foy</name></developer>
      <developer><id>szeiger</id><name>Stefan Zeiger</name></developer>
    </developers>,
  // For publishing snapshots
  credentials ++= (
    for {
      username <- sys.env.get("SONATYPE_USERNAME")
      password <- sys.env.get("SONATYPE_PASSWORD")
    } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
  ).toList
)

val disablePublishing = Seq(
  publishArtifact := false,
  // The above is enough for Maven repos but it doesn't prevent publishing of ivy.xml files
  publish := (),
  publishLocal := ()
)

// Disable cross-compilation with Dotty.
val disableDotty = Seq(
  crossScalaVersions ~= (_.filterNot(_.startsWith("0.")))
)

def crossProj(id: String, base: File) =
  CrossProject(id, base, CrossType.Pure)
    .settings(commonSettings)
    .jsSettings(
      disableDotty,
      fork in Test := false
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "com.novocode" % "junit-interface" % "0.11" % Test
      )
    )
    .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))

val collections =
  crossProj("collections", file("collections"))
    .settings(
      name := "collection-strawman",
      scalacOptions += "-Yno-imports"
    )

val collectionsJVM = collections.jvm
val collectionsJS = collections.js

val `collections-contrib` =
  crossProj("collections-contrib", file("collections-contrib"))
    .dependsOn(collections)
    .settings(
      name := "collections-contrib"
    )

val `collections-contrib-jvm` = `collections-contrib`.jvm
val `collections-contrib-js` = `collections-contrib`.js

val `collection-strawman` = project.in(file("."))
  .settings(commonSettings ++ disablePublishing)
  .settings(
    // Some short-cuts for common tasks
    test in Test := (test in (collectionsJVM, Test)).value,
    compile in Compile := (compile in (collectionsJVM, Compile)).value,
    compile in Test := (compile in (collectionsJVM, Test)).value,
    Seq(publish, publishLocal, publishSigned).map { publishKey =>
      publishKey := {
        val a = (publishKey in collectionsJVM).value
        val b = (publishKey in collectionsJS).value
        val c = (publishKey in `collections-contrib-jvm`).value
        val d = (publishKey in `collections-contrib-js`).value
      }
    }
  )

val junit = project.in(file("test") / "junit")
  .dependsOn(collectionsJVM)
  .settings(commonSettings ++ disablePublishing)
   // Dotty 0.3.0-RC1 crashes when trying to compile this project
  .settings(disableDotty)
  .settings(
    fork in Test := true,
    javaOptions in Test += "-Xss1M",
    libraryDependencies ++= Seq(
      "junit"            % "junit"           % "4.11",
      "com.novocode"     % "junit-interface" % "0.11"   % Test,
      "org.openjdk.jol"  % "jol-core"        % "0.5"
    ),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")
  )

val scalacheck = project.in(file("test") / "scalacheck")
  .dependsOn(collectionsJVM)
  .settings(commonSettings)
  .settings(disablePublishing)
   // Dotty 0.3.0-RC1 crashes when trying to compile this project
  .settings(disableDotty)
  .settings(
    fork in Test := true,
    javaOptions in Test += "-Xss1M",
    libraryDependencies ++= Seq(
      ("org.scalacheck" %% "scalacheck" % "1.13.5" % Test).withDottyCompat()
    )
  )

val timeBenchmark =
  project.in(file("benchmarks/time"))
    .dependsOn(collectionsJVM)
    .enablePlugins(JmhPlugin)
    .settings(commonSettings ++ disablePublishing)
     // Dotty 0.3.0-RC1 crashes when trying to compile this project
    .settings(disableDotty)
    .settings(
      charts := Def.inputTaskDyn {
        val benchmarks = Def.spaceDelimited().parsed
        val targetDir = crossTarget.value
        val jmhReport = targetDir / "jmh-result.json"
        val runTask = run in Jmh
        Def.inputTask {
          val _ = runTask.evaluated
          strawman.collection.Bencharts(jmhReport, "Execution time (lower is better)", targetDir)
          targetDir
        }.toTask(s" -rf json -rff ${jmhReport.absolutePath} ${benchmarks.mkString(" ")}")
      }.evaluated
    )

val memoryBenchmark =
  project.in(file("benchmarks/memory"))
    .dependsOn(collectionsJVM)
    .settings(commonSettings ++ disablePublishing)
    .settings(
      libraryDependencies += ("org.spire-math" %% "jawn-ast" % "0.10.4").withDottyCompat(),
      charts := Def.inputTaskDyn {
        val targetDir = crossTarget.value
        val report = targetDir / "report.json"
        val runTask = run in Compile
        Def.inputTask {
          val _ = runTask.evaluated
          strawman.collection.Bencharts(report, "Memory footprint (lower is better)", targetDir)
          targetDir
        }.toTask(s" ${report.absolutePath}")
      }.evaluated
    )

lazy val charts = inputKey[File]("Runs the benchmarks and produce charts")
