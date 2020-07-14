import scala.language.postfixOps

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  scalaVersion := "2.12.10",
  organization := "org.ergoplatform",
  resolvers += Resolver.sonatypeRepo("public"),
  licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode")),
  homepage := Some(url("https://github.com/ergoplatform/ergo-scala-compiler")),
  description := "ErgoScala and ErgoScript to ErgoTree compiler",
  pomExtra :=
      <developers>
        <developer>
          <id>greenhat</id>
          <name>Denys Zadorozhnyi</name>
          <url>https://github.com/greenhat/</url>
        </developer>
      </developers>,
  publishMavenStyle := true,
  publishTo := sonatypePublishToBundle.value,
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/ergoplatform/ergo-scala-compiler"),
      "scm:git@github.com:ergoplatform/ergo-scala-compiler.git"
    )
  ),

)

// prefix version with "-SNAPSHOT" for builds without a git tag
dynverSonatypeSnapshots in ThisBuild := true
// use "-" instead of default "+"
dynverSeparator in ThisBuild := "-"

lazy val allConfigDependency = "compile->compile;test->test"

val sigmaStateVersion = "3.2.1"

lazy val dependencies = Seq(
  "org.scorexfoundation" %% "sigma-state" % sigmaStateVersion % allConfigDependency,
  "org.scalameta" %% "scalameta" % "4.0.0"
)

lazy val testingDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test
)

lazy val root = project
  .in(file("."))
  .withId("ergo-scala-compiler")
  .settings(commonSettings)
  .settings(publish / skip := true)
  .aggregate(compiler, contractsTest)

lazy val compiler = project
  .in(file("compiler"))
  .withId("compiler")
  .settings(commonSettings)
  .settings(moduleName := "ergo-scala-compiler")
  .settings(
    libraryDependencies ++= dependencies ++ testingDependencies
  )
  .settings(
    scalacOptions ++= Seq(
      "-Xlog-free-terms",
      //      "-Ymacro-debug-lite"
    )
  )

lazy val contractsTest = project
  .in(file("contracts-test"))
  .withId("contracts-test")
  .settings(commonSettings)
  .settings(moduleName := "verified-contracts-test")
  .dependsOn(compiler % allConfigDependency)
  .settings(
    libraryDependencies ++= dependencies ++ testingDependencies ++ Seq(
      "org.ergoplatform" %% "verified-contracts" % "0.0.0-5-ee53f015-SNAPSHOT",
    )
  )
  .settings(publish / skip := true)
  .settings(
    scalacOptions ++= Seq(
      "-Xlog-free-terms",
      //      "-Ymacro-debug-lite"
    )
  )

lazy val commonScalacOptions = List(
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-unused-import",
  "-Ywarn-unused:imports",
)


// PGP key for signing a release build published to sonatype
// signing is done by sbt-pgp plugin
// how to generate a key - https://central.sonatype.org/pages/working-with-pgp-signatures.html
// how to export a key and use it with Travis - https://docs.scala-lang.org/overviews/contributors/index.html#export-your-pgp-key-pair
// pgpPublicRing := file("ci/pubring.asc")
// pgpSecretRing := file("~/.gnupg/privage.key")
pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray)
usePgpKeyHex("E56DB59CB6F7C723F4F6A44F5A02421A8A54A977")

lazy val credentialFile = Path.userHome / ".sbt" / ".sigma-sonatype-credentials"
credentials ++= (for {
  file <- if (credentialFile.exists) Some(credentialFile) else None
} yield Credentials(file)).toSeq

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
