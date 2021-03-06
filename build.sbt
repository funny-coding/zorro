import Dependencies._

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "xyz.funnycoding"
ThisBuild / organizationName := "funnycoding"

lazy val root = (project in file("."))
  .settings(
    name := "zorro"
  )
  .aggregate(core, tests)

lazy val tests = (project in file("modules/tests"))
  .configs(IntegrationTest)
  .settings(
    name := "zorro-test-suite",
    scalacOptions += "-Ymacro-annotations",
    scalafmtOnCompile := true,
    Defaults.itSettings,
    libraryDependencies ++= Seq(
          compilerPlugin(Libraries.kindProjector cross CrossVersion.full),
          compilerPlugin(Libraries.betterMonadicFor),
          Libraries.scalaCheck,
          Libraries.scalaTest,
          Libraries.scalaTestPlus
        )
  )
  .dependsOn(core)

lazy val core = (project in file("modules/core"))
  .enablePlugins(DockerPlugin)
  .enablePlugins(AshScriptPlugin)
  .settings(
    name := "zorro-core",
    packageName in Docker := "zorro",
    scalacOptions += "-Ymacro-annotations",
    scalafmtOnCompile := true,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    Defaults.itSettings,
    dockerBaseImage := "openjdk:8u201-jre-alpine3.9",
    dockerExposedPorts ++= Seq(8080),
    makeBatScripts := Seq(),
    dockerUpdateLatest := true,
    libraryDependencies ++= Seq(
          compilerPlugin(Libraries.kindProjector cross CrossVersion.full),
          compilerPlugin(Libraries.betterMonadicFor),
          Libraries.cats,
          Libraries.catsEffect,
          Libraries.catsMeowMtl,
          Libraries.catsRetry,
          Libraries.circeCore,
          Libraries.circeGeneric,
          Libraries.circeParser,
          Libraries.circeRefined,
          Libraries.cirisCore,
          Libraries.cirisEnum,
          Libraries.cirisRefined,
          Libraries.http4sDsl,
          Libraries.http4sServer,
          Libraries.http4sClient,
          Libraries.http4sCirce,
          Libraries.log4cats,
          Libraries.logback,
          Libraries.newtype,
          Libraries.refinedCore,
          Libraries.refinedCats
        )
  )
