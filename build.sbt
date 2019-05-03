organization := "ru.albemuth.tentura"
name := "tentura"
version := "0.0.10"

homepage := Some(url("https://github.com/gnuzzz/tentura"))
scmInfo := Some(ScmInfo(url("https://github.com/gnuzzz/tentura"), "git@github.com:gnuzzz/tentura.git"))
developers := List(Developer("owner", "Vladimir Kornyshev", "gnuzzz@mail.ru", url("https://github.com/gnuzzz")))

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

publishMavenStyle := true
useGpg := false
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)
//resolvers += "Sonatype OSS Staging" at "https://oss.sonatype.org/content/repositories/staging"

scalaVersion := "2.12.4"

autoCompilerPlugins := true

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

val jcudaVersion = "0.8.0"
val jcusegsortVersion = "0.8.2"

libraryDependencies ++= {
  Seq(
    "org.jcuda"                   % "jcuda"                        % jcudaVersion,
    "org.jcuda"                   % "jcublas"                      % jcudaVersion,
    "org.jcuda"                   % "jcufft"                       % jcudaVersion,
    "org.jcuda"                   % "jcusparse"                    % jcudaVersion,
    "org.jcuda"                   % "jcusolver"                    % jcudaVersion,
    "org.jcuda"                   % "jcurand"                      % jcudaVersion,
    "org.jcuda"                   % "jnvgraph"                     % jcudaVersion,
    "org.jcuda"                   % "jcudnn"                       % jcudaVersion,
    "ru.albemuth.jcuda"           % "jcusegsort"                   % jcusegsortVersion,
    "org.slf4j"                   % "slf4j-log4j12"                % "1.7.7",
    //"com.typesafe.scala-logging"  %   "scala-logging_2.12.0-M4"  % "3.1.0",
    "org.scalatest"               %% "scalatest"                   % "3.0.1"   % "test",
    "com.storm-enroute"           %% "scalameter-core"             % "0.8.2",
    "com.storm-enroute"           %% "scalameter"                  % "0.8.2"
  )
}

fork in Test := true

fork in test := true
