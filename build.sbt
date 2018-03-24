organization := "ru.albemuth.tentura"

name := "tentura"

version := "0.0.7"

scalaVersion := "2.12.4"

autoCompilerPlugins := true

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

val jcudaVersion = "0.8.0"

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
    "org.jcuda"                   % "jcudpp"                       % jcudaVersion,
//    "org.jcuda"                   % "jcudpp-natives"               % jcudaVersion classifier "windows-x86_64",
    "org.slf4j"                   % "slf4j-log4j12"                % "1.7.7",
    //"com.typesafe.scala-logging"  %   "scala-logging_2.12.0-M4"  % "3.1.0",
    "org.scalatest"               %% "scalatest"                   % "3.0.1"   % "test",
    "com.storm-enroute"           %% "scalameter-core"             % "0.8.2",
    "com.storm-enroute"           %% "scalameter"                  % "0.8.2"
  )
}

fork in Test := true

fork in test := true
