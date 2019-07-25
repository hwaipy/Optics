name := "Optics"
version := "0.1"
scalaVersion := "2.12.8"
organization := "com.hwaipy"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalanlp" %% "breeze" % "1.0-RC2"
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "1.0-RC2"

//libraryDependencies += "com.thoughtworks.compute" %% "cpu" % "latest.release"
//libraryDependencies += "com.thoughtworks.compute" %% "gpu" % "latest.release"
//// LWJGL OpenCL library
//libraryDependencies += "org.lwjgl" % "lwjgl-opencl" % "latest.release"
//// Platform dependent runtime of LWJGL core library
//libraryDependencies += ("org.lwjgl" % "lwjgl" % "latest.release").jar().classifier {
//  import scala.util.Properties._
//  if (isMac) {
//    "natives-macos"
//  } else if (isLinux) {
//    "natives-linux"
//  } else if (isWin) {
//    "natives-windows"
//  } else {
//    throw new MessageOnlyException(s"lwjgl does not support $osName")
//  }
//}
libraryDependencies += "org.platanios" %% "tensorflow" % "0.4.0" classifier "darwin-cpu-x86_64"