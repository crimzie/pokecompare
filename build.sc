import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`
import mill._
import mill.define.Target
import mill.scalalib._

object pokecompare extends ScalaModule {

  override def scalaVersion: Target[String] = "0.27.0-RC1"

  override def scalacOptions: Target[Seq[String]] =
    super.scalacOptions() ++ Seq(
      "-Yexplicit-nulls",
      "-Ycheck-init",
    )

}

