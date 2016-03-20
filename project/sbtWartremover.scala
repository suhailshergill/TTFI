object sbtWartremover {
  import sbt._
  import sbt.Keys._

  import wartremover._

  def settings: Seq[Setting[_]] = Seq(
    // https://github.com/puffnfresh/wartremover
    // disable Wart.Throw till
    // <https://github.com/puffnfresh/wartremover/issues/182> lands in the
    // upcoming release
    wartremoverErrors ++= Warts.unsafe filterNot (_ == Wart.Throw)
  )

}
