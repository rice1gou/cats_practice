import cats.data.EitherT
import cats.instances.future._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
val powerLevels = Map(
  "Jazz"      -> 6,
  "Bumblebee" -> 8,
  "Hot Rod"   -> 10
)

object AutoBot {
  def getPowerLevel(ally: String): Response[Int] = {
    powerLevels.get(ally) match {
      case Some(avg) => EitherT.right(Future(avg))
      case None => EitherT.left(Future(s"$ally unreachable"))
    }
  }
  type Response[A] = EitherT[Future, String, A]

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    powerLevel1 <- getPowerLevel(ally1)
    powerLevel2 <- getPowerLevel(ally2)
  } yield (powerLevel1 + powerLevel2) > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value
    Await.result(stack, 1.second) match {
      case Left(msg) => s"Comms error: $msg"
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    }

  }
}
