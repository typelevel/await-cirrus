//> using platform "js"
//> using scala "3.2.2"
//> using jsVersion "1.13.1"
//> using dep "org.typelevel::toolkit::0.0.7"
//> using jsModuleKind "common"

import cats.data.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.ember.client.*
import io.circe.*

import scala.concurrent.duration.*

object index extends IOApp:
  def getInput(input: String) =
    Env[IO].get(s"INPUT_${input.toUpperCase}")

  def getApiUrl =
    Env[IO].get("GITHUB_API_URL").map(_.get)

  def getRepo =
    Env[IO].get("GITHUB_REPOSITORY").map(_.get)

  def getSha =
    Env[IO].get("GITHUB_SHA").map(_.get)

  def getPollInterval =
    OptionT(getInput("poll-interval"))
      .subflatMap(_.toIntOption)
      .map(_.seconds)
      .getOrElse(30.seconds)

  def run(args: List[String]) =
    EmberClientBuilder.default[IO].build.use { client =>
      (getPollInterval, getApiUrl, getRepo, getSha).flatMapN {
        (pollInterval, apiUrl, repo, sha) =>

          val getConclusion =
            OptionT
              .liftF(
                client.expect[CheckSuites](
                  s"$apiUrl/repos/$repo/commits/$sha/check-suites"
                )
              )
              .subflatMap(_.check_suites.find(_.app.slug == "cirrus-ci"))
              .filter(_.status == "completed")
              .subflatMap(_.conclusion)
              .map(_ == "success")
              .value

          def go: IO[ExitCode] = getConclusion.flatMap {
            case Some(conclusion) =>
              if conclusion then
                IO.println("Cirrus CI succeeded").as(ExitCode.Success)
              else IO.println("Cirrus CI errored").as(ExitCode.Error)
            case None =>
              IO.println(
                s"Cirrus CI not yet complete, polling again in $pollInterval ..."
              ) *> IO.sleep(pollInterval) *> go
          }

          go
      }
    }

case class CheckSuites(check_suites: List[CheckSuite]) derives Decoder
object CheckSuites:
  given EntityDecoder[IO, CheckSuites] = jsonOf[IO, CheckSuites]

case class CheckSuite(
    app: App,
    status: String,
    conclusion: Option[String]
) derives Decoder

case class App(slug: String) derives Decoder
