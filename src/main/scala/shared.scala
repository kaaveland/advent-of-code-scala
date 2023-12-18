package com.kaaveland.aoc

import AdventOfCodeFs.{load, save}
import AdventOfCodeHttp.{downloadDay, downloadYear}

import cats.effect.{IO, Resource}
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.`User-Agent`
import org.http4s.{Header, Headers, ProductId}
import org.typelevel.ci.CIStringSyntax

import java.io.{FileInputStream, FileNotFoundException, FileOutputStream}
import java.nio.file.{Path, Paths}
import scala.collection.immutable

object AdventOfCodeInputs {
  import cats.implicits.*
  def storeYear(year: Int): IO[Unit] = downloadYear(year).flatMap { data =>
    data.traverse { case (day, content) =>
      save(year, day, content)
    }
  }.void
  private def storeDay(year: Int, day: Int): IO[String] = downloadDay(year, day).flatMap { data =>
    save(year, day, data)
  }
  def dataFor(year: Int, day: Int): IO[String] = load(year, day).recoverWith {
    case e: FileNotFoundException => storeDay(year, day)
  }
}

object AdventOfCodeFs {
  private val dataDir = IO(Path.of("input")).flatMap(p => IO(p.toFile.mkdir()).map(_ => p))

  private def coordinates(year: Int, day: Int): IO[Path] =
    dataDir.map(p => p.resolve(Paths.get(year.toString, "day_%02d".format(day), "input")))

  def save(year: Int, day: Int, data: String): IO[String] = for {
    p <- coordinates(year, day)
    _ <- IO(p.getParent.toFile.mkdirs())
    fp = Resource.make {
      IO(new FileOutputStream(p.toFile))
    } { stream => IO(stream.close()).handleErrorWith(_ => IO.unit) }
    _ <- fp.use { fp => IO(fp.write(data.getBytes())) }
  } yield data

  def load(year: Int, day: Int): IO[String] = for {
    p <- coordinates(year, day)
    fp = Resource.make {
      IO(new FileInputStream(p.toFile))
    } { stream => IO(stream.close()).handleErrorWith(_ => IO.unit) }
    data <- fp.use { fp => IO(new String(fp.readAllBytes())) }
  } yield data

}

object AdventOfCodeHttp {
  import cats.implicits.*

  private val client = for {
    c <- obtainCookie
    ua <- obtainUserAgent
  } yield EmberClientBuilder
    .default[IO]
    .withUserAgent(`User-Agent`(ProductId(ua)))
    .build
    .map(client => {
      Client[IO] { req =>
        client.run(req.withHeaders(Headers(Header.Raw(ci"Cookie", s"session=$c"))))
      }
    })

  private def uagentFile: IO[String] = for {
    h <- home
  } yield s"$h/.aoc_uagent"

  private def home: IO[String] = IO(System.getProperty("user.home"))

  private def obtainUserAgentFromStdin: IO[String] = for {
    _ <- IO.println("Please provide your email for the user agent header for advent of code")
    v <- IO.readLine
  } yield v.trim

  private def obtainCookie: IO[String] = fromFileWithFallback(cookieFile, obtainCookieFromStdin)
  private def obtainUserAgent: IO[String] =
    fromFileWithFallback(uagentFile, obtainUserAgentFromStdin)
  private def cookieFile: IO[String] = for {
    h <- home
  } yield s"$h/.aoc_cookie"
  private def obtainCookieFromStdin: IO[String] = for {
    _ <- IO.println("Please provide your advent of code session cookie from developer tools")
    v <- IO.readLine
  } yield v.trim

  private def fromFileWithFallback(path: IO[String], fallback: IO[String]): IO[String] = {
    val fromFile: IO[String] = for {
      p <- path
      fp = Resource.make {
        IO(new FileInputStream(p))
      } { stream => IO(stream.close()).handleErrorWith(_ => IO.unit) }
      content <- fp.use { fp => IO(new String(fp.readAllBytes())).map(_.trim) }
    } yield content
    
    fromFile.recoverWith { case e: FileNotFoundException =>
      for {
        f <- fallback
        p <- path
        fp = Resource.make {
          IO(new FileOutputStream(p))
        } { stream => IO(stream.close()).handleErrorWith(_ => IO.unit) }
        _ <- fp.use { fp => IO(fp.write(f.getBytes())) }
      } yield f
    }
  }

  def downloadYear(year: Int): IO[List[(Int, String)]] = client.flatMap { resource =>
    resource.use { client =>
      Range(1, 25).toList.traverse { day =>
        fetchDay(client, year, day).map(data => (day, data))
      }
    }
  }

  def downloadDay(year: Int, day: Int): IO[String] = client.flatMap { resource =>
    resource.use { client =>
      fetchDay(client, year, day)
    }
  }
  private def fetchDay(client: Client[IO], year: Int, day: Int): IO[String] =
    client.expect[String](s"https://adventofcode.com/$year/day/$day/input")
}
