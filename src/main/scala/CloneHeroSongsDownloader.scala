import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.stream.scaladsl.{Keep, Sink}

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URLEncoder
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

object CloneHeroSongsDownloader {

  def fetchSong(song: Song)
               (implicit ec: ExecutionContext,
                actorSystem: ActorSystem): Option[String] = {
    def fetchSongJson(url: String): Future[String] = {
      Http().singleRequest(HttpRequest(uri = Uri(url)))
        .flatMap(_.entity.dataBytes
          .toMat(Sink.seq)(Keep.right)
          .run())
        .map(_.reduce(_ ++ _).toArray)
        .map(x => new String(x))
    }

    for {
      title <- song.titles
      artist <- song.artists
    } yield {
      val url = s"https://chorus.fightthe.pw/api/search?query=" +
        s"name%3D%22${URLEncoder.encode(title, "UTF-8")}%22%20" +
        s"artist%3D%22${URLEncoder.encode(artist, "UTF-8")}%22%20" +
        s"diff_drums%3D1"

      val json = Await.result(fetchSongJson(url), 10.seconds)

      Thread.sleep(10)
      if (json != "{\"songs\":[],\"roles\":{}}") {
        println(json)
        return Some(json)
      }
    }

    None
  }

  def fetchSongs(
                inputFilePath: String,
                outputFilePath: String
                )(implicit ec: ExecutionContext,
                 actorSystem: ActorSystem): Unit = {
    val songs = parseSongs(inputFilePath)
    val outLines = songs.flatMap(song => {
      fetchSong(song)
    })

    val file = new File(outputFilePath)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- outLines) {
      bw.write(line)
    }
    bw.close()
  }

  def parseSongs(filePath: String): List[Song] = {
    def parseField(str: String): List[String] = {
      Set(
        str.trim,
        str.split("[;(-]").head.trim
      ).toList
    }

    Source.fromResource(filePath).getLines()
      .map(line => {
        Song(
          titles = parseField(line.split("\\|")(1).trim),
          artists = line.split("\\|").head.split(";")
            .map(_.trim)
            .flatMap(parseField)
            .toList
        )
      }).toList
  }

  def main(args: Array[String]): Unit = {
    implicit val actorSystem = ActorSystem("as")
    implicit val ec = actorSystem.dispatcher

    fetchSongs(
      inputFilePath = "spotlistr-exported-playlist.csv",
      outputFilePath = "clone-hero-songs.txt"
    )
    println("done")
  }
}

case class Song(
                 titles: List[String],
                 artists: List[String]
               )