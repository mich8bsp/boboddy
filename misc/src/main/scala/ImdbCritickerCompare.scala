import scala.collection.mutable
import scala.io.Source

case class MovieRating(
                      title: String,
                      rating: Int, //1 - 10
                      year: Int,
                      imdbId: String,
                      titleType: Option[String] = None
                      )

object ImdbCritickerCompare {

  def parseCsvLine(line: String): Array[String] = {
    val splitLine = line.split(",")
    if(splitLine.forall(x => !x.contains("\""))){
      splitLine
    }else{
      val bfr: mutable.Buffer[String] = mutable.Buffer[String]()
      val accum: mutable.Buffer[String] = mutable.Buffer[String]()
      splitLine.foreach(field => {
        if(field.startsWith("\"") && field.endsWith("\"")){
          bfr.append(field.filterNot(_ == '"'))
        }else if(field.contains("\"")){
            if(accum.nonEmpty){
              accum.append(field.filterNot(_ == '"'))
              bfr.append(accum.mkString(","))
              accum.clear()
            }else{
              accum.append(field.filterNot(_ == '"'))
            }
          }else{
            if(accum.nonEmpty){
              accum.append(field)
            }else{
              bfr.append(field)
            }
          }
      })

      bfr.toArray
    }
  }

  //Const,Your Rating,Date Rated,Title,URL,Title Type,IMDb Rating,Runtime (mins),Year,Genres,Num Votes,Release Date,Directors
  def readImdbRating(line: String): MovieRating = {
    val splitLine = parseCsvLine(line)

   try {
     MovieRating(
       title = splitLine(3),
       rating = splitLine(1).toInt,
       year = splitLine(8).toInt,
       imdbId = splitLine.head,
       titleType = Some(splitLine(5))
     )
   }finally {
//     println("bad")
   }
  }

  //Score, Film Name, Year, Date Rated, Mini Review, URL, IMDB ID
  def readCritickerRating(line: String): MovieRating = {
    val splitLine = parseCsvLine(line)

    try {
      MovieRating(
        title = splitLine(1),
        rating = splitLine.head.toInt match {
          case 0 | 25 => 1
          case 50 => 5
          case 75 | 85 | 100 => 10
        },
        year = splitLine(2).toInt,
        imdbId = splitLine(6)
      )
    }finally {
//      println("bad")
    }
  }


  def main(args: Array[String]): Unit = {
    val imdbRatings: Seq[MovieRating] = Source.fromResource("imdb_ratings.csv").getLines().toList
      .tail
      .map(readImdbRating)

//    imdbRatings.foreach(println)

    val critickerRatings: Seq[MovieRating] = Source.fromResource("criticker_ratings.csv").getLines().toList
      .tail
      .map(readCritickerRating)

//    critickerRatings.foreach(println)

    val imdbRatingsById: Map[String, MovieRating] = imdbRatings.map(x => x.imdbId -> x).toMap
    val critickerRatingsById: Map[String, MovieRating] = critickerRatings.map(x => x.imdbId -> x).toMap

    println("Missing in IMDB")
    (imdbRatingsById.keySet ++ critickerRatingsById.keySet).foreach(id => (imdbRatingsById.get(id), critickerRatingsById.get(id)) match {
      case (None, Some(x)) => println(s"Rating missing in IMDB: $x")
      case _ =>
    })

    println("Missing in Criticker")
    (imdbRatingsById.keySet ++ critickerRatingsById.keySet).foreach(id => (imdbRatingsById.get(id), critickerRatingsById.get(id)) match {
      case (Some(x), None) => println(s"Rating missing in criticker: ${x}")
      case _ =>
    })

    println("Mismatch in ratings")
    (imdbRatingsById.keySet ++ critickerRatingsById.keySet).foreach(id => (imdbRatingsById.get(id), critickerRatingsById.get(id)) match {
      case (Some(x), Some(y)) => if(x.rating != y.rating){
        println(s"Ratings don't match: IMDB - ${x}, Criticker - $y")
      }
      case _ =>
    })

  }
}
