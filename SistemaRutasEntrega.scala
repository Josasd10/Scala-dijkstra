import scala.collection.mutable

object SistemaRutasEntrega {


  val grafo: Map[String, List[(String, Int)]] = Map(
    "Tienda" -> List(("Bodega Norte", 5), ("Bodega Sur", 8)),
    "Bodega Norte" -> List(("Cliente A", 4), ("Cliente B", 7)),
    "Bodega Sur" -> List(("Cliente B", 2), ("Cliente C", 6)),
    "Cliente A" -> List(("Cliente Final", 3)),
    "Cliente B" -> List(("Cliente Final", 1)),
    "Cliente C" -> List(("Cliente Final", 5)),
    "Cliente Final" -> List()
  )

  def main(args: Array[String]): Unit = {

    println("===== SISTEMA DE RUTAS DE ENTREGA =====")

    println("\nUbicaciones disponibles:")
    grafo.keys.foreach(println)

    print("\nIngrese punto de inicio: ")
    val inicio = scala.io.StdIn.readLine()

    print("Ingrese destino: ")
    val destino = scala.io.StdIn.readLine()

    if (!grafo.contains(inicio) || !grafo.contains(destino)) {
      println("Error: ubicación no válida")
      return
    }

    val (distancias, anteriores) = dijkstra(grafo, inicio)

    if (distancias(destino) == Int.MaxValue) {
      println("No existe una ruta disponible")
    } else {

      val ruta = reconstruirRuta(anteriores, inicio, destino)

      println("\n===== RESULTADO =====")
      println(s"Ruta más corta: ${ruta.mkString(" -> ")}")
      println(s"Distancia total: ${distancias(destino)} km")
    }
  }

  def dijkstra(
      grafo: Map[String, List[(String, Int)]],
      inicio: String
  ): (Map[String, Int], Map[String, String]) = {

    // Distancias iniciales
    val distancias = mutable.Map[String, Int]()
    val anteriores = mutable.Map[String, String]()

    for (nodo <- grafo.keys) {
      distancias(nodo) = Int.MaxValue
    }

    distancias(inicio) = 0

  
    implicit val orden: Ordering[(String, Int)] = Ordering.by[(String, Int), Int](_._2).reverse
    val cola = mutable.PriorityQueue.empty[(String, Int)]

    cola.enqueue((inicio, 0))

    while (cola.nonEmpty) {

      val (nodoActual, distanciaActual) = cola.dequeue()

      // Revisar vecinos
      for ((vecino, peso) <- grafo(nodoActual)) {

        val nuevaDistancia = distanciaActual + peso

        if (nuevaDistancia < distancias(vecino)) {

          distancias(vecino) = nuevaDistancia
          anteriores(vecino) = nodoActual

          cola.enqueue((vecino, nuevaDistancia))
        }
      }
    }

    (distancias.toMap, anteriores.toMap)
  }

  def reconstruirRuta(
      anteriores: Map[String, String],
      inicio: String,
      destino: String
  ): List[String] = {

    var ruta = List[String]()
    var actual = destino

    ruta = actual :: ruta

    while (anteriores.contains(actual)) {
      actual = anteriores(actual)
      ruta = actual :: ruta
    }

    ruta
  }
}