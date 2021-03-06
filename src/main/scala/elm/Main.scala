package elm

import elm.semantics.ElmApp

object Main extends ElmApp {
  import elm.semantics.actors.Language._

  //(key zip mouse).map(println(_))

  mouse.foldp(0) {
    case (i,(j,k)) => i+j+k
  } += (println(_))
}
